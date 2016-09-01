{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      :  Language.Thrift.Parser
-- Copyright   :  (c) Abhinav Gupta 2016
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Provides a parser for Thrift IDLs.
--
-- In addition to parsing the IDLs, the parser also keeps track of
-- Javadoc-style docstrings on defined items and makes their values available.
-- For example,
--
-- > /**
-- >  * Fetches an item.
-- >  */
-- > Item getItem()
--
-- Note that the parser does not validate the Thrift file for correctness, so,
-- for example, you could define a string value for an int constant.
--
module Language.Thrift.Parser
    ( parseFromFile
    , parse
    , thriftIDL

    -- * Components

    , program

    , header
    , include
    , namespace

    , definition
    , constant
    , typeDefinition
    , service

    , typedef
    , enum
    , struct
    , union
    , exception
    , senum

    , typeReference
    , constantValue

    , docstring

    -- * Parser

    , Parser
    , runParser
    , whiteSpace
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State (StateT)
import Data.Text                 (Text)

import qualified Control.Monad.Trans.State as State
import qualified Data.Text                 as Text
import qualified Text.Megaparsec           as P
import qualified Text.Megaparsec.Lexer     as PL

import Language.Thrift.Internal.Reserved (isReserved)

import qualified Language.Thrift.Types as T

-- | Keeps track of the last docstring seen by the system so that we can
-- attach it to entities.
data State = State
    { stateDocstring :: T.Docstring
    }
    deriving (Show, Eq)

-- | Underlying Parser type.
type Parser s = StateT State (P.Parsec s)

-- | Evaluates the underlying parser with a default state and get the Megaparsec
-- parser.
runParser :: Parser s a -> P.Parsec s a
runParser p = State.evalStateT p (State Nothing)

-- | Parses the Thrift file at the given path.
parseFromFile :: FilePath -> IO (Either P.ParseError (T.Program P.SourcePos))
parseFromFile =
    P.parseFromFile (thriftIDL :: P.Parsec Text (T.Program P.SourcePos))

-- | @parse name contents@ parses the contents of a Thrift document with name
-- @name@ held in @contents@.
parse
    :: P.Stream s Char
    => FilePath -> s -> Either P.ParseError (T.Program P.SourcePos)
parse = P.parse thriftIDL

-- | Megaparsec parser that is able to parse full Thrift documents.
thriftIDL :: P.Stream s Char => P.Parsec s (T.Program P.SourcePos)
thriftIDL = runParser program


clearDocstring :: Parser s ()
clearDocstring = State.modify' (\s -> s { stateDocstring = Nothing })


-- | Returns the last docstring recorded by the parser and forgets about it.
lastDocstring :: Parser s T.Docstring
lastDocstring = do
    s <- State.gets stateDocstring
    clearDocstring
    return s

-- | Optional whitespace.
whiteSpace :: P.Stream s Char => Parser s ()
whiteSpace = someSpace <|> pure ()

-- | Required whitespace.
someSpace :: P.Stream s Char => Parser s ()
someSpace = P.skipSome $ readDocstring <|> skipComments <|> skipSpace
  where
    readDocstring = do
        s <- docstring
        unless (Text.null s) $
            State.modify' (\st -> st { stateDocstring = Just s})

    skipSpace = P.choice
      [ P.newline *> clearDocstring
      , P.skipSome P.spaceChar
      ]

    skipComments = P.choice
        [ P.char '#'            *> skipLine
        , P.try (P.string "//") *> skipLine
        , P.try (P.string "/*") *> skipCStyleComment
        ] *> clearDocstring

    skipLine = void P.eol <|> P.eof <|> (P.anyChar *> skipLine)

    skipCStyleComment = P.choice
      [ P.try (P.string "*/")      *> pure ()
      , P.skipSome (P.noneOf "/*") *> skipCStyleComment
      , P.oneOf "/*"               *> skipCStyleComment
      ]

-- | @p `skipUpTo` n@ skips @p@ @n@ times or until @p@ stops matching --
-- whichever comes first.
skipUpTo :: P.Stream s Char => Parser s a -> Int -> Parser s ()
skipUpTo p = loop
  where
    loop 0 = return ()
    loop n =
        ( do
            void $ P.try p
            loop $! n - 1
        ) <|> return ()

hspace :: P.Stream s Char => Parser s ()
hspace = void $ P.oneOf " \t"

-- | A javadoc-style docstring.
--
-- > /**
-- >  * foo
-- >  */
--
-- This parses attempts to preserve indentation inside the docstring while
-- getting rid of the aligned @*@s (if any) and any other preceding space.
--
docstring :: P.Stream s Char => Parser s Text
docstring = do
    P.try (P.string "/**") >> P.skipMany hspace
    indent <- P.sourceColumn <$> P.getPosition
    isNewLine <- maybeEOL
    chunks <- loop isNewLine (indent - 1) []
    return $! Text.intercalate "\n" chunks
  where
    maybeEOL = (P.eol >> return True) <|> return False

    commentChar =
        P.noneOf "*\r\n" <|>
        P.try (P.char '*' <* P.notFollowedBy (P.char '/'))

    loop shouldDedent maxDedent chunks = do
        when shouldDedent $
            hspace `skipUpTo` maxDedent
        finishComment <|> readDocLine
      where
        finishComment = do
            P.try (P.skipMany hspace <* P.string "*/")
            void $ optional P.spaceChar
            return $! reverse chunks
        readDocLine = do
            -- Lines could have aligned *s at the start.
            --
            --      /**
            --       * foo
            --       * bar
            --       */
            --
            -- But only if we dedented. If we didn't, that's possibly because,
            --
            --      /** foo [..]
            --
            -- So if foo starts with "*", we don't want to drop that.
            when shouldDedent . void $
                optional $ P.try (P.char '*' >> optional hspace)

            line <- Text.pack <$> P.many commentChar

            -- This line most likely ends with a newline but if it's the last
            -- one, it could also be "foo */"
            void (optional hspace >> maybeEOL)

            loop True maxDedent (line:chunks)


symbolic :: P.Stream s Char => Char -> Parser s ()
symbolic c = void $ PL.symbol whiteSpace [c]

token :: P.Stream s Char => Parser s a -> Parser s a
token = PL.lexeme whiteSpace

braces, angles, parens :: P.Stream s Char => Parser s a -> Parser s a

braces = P.between (symbolic '{') (symbolic '}')
angles = P.between (symbolic '<') (symbolic '>')
parens = P.between (symbolic '(') (symbolic ')')

comma, semi, colon, equals :: P.Stream s Char => Parser s ()

comma  = symbolic ','
semi   = symbolic ';'
colon  = symbolic ':'
equals = symbolic '='

-- | errorUnlessReserved ensures that the given identifier is in the
-- reservedKeywords list. If it's not, we have a bug and we should crash.
errorUnlessReserved :: Monad m => String -> m ()
errorUnlessReserved name =
    unless (isReserved name) $
        error ("reserved called with unreserved identifier " ++ show name)

-- | Parses a reserved identifier and adds it to the collection of known
-- reserved keywords.
reserved :: P.Stream s Char => String -> Parser s ()
reserved name =
    errorUnlessReserved name >>
    P.label name $ token $ P.try $ do
        void (P.string name)
        P.notFollowedBy (P.alphaNumChar <|> P.oneOf "_.")

-- | A string literal. @"hello"@
literal :: P.Stream s Char => Parser s Text
literal = P.label "string literal" $ token $
    stringLiteral '"' <|> stringLiteral '\''

stringLiteral :: P.Stream s Char => Char -> Parser s Text
stringLiteral q = fmap Text.pack $
    P.char q >> P.manyTill PL.charLiteral (P.char q)


integer :: P.Stream s Char => Parser s Integer
integer = token PL.integer


-- | An identifier in a Thrift file.
identifier :: P.Stream s Char => Parser s Text
identifier = P.label "identifier" $ token $ do
    name <- (:)
        <$> (P.letterChar <|> P.char '_')
        <*> many (P.alphaNumChar <|> P.oneOf "_.")
    when (isReserved name) $
        P.unexpected name
    return (Text.pack name)


-- | Top-level parser to parse complete Thrift documents.
program :: P.Stream s Char => Parser s (T.Program P.SourcePos)
program = whiteSpace >>
    T.Program
        <$> many (header     <* optionalSep)
        <*> many (definition <* optionalSep)
        <*  P.eof

-- | Headers defined for the IDL.
header :: P.Stream s Char => Parser s (T.Header P.SourcePos)
header = P.choice
  [ T.HeaderInclude   <$> include
  , T.HeaderNamespace <$> namespace
  ]


-- | The IDL includes another Thrift file.
--
-- > include "common.thrift"
-- >
-- > typedef common.Foo Bar
--
include :: P.Stream s Char => Parser s (T.Include P.SourcePos)
include = reserved "include" >> withPosition (T.Include <$> literal)


-- | Namespace directives allows control of the namespace or package
-- name used by the generated code for certain languages.
--
-- > namespace py my_service.generated
namespace :: P.Stream s Char => Parser s (T.Namespace P.SourcePos)
namespace = P.choice
  [ reserved "namespace" >>
    withPosition (T.Namespace <$> (star <|> identifier) <*> identifier)
  , reserved "cpp_namespace" >>
    withPosition (T.Namespace "cpp" <$> identifier)
  , reserved "php_namespace" >>
    withPosition (T.Namespace "php" <$> identifier)
  , reserved "py_module" >>
    withPosition (T.Namespace "py" <$> identifier)
  , reserved "perl_package" >>
    withPosition (T.Namespace "perl" <$> identifier)
  , reserved "ruby_namespace" >>
    withPosition (T.Namespace "rb" <$> identifier)
  , reserved "java_package" >>
    withPosition (T.Namespace "java" <$> identifier)
  , reserved "cocoa_package" >>
    withPosition (T.Namespace "cocoa" <$> identifier)
  , reserved "csharp_namespace" >>
    withPosition (T.Namespace "csharp" <$> identifier)
  ]
  where
    star = symbolic '*' >> pure "*"


-- | Convenience wrapper for parsers expecting a position.
--
-- The position will be retrieved BEFORE the parser itself is executed.
withPosition :: P.Stream s Char => Parser s (P.SourcePos -> a) -> Parser s a
withPosition p = P.getPosition >>= \pos -> p <*> pure pos


-- | Convenience wrapper for parsers that expect a docstring and a position.
--
-- > data Foo = Foo { bar :: Bar, doc :: Docstring, pos :: Delta }
-- >
-- > parseFoo = withDocstring $ Foo <$> parseBar
withDocstring
    :: P.Stream s Char
    => Parser s (T.Docstring -> P.SourcePos -> a) -> Parser s a
withDocstring p = lastDocstring >>= \s -> do
    pos <- P.getPosition
    p <*> pure s <*> pure pos


-- | A constant, type, or service definition.
definition :: P.Stream s Char => Parser s (T.Definition P.SourcePos)
definition = whiteSpace >> P.choice
    [ T.ConstDefinition   <$> constant
    , T.TypeDefinition    <$> typeDefinition
    , T.ServiceDefinition <$> service
    ]


-- | A type definition.
typeDefinition :: P.Stream s Char => Parser s (T.Type P.SourcePos)
typeDefinition = P.choice
    [ T.TypedefType   <$> typedef
    , T.EnumType      <$> enum
    , T.SenumType     <$> senum
    , T.StructType    <$> struct
    , T.UnionType     <$> union
    , T.ExceptionType <$> exception
    ]


-- | A typedef is just an alias for another type.
--
-- > typedef common.Foo Bar
typedef :: P.Stream s Char => Parser s (T.Typedef P.SourcePos)
typedef = reserved "typedef" >> withDocstring
    (T.Typedef <$> typeReference <*> identifier <*> typeAnnotations)


-- | Enums are sets of named integer values.
--
-- > enum Role {
-- >     User = 1, Admin
-- > }
enum :: P.Stream s Char => Parser s (T.Enum P.SourcePos)
enum = reserved "enum" >> withDocstring
    ( T.Enum
        <$> identifier
        <*> braces (many enumDef)
        <*> typeAnnotations
    )


-- | A @struct@.
--
-- > struct User {
-- >     1: string name
-- >     2: Role role = Role.User;
-- > }
struct :: P.Stream s Char => Parser s (T.Struct P.SourcePos)
struct = reserved "struct" >> withDocstring
    ( T.Struct
        <$> identifier
        <*> braces (many field)
        <*> typeAnnotations
    )


-- | A @union@ of types.
--
-- > union Value {
-- >     1: string stringValue;
-- >     2: i32 intValue;
-- > }
union :: P.Stream s Char => Parser s (T.Union P.SourcePos)
union = reserved "union" >> withDocstring
    ( T.Union
        <$> identifier
        <*> braces (many field)
        <*> typeAnnotations
    )


-- | An @exception@ that can be raised by service methods.
--
-- > exception UserDoesNotExist {
-- >     1: optional string message
-- >     2: required string username
-- > }
exception :: P.Stream s Char => Parser s (T.Exception P.SourcePos)
exception = reserved "exception" >> withDocstring
    ( T.Exception
        <$> identifier
        <*> braces (many field)
        <*> typeAnnotations
    )


-- | Whether a field is @required@ or @optional@.
fieldRequiredness :: P.Stream s Char => Parser s T.FieldRequiredness
fieldRequiredness = P.choice
  [ reserved "required" *> pure T.Required
  , reserved "optional" *> pure T.Optional
  ]


-- | A struct field.
field :: P.Stream s Char => Parser s (T.Field P.SourcePos)
field = withDocstring $
  T.Field
    <$> optional (integer <* colon)
    <*> optional fieldRequiredness
    <*> typeReference
    <*> identifier
    <*> optional (equals *> constantValue)
    <*> typeAnnotations
    <*  optionalSep


-- | A value defined inside an @enum@.
enumDef :: P.Stream s Char => Parser s (T.EnumDef P.SourcePos)
enumDef = withDocstring $
  T.EnumDef
    <$> identifier
    <*> optional (equals *> PL.signed whiteSpace integer)
    <*> typeAnnotations
    <*  optionalSep


-- | An string-only enum. These are a deprecated feature of Thrift and shouldn't
-- be used.
senum :: P.Stream s Char => Parser s (T.Senum P.SourcePos)
senum = reserved "senum" >> withDocstring
    ( T.Senum
        <$> identifier
        <*> braces (many (literal <* optionalSep))
        <*> typeAnnotations
    )


-- | A 'const' definition.
--
-- > const i32 code = 1;
constant :: P.Stream s Char => Parser s (T.Const P.SourcePos)
constant = do
  reserved "const"
  withDocstring $
    T.Const
        <$> typeReference
        <*> (identifier <* equals)
        <*> constantValue
        <*  optionalSep


-- | A constant value literal.
constantValue :: P.Stream s Char => Parser s (T.ConstValue P.SourcePos)
constantValue = withPosition $ P.choice
  [ P.try (P.string "0x") >> T.ConstInt <$> token PL.hexadecimal
  , either T.ConstInt T.ConstFloat
                      <$> token (PL.signed whiteSpace PL.number)
  , T.ConstLiteral    <$> literal
  , T.ConstIdentifier <$> identifier
  , T.ConstList       <$> constList
  , T.ConstMap        <$> constMap
  ]


constList :: P.Stream s Char => Parser s [T.ConstValue P.SourcePos]
constList = symbolic '[' *> loop []
  where
    loop xs = P.choice
      [ symbolic ']' *> return (reverse xs)
      , (:) <$> (constantValue <* optionalSep)
            <*> pure xs
            >>= loop
      ]


constMap
    :: P.Stream s Char
    => Parser s [(T.ConstValue P.SourcePos, T.ConstValue P.SourcePos)]
constMap = symbolic '{' *> loop []
  where
    loop xs = P.choice [
        symbolic '}' *> return (reverse xs)
      , (:) <$> (constantValuePair <* optionalSep)
            <*> pure xs
            >>= loop
      ]


constantValuePair
    :: P.Stream s Char
    => Parser s (T.ConstValue P.SourcePos, T.ConstValue P.SourcePos)
constantValuePair =
    (,) <$> (constantValue <* colon)
        <*> (constantValue <* optionalSep)


-- | A reference to a built-in or defined field.
typeReference :: P.Stream s Char => Parser s (T.TypeReference P.SourcePos)
typeReference = P.choice
  [ baseType
  , containerType
  , withPosition (T.DefinedType <$> identifier)
  ]


baseType
    :: P.Stream s Char => Parser s (T.TypeReference P.SourcePos)
baseType = withPosition $
    P.choice [reserved s *> (v <$> typeAnnotations) | (s, v) <- bases]
  where
    bases =
      [ ("string", T.StringType)
      , ("binary", T.BinaryType)
      , ("slist", T.SListType)
      , ("bool", T.BoolType)
      , ("byte", T.ByteType)
      , ("i8", T.ByteType)
      , ("i16", T.I16Type)
      , ("i32", T.I32Type)
      , ("i64", T.I64Type)
      , ("double", T.DoubleType)
      ]


containerType
    :: P.Stream s Char => Parser s (T.TypeReference P.SourcePos)
containerType = withPosition $
    P.choice [mapType, setType, listType] <*> typeAnnotations
  where
    mapType = reserved "map" >>
        angles (T.MapType <$> (typeReference <* comma) <*> typeReference)
    setType = reserved "set" >> angles (T.SetType <$> typeReference)
    listType = reserved "list" >> angles (T.ListType <$> typeReference)


-- | A service.
--
-- > service MyService {
-- >     // ...
-- > }
service :: P.Stream s Char => Parser s (T.Service P.SourcePos)
service = do
  reserved "service"
  withDocstring $
    T.Service
        <$> identifier
        <*> optional (reserved "extends" *> identifier)
        <*> braces (many function)
        <*> typeAnnotations


-- | A function defined inside a service.
--
-- > Foo getFoo() throws (1: FooDoesNotExist doesNotExist);
-- > oneway void putBar(1: Bar bar);
function :: P.Stream s Char => Parser s (T.Function P.SourcePos)
function = withDocstring $
    T.Function
        <$> ((reserved "oneway" *> pure True) <|> pure False)
        <*> ((reserved "void" *> pure Nothing) <|> Just <$> typeReference)
        <*> identifier
        <*> parens (many field)
        <*> optional (reserved "throws" *> parens (many field))
        <*> typeAnnotations
        <*  optionalSep


-- | Type annotations on entitites.
--
-- > (foo = "bar", baz = "qux")
--
-- These do not usually affect code generation but allow for custom logic if
-- writing your own code generator.
typeAnnotations :: P.Stream s Char => Parser s [T.TypeAnnotation]
typeAnnotations = parens (many typeAnnotation) <|> pure []


typeAnnotation :: P.Stream s Char => Parser s T.TypeAnnotation
typeAnnotation =
    T.TypeAnnotation
        <$> identifier
        <*> (optional (equals *> literal) <* optionalSep)


optionalSep :: P.Stream s Char => Parser s ()
optionalSep = void $ optional (comma <|> semi)
