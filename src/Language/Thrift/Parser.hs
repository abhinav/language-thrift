{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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

    , functionParameters
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
import Data.Functor              (($>))
import Data.Proxy                (Proxy (Proxy))
import Data.Scientific           (floatingOrInteger)
import Data.Text                 (Text)
import Data.Void                 (Void)

import qualified Control.Monad.Trans.State  as State
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as PC
import qualified Text.Megaparsec.Char.Lexer as PL

import Language.Thrift.Internal.Reserved (isReserved)

import qualified Language.Thrift.AST as T

-- | Keeps track of the last docstring seen by the system so that we can
-- attach it to entities.
newtype State = State
    { stateDocstring :: T.Docstring
    }
    deriving (Show, Eq)

-- | Underlying Parser type.
type Parser s = StateT State (P.Parsec Void s)

-- | Evaluates the underlying parser with a default state and get the Megaparsec
-- parser.
runParser :: P.Stream s => Parser s a -> P.Parsec Void s a
runParser p = State.evalStateT p (State Nothing)

-- | Parses the Thrift file at the given path.
parseFromFile
    :: FilePath
    -> IO (Either (P.ParseErrorBundle Text Void) (T.Program P.SourcePos))
parseFromFile path = P.runParser thriftIDL path <$> Text.readFile path

-- | @parse name contents@ parses the contents of a Thrift document with name
-- @name@ held in @contents@.
parse
    :: (P.TraversableStream s, P.Token s ~ Char)
    => FilePath
    -> s -> Either (P.ParseErrorBundle s Void) (T.Program P.SourcePos)
parse = P.parse thriftIDL

-- | Megaparsec parser that is able to parse full Thrift documents.
thriftIDL
    :: (P.TraversableStream s, P.Token s ~ Char)
    => P.Parsec Void s (T.Program P.SourcePos)
thriftIDL = runParser program


clearDocstring :: P.Stream s => Parser s ()
clearDocstring = State.modify' (\s -> s { stateDocstring = Nothing })


-- | Returns the last docstring recorded by the parser and forgets about it.
lastDocstring :: P.Stream s => Parser s T.Docstring
lastDocstring = do
    s <- State.gets stateDocstring
    clearDocstring
    return s

-- | Optional whitespace.
whiteSpace :: (P.TraversableStream s, P.Token s ~ Char) => Parser s ()
whiteSpace = someSpace <|> pure ()

-- | Required whitespace.
someSpace :: (P.TraversableStream s, P.Token s ~ Char) => Parser s ()
someSpace = P.skipSome $ readDocstring <|> skipComments <|> skipSpace
  where
    readDocstring = do
        s <- docstring
        unless (Text.null s) $
            State.modify' (\st -> st { stateDocstring = Just s})

    skipSpace = P.choice
      [ PC.newline *> clearDocstring
      , P.skipSome PC.spaceChar
      ]

    skipComments = P.choice
        [ PC.char '#'            *> skipLine
        , P.try (string "//") *> skipLine
        , P.try (string "/*") *> skipCStyleComment
        ] *> clearDocstring

    skipLine = void PC.eol <|> P.eof <|> (P.anySingle *> skipLine)

    skipCStyleComment = P.choice
      [ P.try (string "*/")   $> ()
      , P.skipSome (noneOf "/*") *> skipCStyleComment
      , oneOf "/*"               *> skipCStyleComment
      ]

oneOf :: (P.Stream s, P.Token s ~ Char) => String -> Parser s Char
oneOf = P.oneOf
{-# INLINE oneOf #-}

noneOf :: (P.Stream s, P.Token s ~ Char) => String -> Parser s Char
noneOf = P.noneOf
{-# INLINE noneOf #-}

-- | @p `skipUpTo` n@ skips @p@ @n@ times or until @p@ stops matching --
-- whichever comes first.
skipUpTo :: P.Stream s => Parser s a -> Int -> Parser s ()
skipUpTo p = loop
  where
    loop 0 = return ()
    loop n =
        ( do
            void $ P.try p
            loop $! n - 1
        ) <|> return ()

hspace :: (P.Stream s, P.Token s ~ Char) => Parser s ()
hspace = void $ oneOf " \t"

-- | A javadoc-style docstring.
--
-- > /**
-- >  * foo
-- >  */
--
-- This parses attempts to preserve indentation inside the docstring while
-- getting rid of the aligned @*@s (if any) and any other preceding space.
--
docstring :: (P.TraversableStream s, P.Token s ~ Char) => Parser s Text
docstring = do
    P.try (string "/**") >> P.skipMany hspace
    indent <- fromIntegral . P.unPos <$> PL.indentLevel
    isNewLine <- maybeEOL
    chunks <- loop isNewLine (indent - 1) []
    return $! Text.intercalate "\n" (Text.dropWhileEnd (== ' ') <$> chunks)
  where
    maybeEOL = (PC.eol >> return True) <|> return False

    commentChar =
        noneOf "*\r\n" <|>
        P.try (PC.char '*' <* P.notFollowedBy (PC.char '/'))

    loop shouldDedent maxDedent chunks = do
        when shouldDedent $
            hspace `skipUpTo` maxDedent
        finishComment <|> readDocLine
      where
        finishComment = do
            P.try (P.skipMany hspace <* string "*/")
            void $ optional PC.spaceChar
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
                optional $ P.try (PC.char '*' >> optional hspace)

            line <- Text.pack <$> P.many commentChar

            -- This line most likely ends with a newline but if it's the last
            -- one, it could also be "foo */"
            void (optional hspace >> maybeEOL)

            loop True maxDedent (line:chunks)


symbolic :: forall s. (P.TraversableStream s, P.Token s ~ Char) => Char -> Parser s ()
symbolic c = void $ PL.symbol whiteSpace (P.tokenToChunk (Proxy :: Proxy s) c)

token :: (P.TraversableStream s, P.Token s ~ Char) => Parser s a -> Parser s a
token = PL.lexeme whiteSpace

braces, angles, parens
    :: (P.TraversableStream s, P.Token s ~ Char) => Parser s a -> Parser s a

braces = P.between (symbolic '{') (symbolic '}')
angles = P.between (symbolic '<') (symbolic '>')
parens = P.between (symbolic '(') (symbolic ')')

comma, semi, colon, equals :: (P.TraversableStream s, P.Token s ~ Char) => Parser s ()

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
reserved :: (P.TraversableStream s, P.Token s ~ Char) => String -> Parser s ()
reserved name =
    errorUnlessReserved name >>
    P.label name $ token $ P.try $ do
        void (string name)
        P.notFollowedBy (PC.alphaNumChar <|> oneOf "_.")


-- | A string literal. @"hello"@
literal :: (P.TraversableStream s, P.Token s ~ Char) => Parser s Text
literal = P.label "string literal" $ token $
    stringLiteral '"' <|> stringLiteral '\''

stringLiteral :: (P.Stream s, P.Token s ~ Char) => Char -> Parser s Text
stringLiteral q = fmap Text.pack $
    PC.char q >> P.manyTill PL.charLiteral (PC.char q)


integer :: (P.TraversableStream s, P.Token s ~ Char) => Parser s Integer
integer = token PL.decimal


-- | An identifier in a Thrift file.
identifier :: (P.TraversableStream s, P.Token s ~ Char) => Parser s Text
identifier = P.label "identifier" $ token $ do
    name <- (:)
        <$> (PC.letterChar <|> PC.char '_')
        <*> many (PC.alphaNumChar <|> oneOf "_.")
    when (isReserved name) $
        P.unexpected (P.Label (NonEmpty.fromList name))
    return (Text.pack name)


-- | Top-level parser to parse complete Thrift documents.
program :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Program P.SourcePos)
program = whiteSpace >>
    T.Program
        <$> many (header     <* optionalSep)
        <*> many (definition <* optionalSep)
        <*  P.eof

-- | Headers defined for the IDL.
header :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Header P.SourcePos)
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
include :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Include P.SourcePos)
include = reserved "include" >> withPosition (T.Include <$> literal)


-- | Namespace directives allows control of the namespace or package
-- name used by the generated code for certain languages.
--
-- > namespace py my_service.generated
namespace
    :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Namespace P.SourcePos)
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
withPosition :: P.TraversableStream s => Parser s (P.SourcePos -> a) -> Parser s a
withPosition p = P.getSourcePos >>= \pos -> p <*> pure pos


-- | Convenience wrapper for parsers that expect a docstring and a position.
--
-- > data Foo = Foo { bar :: Bar, doc :: Docstring, pos :: Delta }
-- >
-- > parseFoo = withDocstring $ Foo <$> parseBar
withDocstring :: P.TraversableStream s => Parser s (T.Docstring -> P.SourcePos -> a) -> Parser s a
withDocstring p = lastDocstring >>= \s -> do
    pos <- P.getSourcePos
    p <*> pure s <*> pure pos


-- | A constant, type, or service definition.
definition
    :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Definition P.SourcePos)
definition = whiteSpace >> P.choice
    [ T.ConstDefinition   <$> constant
    , T.TypeDefinition    <$> typeDefinition
    , T.ServiceDefinition <$> service
    ]


-- | A type definition.
typeDefinition
    :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Type P.SourcePos)
typeDefinition = P.choice
    [ T.TypedefType <$> typedef
    , T.EnumType    <$> enum
    , T.SenumType   <$> senum
    , T.StructType  <$> struct
    ]


-- | A typedef is just an alias for another type.
--
-- > typedef common.Foo Bar
typedef :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Typedef P.SourcePos)
typedef = reserved "typedef" >> withDocstring
    (T.Typedef <$> typeReference <*> identifier <*> typeAnnotations)


-- | Enums are sets of named integer values.
--
-- > enum Role {
-- >     User = 1, Admin
-- >
enum :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Enum P.SourcePos)
enum = reserved "enum" >> withDocstring
    ( T.Enum
        <$> identifier
        <*> braces (many enumDef)
        <*> typeAnnotations
    )


-- | A @struct@, @union@, or @exception@.
--
-- > struct User {
-- >     1: string name
-- >     2: Role role = Role.User;
-- > }
--
-- > union Value {
-- >     1: string stringValue;
-- >     2: i32 intValue;
-- > }
--
-- > exception UserDoesNotExist {
-- >     1: optional string message
-- >     2: required string username
-- > }
struct :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Struct P.SourcePos)
struct = kind >>= \k -> withDocstring
    ( T.Struct k
        <$> identifier
        <*> braces (many field)
        <*> typeAnnotations
    )
  where
    kind = P.choice
        [ reserved "struct"    >> return T.StructKind
        , reserved "union"     >> return T.UnionKind
        , reserved "exception" >> return T.ExceptionKind
        ]


-- | A @union@ of types.
union :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Struct P.SourcePos)
union = struct
{-# DEPRECATED union "Use struct." #-}

-- | An @exception@ that can be raised by service methods.
exception :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Struct P.SourcePos)
exception = struct
{-# DEPRECATED exception"Use struct." #-}


-- | Whether a field is @required@ or @optional@.
fieldRequiredness
    :: (P.TraversableStream s, P.Token s ~ Char) => Parser s T.FieldRequiredness
fieldRequiredness = P.choice
  [ reserved "required" $> T.Required
  , reserved "optional" $> T.Optional
  ]


-- | A struct field.
field :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Field P.SourcePos)
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
enumDef :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.EnumDef P.SourcePos)
enumDef = withDocstring $
  T.EnumDef
    <$> identifier
    <*> optional (equals *> PL.signed whiteSpace integer)
    <*> typeAnnotations
    <*  optionalSep


-- | An string-only enum. These are a deprecated feature of Thrift and shouldn't
-- be used.
senum :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Senum P.SourcePos)
senum = reserved "senum" >> withDocstring
    ( T.Senum
        <$> identifier
        <*> braces (many (literal <* optionalSep))
        <*> typeAnnotations
    )


-- | A 'const' definition.
--
-- > const i32 code = 1;
constant :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Const P.SourcePos)
constant = do
  reserved "const"
  withDocstring $
    T.Const
        <$> typeReference
        <*> (identifier <* equals)
        <*> constantValue
        <*  optionalSep


-- | A constant value literal.
constantValue
    :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.ConstValue P.SourcePos)
constantValue = withPosition $ P.choice
  [ P.try (string "0x") >> T.ConstInt <$> token PL.hexadecimal
  , either T.ConstFloat T.ConstInt
                      <$> token signedNumber
  , T.ConstLiteral    <$> literal
  , T.ConstIdentifier <$> identifier
  , T.ConstList       <$> constList
  , T.ConstMap        <$> constMap
  ]
  where
    signedNumber = floatingOrInteger <$> PL.signed whiteSpace PL.scientific


constList
    :: (P.TraversableStream s, P.Token s ~ Char) => Parser s [T.ConstValue P.SourcePos]
constList = symbolic '[' *> loop []
  where
    loop xs = P.choice
      [ symbolic ']' $> reverse xs
      , (:) <$> (constantValue <* optionalSep)
            <*> pure xs
            >>= loop
      ]


constMap
    :: (P.TraversableStream s, P.Token s ~ Char)
    => Parser s [(T.ConstValue P.SourcePos, T.ConstValue P.SourcePos)]
constMap = symbolic '{' *> loop []
  where
    loop xs = P.choice [
        symbolic '}' $> reverse xs
      , (:) <$> (constantValuePair <* optionalSep)
            <*> pure xs
            >>= loop
      ]


constantValuePair
    :: (P.TraversableStream s, P.Token s ~ Char)
    => Parser s (T.ConstValue P.SourcePos, T.ConstValue P.SourcePos)
constantValuePair =
    (,) <$> (constantValue <* colon)
        <*> (constantValue <* optionalSep)


-- | A reference to a built-in or defined field.
typeReference
    :: (P.TraversableStream s, P.Token s ~ Char)
    => Parser s (T.TypeReference P.SourcePos)
typeReference = P.choice
  [ baseType
  , containerType
  , withPosition (T.DefinedType <$> identifier)
  ]


baseType
    :: (P.TraversableStream s, P.Token s ~ Char)
    => Parser s (T.TypeReference P.SourcePos)
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
    :: (P.TraversableStream s, P.Token s ~ Char)
    => Parser s (T.TypeReference P.SourcePos)
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
service :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Service P.SourcePos)
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
function
    :: (P.TraversableStream s, P.Token s ~ Char) => Parser s (T.Function P.SourcePos)
function = withDocstring $
    T.Function
        <$> ((reserved "oneway" $> True) <|> pure False)
        <*> ((reserved "void" $> Nothing) <|> Just <$> typeReference)
        <*> identifier
        <*> functionParameters
        <*> optional (reserved "throws" *> parens (many field))
        <*> typeAnnotations
        <*  optionalSep

functionParameters
    :: (P.TraversableStream s, P.Token s ~ Char)
    => Parser s [T.Field P.SourcePos]
functionParameters = parens $ many field

-- | Type annotations on entitites.
--
-- > (foo = "bar", baz = "qux")
--
-- These do not usually affect code generation but allow for custom logic if
-- writing your own code generator.
typeAnnotations
    :: (P.TraversableStream s, P.Token s ~ Char) => Parser s [T.TypeAnnotation]
typeAnnotations = parens (many typeAnnotation) <|> pure []


typeAnnotation :: (P.TraversableStream s, P.Token s ~ Char) => Parser s T.TypeAnnotation
typeAnnotation =
    T.TypeAnnotation
        <$> identifier
        <*> (optional (equals *> literal) <* optionalSep)


optionalSep :: (P.TraversableStream s, P.Token s ~ Char) => Parser s ()
optionalSep = void $ optional (comma <|> semi)

string :: forall s. (P.Stream s, P.Token s ~ Char) => String -> Parser s (P.Tokens s)
string = PC.string . P.tokensToChunk (Proxy :: Proxy s)
