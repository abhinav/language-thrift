{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Module      :  Language.Thrift.Parser
-- Copyright   :  (c) Abhinav Gupta 2015
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
    ( thriftIDL

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

    -- * Parser

    , ThriftParser
    , runThriftParser
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader    (ReaderT)
import Control.Monad.State     (StateT)
import Control.Monad.Trans     (lift)
import Data.Text               (Text)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style (emptyIdents)

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State  as State
import qualified Data.Text            as Text

import qualified Language.Thrift.Types as T


-- | Get a top level parser that is able to parse full Thrift documents.
--
-- Entities defined in the IDL are annotated with @n@ values (determined by
-- executing @p n@ before the parser for the entity is executed).
--
-- Usage with Trifecta to get entities tagged with location information (see
-- also, 'Language.Thrift.Parser.Trifecta.thriftIDL'):
--
-- > Trifecta.parseFromFile (thriftIDL Trifecta.position) "service.thrift"
--
-- Usage with Attoparsec without any annotations:
--
-- > Attoparsec.parse (thriftIDL (return ())) document
--
thriftIDL :: (MonadPlus p, TokenParsing p) => p n -> p (T.Program n)
thriftIDL getAnnot = runThriftParser getAnnot program


-- | Keeps track of the last docstring seen by the system so that we can
-- attach it to entities.
newtype ParserState = ParserState
  { parserLastDocstring :: T.Docstring
  } deriving (Show, Ord, Eq)

-- | The ThriftParser wraps another parser @p@ with some extra state. It also
-- allows injecting a configurable action @(p n)@ which produces annotations
-- that will be attached to entities in the Thrift file. See 'thriftIDLParser'
-- for an example.
newtype ThriftParser p n a =
        ThriftParser (StateT ParserState (ReaderT (p n) p) a)
    deriving
      ( Functor
      , Applicative
      , Alternative
      , Monad
      , MonadPlus
      , Parsing
      , CharParsing
      )

-- | Returns the last docstring recorded by the system and clears it from the
-- parser state.
lastDocstring :: Monad p => ThriftParser p n T.Docstring
lastDocstring = ThriftParser $ do
    s <- State.gets parserLastDocstring
    State.put (ParserState Nothing)
    return s

-- | Get an exeecutable parser from the given ThriftParser.
runThriftParser
    :: (MonadPlus p, TokenParsing p)
    => p n
    -- ^ How to get annotations from the underlying parser. If this is not
    -- something you need to use, make it @return ()@ and generated types will
    -- be annotated with @()@.
    -> ThriftParser p n a
    -> p a
runThriftParser getAnnot (ThriftParser p) =
    Reader.runReaderT (State.evalStateT p (ParserState Nothing)) getAnnot


instance
  (TokenParsing p, MonadPlus p) => TokenParsing (ThriftParser p n) where
    -- Docstring parsing works by cheating. We define docstrings as
    -- whitespace, but we record it when we move over it. If we run into
    -- another newline or other comments after seeing the docstring's "*/\n",
    -- we clear the docstring out because it's most likely not attached to the
    -- entity that follows. So for docstrings to be attached, there must be a
    -- single newline between "*/" and the entity.
    someSpace = skipSome $ readDocstring <|> skipComments <|> skipSpace
      where
        skipSpace = choice [
            newline *> clearDocstring
          , ThriftParser someSpace
          ]

        skipComments = choice [
              char '#'   *> skipLine
            , text "//"  *> skipLine
            , text "/*"  *> skipCStyleComment
            ] *> clearDocstring
        skipLine = skipMany (satisfy (/= '\n')) <* newline
        skipCStyleComment = choice [
            text "*/"              *> pure ()
          , skipSome (noneOf "/*") *> skipCStyleComment
          , oneOf "/*"             *> skipCStyleComment
          ]

        -- TODO this is really ugly. use some sort of docstring parser instead
        clearDocstring = ThriftParser $ State.put (ParserState Nothing)
        readDocstring = text "/**" *> loop []
          where
            saveDocstring s = unless (Text.null s') $
                ThriftParser . State.put . ParserState . Just $ s'
              where
                s' = sanitizeDocstring s
            loop chunks = choice [
                text "*/" *> optional (newline <|> space) *>
                saveDocstring (Text.strip . Text.concat $ reverse chunks)
              , Text.pack      <$> some (noneOf "/*") >>= loop . (:chunks)
              , Text.singleton <$>        oneOf "/*"  >>= loop . (:chunks)
              ]
            sanitizeDocstring :: Text -> Text
            sanitizeDocstring =
                Text.intercalate "\n"
              . map (Text.dropWhile ignore)
              . Text.lines
              where ignore c = c == '*' || c == ' '


-- | Type of identifiers allowed by Thrift.
idStyle
    :: forall p n. (TokenParsing p, MonadPlus p)
    => IdentifierStyle (ThriftParser p n)
idStyle = (emptyIdents :: IdentifierStyle (ThriftParser p n))
    { _styleStart = letter <|> char '_'
    , _styleLetter = alphaNum <|> oneOf "_."
    }


-- | Constructor for reserved keywords.
reserved :: (TokenParsing p, MonadPlus p) => Text -> ThriftParser p n ()
reserved = reserveText idStyle


-- | Top-level parser to parse complete Thrift documents.
program :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Program n)
program = whiteSpace >> T.Program <$> many header <*> many definition


-- | A string literal. @"hello"@
literal :: (TokenParsing p, MonadPlus p) => ThriftParser p n Text
literal = stringLiteral <|> stringLiteral'


-- | An identifier in a Thrift file.
identifier :: (TokenParsing p, MonadPlus p) => ThriftParser p n Text
identifier = ident idStyle


-- | Headers defined for the IDL.
header :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Header n)
header = choice
  [ T.HeaderInclude   <$> include
  , T.HeaderNamespace <$> namespace
  ]

include :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Include n)
include = reserved "include" >> withSrcAnnot (T.Include <$> literal)

namespace :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Namespace n)
namespace = choice
  [ reserved "namespace" >>
    withSrcAnnot (T.Namespace <$> (text "*" <|> identifier) <*> identifier)
  , reserved "cpp_namespace" >>
    withSrcAnnot (T.Namespace "cpp" <$> identifier)
  , reserved "php_namespace" >>
    withSrcAnnot (T.Namespace "php" <$> identifier)
  , reserved "py_module" >>
    withSrcAnnot (T.Namespace "py" <$> identifier)
  , reserved "perl_package" >>
    withSrcAnnot (T.Namespace "perl" <$> identifier)
  , reserved "ruby_namespace" >>
    withSrcAnnot (T.Namespace "rb" <$> identifier)
  , reserved "java_package" >>
    withSrcAnnot (T.Namespace "java" <$> identifier)
  , reserved "cocoa_package" >>
    withSrcAnnot (T.Namespace "cocoa" <$> identifier)
  , reserved "csharp_namespace" >>
    withSrcAnnot (T.Namespace "csharp" <$> identifier)
  ]

-- | Retrieve the current source annotation.
getSrcAnnot :: Monad p => ThriftParser p n n
getSrcAnnot = ThriftParser . lift $ Reader.ask >>= lift

-- | Convenience wrapper for parsers expecting a source annotation.
--
-- The source annotation will be retrieved BEFORE the parser itself is
-- executed.
withSrcAnnot
    :: (Functor p, Monad p)
    => ThriftParser p n (n -> a) -> ThriftParser p n a
withSrcAnnot p = getSrcAnnot >>= \annot -> p <*> pure annot

-- | Convenience wrapper for parsers that expect a docstring and a
-- source annotation.
--
-- > data Foo = Foo { bar :: Bar, doc :: Docstring, pos :: Delta }
-- >
-- > parseFoo = docstring $ Foo <$> parseBar
docstring
    :: (Functor p, Monad p)
    => ThriftParser p n (T.Docstring -> n -> a) -> ThriftParser p n a
docstring p = lastDocstring >>= \s -> do
    annot <- getSrcAnnot
    p <*> pure s <*> pure annot


-- | A constant, type, or service definition.
definition
    :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Definition n)
definition = choice
    [ T.ConstDefinition   <$> constant
    , T.TypeDefinition    <$> typeDefinition
    , T.ServiceDefinition <$> service
    ]


-- | A type definition.
typeDefinition :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Type n)
typeDefinition = choice
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
typedef :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Typedef n)
typedef = reserved "typedef" >>
    docstring (T.Typedef <$> typeReference <*> identifier <*> typeAnnotations)


-- | Enums are sets of named integer values.
--
-- > enum Role {
-- >     User = 1, Admin
-- > }
enum :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Enum n)
enum = reserved "enum" >>
    docstring (T.Enum
        <$> identifier
        <*> braces (many enumDef)
        <*> typeAnnotations)


-- | A @struct@.
--
-- > struct User {
-- >     1: string name
-- >     2: Role role = Role.User;
-- > }
struct :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Struct n)
struct = reserved "struct" >>
    docstring (T.Struct
        <$> identifier
        <*> braces (many field)
        <*> typeAnnotations)


-- | A @union@ of types.
--
-- > union Value {
-- >     1: string stringValue;
-- >     2: i32 intValue;
-- > }
union :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Union n)
union = reserved "union" >>
    docstring (T.Union
        <$> identifier
        <*> braces (many field)
        <*> typeAnnotations)


-- | An @exception@ that can be raised by service methods.
--
-- > exception UserDoesNotExist {
-- >     1: optional string message
-- >     2: required string username
-- > }
exception :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Exception n)
exception = reserved "exception" >>
     docstring (T.Exception
        <$> identifier
        <*> braces (many field)
        <*> typeAnnotations)


-- | Whether a field is @required@ or @optional@.
fieldRequiredness
    :: (TokenParsing p, MonadPlus p) => ThriftParser p n T.FieldRequiredness
fieldRequiredness = choice [
    reserved "required" *> pure T.Required
  , reserved "optional" *> pure T.Optional
  ]

-- | A struct field.
field :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Field n)
field = docstring $
  T.Field
    <$> optional (integer <* symbolic ':')
    <*> optional fieldRequiredness
    <*> typeReference
    <*> identifier
    <*> optional (equals *> constantValue)
    <*> typeAnnotations
    <*  optionalSep


-- | A value defined inside an @enum@.
enumDef :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.EnumDef n)
enumDef = docstring $
  T.EnumDef
    <$> identifier
    <*> optional (equals *> integer)
    <*> typeAnnotations
    <*  optionalSep


-- | An string-only enum. These are a deprecated feature of Thrift and
-- shouldn't be used.
senum :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Senum n)
senum = reserved "senum" >> docstring
    (T.Senum
        <$> identifier
        <*> braces (many (literal <* optionalSep))
        <*> typeAnnotations)


-- | A 'const' definition.
--
-- > const i32 code = 1;
constant :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Const n)
constant = do
  reserved "const"
  docstring $
    T.Const
        <$> typeReference
        <*> (identifier <* equals)
        <*> constantValue
        <*  optionalSep


-- | A constant value literal.
constantValue
    :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.ConstValue n)
constantValue = choice [
    either T.ConstInt T.ConstFloat <$> integerOrDouble
  , T.ConstLiteral <$> literal
  , withSrcAnnot (T.ConstIdentifier <$> identifier)
  , T.ConstList <$> constList
  , T.ConstMap <$> constMap
  ]


constList
    :: (TokenParsing p, MonadPlus p) => ThriftParser p n [T.ConstValue n]
constList = symbolic '[' *> loop []
  where
    loop xs = choice [
        symbolic ']' *> return (reverse xs)
      , (:) <$> (constantValue <* optionalSep)
            <*> pure xs
            >>= loop
      ]


constMap
    :: (TokenParsing p, MonadPlus p)
    => ThriftParser p n [(T.ConstValue n, T.ConstValue n)]
constMap = symbolic '{' *> loop []
  where
    loop xs = choice [
        symbolic '}' *> return (reverse xs)
      , (:) <$> (constantValuePair <* optionalSep)
            <*> pure xs
            >>= loop
      ]


constantValuePair
    :: (TokenParsing p, MonadPlus p)
    => ThriftParser p n (T.ConstValue n, T.ConstValue n)
constantValuePair =
    (,) <$> (constantValue <* colon)
        <*> (constantValue <* optionalSep)


-- | A reference to a built-in or defined field.
typeReference
    :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.TypeReference n)
typeReference = choice [
    baseType
  , containerType
  , withSrcAnnot (T.DefinedType <$> identifier)
  ]


baseType
    :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.TypeReference n)
baseType =
    choice [reserved s *> (v <$> typeAnnotations) | (s, v) <- bases]
  where
    bases = [
        ("string", T.StringType)
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
    :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.TypeReference n)
containerType =
    choice [mapType, setType, listType] <*> typeAnnotations
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
service :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Service n)
service = do
  reserved "service"
  docstring $
    T.Service
        <$> identifier
        <*> optional (reserved "extends" *> identifier)
        <*> braces (many function)
        <*> typeAnnotations


-- | A function defined inside a service.
--
-- > Foo getFoo() throws (1: FooDoesNotExist doesNotExist);
-- > oneway void putBar(1: Bar bar);
function :: (TokenParsing p, MonadPlus p) => ThriftParser p n (T.Function n)
function = docstring $
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
typeAnnotations
    :: (TokenParsing p, MonadPlus p)
    => ThriftParser p n [T.TypeAnnotation]
typeAnnotations = parens (many typeAnnotation) <|> pure []


typeAnnotation
    :: (TokenParsing p, MonadPlus p)
    => ThriftParser p n T.TypeAnnotation
typeAnnotation =
    T.TypeAnnotation
        <$> identifier
        <*> (optional (equals *> literal) <* optionalSep)


optionalSep :: (TokenParsing p, MonadPlus p) => ThriftParser p n ()
optionalSep = void $ optional (comma <|> semi)


equals :: (TokenParsing p, MonadPlus p) => ThriftParser p n ()
equals = void $ symbolic '='
