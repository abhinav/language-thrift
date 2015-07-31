{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
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
    (

      thriftIDLParser

    -- * Parser type

    , ThriftParser
    , runThriftParser

    -- * Parser components

    , program
    , header
    , definition
    , typeDefinition
    , typedef
    , enum
    , enumDef
    , senum
    , struct
    , union
    , exception
    , fieldRequiredness
    , fieldType
    , field
    , constant
    , constantValue
    , service
    , function
    , typeAnnotations
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State     (StateT)
import Data.Text               (Text)
import Text.Parser.Token.Style (emptyIdents)
import Text.Trifecta
import Text.Trifecta.Delta     (Delta)

import qualified Control.Monad.State as State
import qualified Data.Text           as Text

import qualified Language.Thrift.Types as T


-- | Top level Trifecta parser that is able to parse full Thrift documents.
--
-- Entities defined in the IDL are annotated with 'Delta's which contain
-- location information about those definitions.
--
-- This may be executed with 'parseFromFile', 'parseString', or other Trifecta
-- variants.
thriftIDLParser :: Parser (T.Program Delta)
thriftIDLParser = runThriftParser program


-- | Keeps track of the last docstring seen by the system so that we can
-- attach it to entities.
newtype ParserState = ParserState
  { parserLastDocstring :: T.Docstring
  } deriving (Show, Ord, Eq)

-- | A parser that wraps the standard Trifecta parser with extra state.
newtype ThriftParser a = ThriftParser (StateT ParserState Parser a)
    deriving
      ( Functor
      , Applicative
      , Alternative
      , Monad
      , MonadPlus
      , Parsing
      , CharParsing
      , DeltaParsing
      )

-- | Returns the last docstring recorded by the system and clears it from the
-- parser state.
lastDocstring :: ThriftParser T.Docstring
lastDocstring = ThriftParser $ do
    s <- State.gets parserLastDocstring
    State.put (ParserState Nothing)
    return s

-- | Get a Trifecta parser.
runThriftParser :: ThriftParser a -> Parser a
runThriftParser (ThriftParser p) = State.evalStateT p (ParserState Nothing)

instance TokenParsing ThriftParser where
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
                Text.unlines . map (Text.dropWhile ignore) . Text.lines
              where
                ignore c = c == '*' || c == ' '


-- | Type of identifiers allowed by Thrift.
idStyle :: IdentifierStyle ThriftParser
idStyle = (emptyIdents :: IdentifierStyle ThriftParser)
    { _styleStart = letter <|> char '_'
    , _styleLetter = alphaNum <|> oneOf "_."
    }


-- | Constructor for reserved keywords.
reserved :: Text -> ThriftParser ()
reserved = reserveText idStyle


-- | Top-level parser to parse complete Thrift documents.
program :: ThriftParser (T.Program Delta)
program = whiteSpace >> T.Program <$> many header <*> many definition


-- | A string literal. @"hello"@
literal :: ThriftParser Text
literal = stringLiteral <|> stringLiteral'


-- | An identifier in a Thrift file.
identifier :: ThriftParser Text
identifier = ident idStyle


-- | Headers defined for the IDL.
header :: ThriftParser T.Header
header = choice [
    reserved "include" >> T.Include <$> literal
  , reserved "namespace" >>
      T.Namespace <$> (text "*" <|> identifier) <*> identifier
  , reserved "cpp_namespace" >> T.Namespace "cpp" <$> identifier
  , reserved "php_namespace" >> T.Namespace "php" <$> identifier
  , reserved "py_module" >> T.Namespace "py" <$> identifier
  , reserved "perl_package" >> T.Namespace "perl" <$> identifier
  , reserved "ruby_namespace" >> T.Namespace "rb" <$> identifier
  , reserved "java_package" >> T.Namespace "java" <$> identifier
  , reserved "cocoa_package" >> T.Namespace "cocoa" <$> identifier
  , reserved "csharp_namespace" >> T.Namespace "csharp" <$> identifier
  ]


-- | Convenience wrapper for parsers that expect a docstring and a location
-- 'Delta'.
--
-- > data Foo = Foo { bar :: Bar, doc :: Docstring, pos :: Delta }
-- >
-- > parseFoo = docstring $ Foo <$> parseBar
docstring :: ThriftParser (T.Docstring -> Delta -> a) -> ThriftParser a
docstring p = lastDocstring >>= \s -> do
    startPosition <- position
    p <*> pure s <*> pure startPosition


-- | A constant, type, or service definition.
definition :: ThriftParser (T.Definition Delta)
definition = choice [constant, typeDefinition, service]


-- | A type definition.
typeDefinition :: ThriftParser (T.Definition Delta)
typeDefinition =
  T.TypeDefinition
    <$> choice [typedef, enum, senum, struct, union, exception]
    <*> typeAnnotations


-- | A typedef is just an alias for another type.
--
-- > typedef common.Foo Bar
typedef :: ThriftParser (T.Type Delta)
typedef = reserved "typedef" >>
    docstring (T.Typedef <$> fieldType <*> identifier)


-- | Enums are sets of named integer values.
--
-- > enum Role {
-- >     User = 1, Admin
-- > }
enum :: ThriftParser (T.Type Delta)
enum = reserved "enum" >>
    docstring (T.Enum <$> identifier <*> braces (many enumDef))


-- | A @struct@.
--
-- > struct User {
-- >     1: string name
-- >     2: Role role = Role.User;
-- > }
struct :: ThriftParser (T.Type Delta)
struct = reserved "struct" >>
    docstring (T.Struct <$> identifier <*> braces (many field))


-- | A @union@ of types.
--
-- > union Value {
-- >     1: string stringValue;
-- >     2: i32 intValue;
-- > }
union :: ThriftParser (T.Type Delta)
union = reserved "union" >>
    docstring (T.Union <$> identifier <*> braces (many field))


-- | An @exception@ that can be raised by service methods.
--
-- > exception UserDoesNotExist {
-- >     1: optional string message
-- >     2: required string username
-- > }
exception :: ThriftParser (T.Type Delta)
exception = reserved "exception" >>
     docstring (T.Exception <$> identifier <*> braces (many field))


-- | Whether a field is @required@ or @optional@.
fieldRequiredness :: ThriftParser T.FieldRequiredness
fieldRequiredness = choice [
    reserved "required" *> pure T.Required
  , reserved "optional" *> pure T.Optional
  ]

-- | A struct field.
field :: ThriftParser (T.Field Delta)
field = docstring $
  T.Field
    <$> optional (integer <* symbolic ':')
    <*> optional fieldRequiredness
    <*> fieldType
    <*> identifier
    <*> optional (equals *> constantValue)
    <*> typeAnnotations
    <*  optionalSep


-- | A value defined inside an @enum@.
enumDef :: ThriftParser (T.EnumDef Delta)
enumDef = docstring $
  T.EnumDef
    <$> identifier
    <*> optional (equals *> integer)
    <*> typeAnnotations
    <*  optionalSep


-- | An string-only enum. These are a deprecated feature of Thrift and
-- shouldn't be used.
senum :: ThriftParser (T.Type Delta)
senum = reserved "senum" >> docstring
    (T.Senum <$> identifier <*> braces (many (literal <* optionalSep)))


-- | A 'const' definition.
--
-- > const i32 code = 1;
constant :: ThriftParser (T.Definition Delta)
constant = do
  reserved "const"
  docstring $
    T.ConstDefinition
        <$> fieldType
        <*> (identifier <* equals)
        <*> constantValue
        <*  optionalSep


-- | A constant value literal.
constantValue :: ThriftParser T.ConstValue
constantValue = choice [
    either T.ConstInt T.ConstFloat <$> integerOrDouble
  , T.ConstLiteral <$> literal
  , T.ConstIdentifier <$> identifier
  , T.ConstList <$> constList
  , T.ConstMap <$> constMap
  ]


constList :: ThriftParser [T.ConstValue]
constList = brackets $ commaSep (constantValue <* optionalSep)


constMap :: ThriftParser [(T.ConstValue, T.ConstValue)]
constMap = braces $ commaSep constantValuePair


constantValuePair :: ThriftParser (T.ConstValue, T.ConstValue)
constantValuePair =
    (,) <$> (constantValue <* colon)
        <*> (constantValue <* optionalSep)


-- | A reference to a built-in or defined field.
fieldType :: ThriftParser T.FieldType
fieldType = choice [
    baseType
  , containerType
  , T.DefinedType <$> identifier
  ]


baseType :: ThriftParser T.FieldType
baseType =
    choice [reserved s *> (v <$> typeAnnotations) | (s, v) <- bases]
  where
    bases = [
        ("string", T.StringType)
      , ("binary", T.BinaryType)
      , ("slist", T.SListType)
      , ("bool", T.BoolType)
      , ("byte", T.ByteType)
      , ("i16", T.I16Type)
      , ("i32", T.I32Type)
      , ("i64", T.I64Type)
      , ("double", T.DoubleType)
      ]


containerType :: ThriftParser T.FieldType
containerType =
    choice [mapType, setType, listType] <*> typeAnnotations
  where
    mapType = reserved "map" >>
        angles (T.MapType <$> (fieldType <* comma) <*> fieldType)
    setType = reserved "set" >> angles (T.SetType <$> fieldType)
    listType = reserved "list" >> angles (T.ListType <$> fieldType)


-- | A service.
--
-- > service MyService {
-- >     // ...
-- > }
service :: ThriftParser (T.Definition Delta)
service = do
  reserved "service"
  docstring $
    T.ServiceDefinition
        <$> identifier
        <*> optional (reserved "extends" *> identifier)
        <*> braces (many function)
        <*> typeAnnotations


-- | A function defined inside a service.
--
-- > Foo getFoo() throws (1: FooDoesNotExist doesNotExist);
-- > oneway void putBar(1: Bar bar);
function :: ThriftParser (T.Function Delta)
function = docstring $
    T.Function
        <$> ((reserved "oneway" *> pure True) <|> pure False)
        <*> ((reserved "void" *> pure Nothing) <|> Just <$> fieldType)
        <*> identifier
        <*> parens (many field)
        <*> optional (reserved "throws" *> parens (many field))
        <*> typeAnnotations
        <*  optionalSep


-- | Type annotations on entitites.
--
-- > ("foo" = "bar", "baz" = "qux")
--
-- These do not usually affect code generation but allow for custom logic if
-- writing your own code generator.
typeAnnotations :: ThriftParser [T.TypeAnnotation]
typeAnnotations = parens (many typeAnnotation) <|> pure []


typeAnnotation :: ThriftParser T.TypeAnnotation
typeAnnotation =
    T.TypeAnnotation
        <$> identifier
        <*> (equals *> literal <* optionalSep)


optionalSep :: ThriftParser ()
optionalSep = void $ optional (comma <|> semi)


equals :: ThriftParser ()
equals = void $ symbolic '='

