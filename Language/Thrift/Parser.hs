{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Thrift.Parser
    ( ThriftParser
    , runThriftParser

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


newtype ParserState = ParserState
  { parserLastDocstring :: T.Docstring
  } deriving (Show, Ord, Eq)

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

lastDocstring :: ThriftParser T.Docstring
lastDocstring = ThriftParser $ do
    s <- State.gets parserLastDocstring
    State.put (ParserState Nothing)
    return s

runThriftParser :: ThriftParser a -> Parser a
runThriftParser (ThriftParser p) = State.evalStateT p (ParserState Nothing)

instance TokenParsing ThriftParser where
    someSpace = skipSome $
        readDocstring <|> skipComments <|> skipSpace
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

idStyle :: IdentifierStyle ThriftParser
idStyle = (emptyIdents :: IdentifierStyle ThriftParser)
    { _styleStart = letter <|> char '_'
    , _styleLetter = alphaNum <|> oneOf "_."
    }

reserved :: Text -> ThriftParser ()
reserved = reserveText idStyle

program :: ThriftParser (T.Program Delta)
program = whiteSpace >> T.Program <$> many header <*> many definition

literal :: ThriftParser Text
literal = stringLiteral <|> stringLiteral'

identifier :: ThriftParser Text
identifier = ident idStyle

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

docstring :: ThriftParser (T.Docstring -> Delta -> a) -> ThriftParser a
docstring p = lastDocstring >>= \s -> do
    startPosition <- position
    p <*> pure s <*> pure startPosition

definition :: ThriftParser (T.Definition Delta)
definition = choice [constant, typeDefinition, service]

typeDefinition :: ThriftParser (T.Definition Delta)
typeDefinition =
  T.TypeDefinition
    <$> choice [typedef, enum, senum, struct, union, exception]
    <*> typeAnnotations

typedef :: ThriftParser (T.Type Delta)
typedef = reserved "typedef" >>
    docstring (T.Typedef <$> fieldType <*> identifier)

enum :: ThriftParser (T.Type Delta)
enum = reserved "enum" >>
    docstring (T.Enum <$> identifier <*> braces (many enumDef))

struct :: ThriftParser (T.Type Delta)
struct = reserved "struct" >>
    docstring (T.Struct <$> identifier <*> braces (many field))

union :: ThriftParser (T.Type Delta)
union = reserved "union" >>
    docstring (T.Union <$> identifier <*> braces (many field))

exception :: ThriftParser (T.Type Delta)
exception = reserved "exception" >>
     docstring (T.Exception <$> identifier <*> braces (many field))

fieldRequiredness :: ThriftParser T.FieldRequiredness
fieldRequiredness = choice [
    reserved "required" *> pure T.Required
  , reserved "optional" *> pure T.Optional
  ]

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

equals :: ThriftParser ()
equals = void $ symbolic '='

enumDef :: ThriftParser (T.EnumDef Delta)
enumDef = docstring $
  T.EnumDef
    <$> identifier
    <*> optional (equals *> integer)
    <*> typeAnnotations
    <*  optionalSep

senum :: ThriftParser (T.Type Delta)
senum = reserved "senum" >> docstring
    (T.Senum <$> identifier <*> braces (many (literal <* optionalSep)))

constant :: ThriftParser (T.Definition Delta)
constant = do
  reserved "const"
  docstring $
    T.ConstDefinition
        <$> fieldType
        <*> (identifier <* equals)
        <*> constantValue
        <*  optionalSep

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

service :: ThriftParser (T.Definition Delta)
service = do
  reserved "service"
  docstring $
    T.ServiceDefinition
        <$> identifier
        <*> optional (reserved "extends" *> identifier)
        <*> braces (many function)
        <*> typeAnnotations

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

typeAnnotations :: ThriftParser [T.TypeAnnotation]
typeAnnotations = parens (many typeAnnotation) <|> pure []

typeAnnotation :: ThriftParser T.TypeAnnotation
typeAnnotation =
    T.TypeAnnotation
        <$> identifier
        <*> (equals *> literal <* optionalSep)

optionalSep :: ThriftParser ()
optionalSep = void $ optional (comma <|> semi)
