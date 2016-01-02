-- This file implements the prettty printer. It is included with wl-pprint and
-- ansi-wl-pprint in scope.

#if __GLASGOW_HASKELL__ >= 709
import Prelude hiding ((<$>), lines)
#else
import Prelude hiding (lines)
#endif

import Data.Text (Text, unpack, lines, strip)

import qualified Language.Thrift.Internal.Types as T

import Language.Thrift.Pretty.Types


-- | Top-level pretty printer for Thrift documents that uses the default
-- configuration ('defaultConfig') for pretty printing.
prettyPrint :: T.Program ann -> Doc
prettyPrint = program defaultConfig


-- | Pretty print a Thrift IDL.
program :: Config -> T.Program ann -> Doc
program c T.Program{..} =
    vsep (map header programHeaders) <$> line <>
    map (definition c) programDefinitions `sepBy` (line <> line)

instance Pretty (T.Program a) where
    pretty = program defaultConfig

-- | Print the headers for a program.
header :: T.Header ann -> Doc
header (T.HeaderInclude inc) = include inc
header (T.HeaderNamespace ns) = namespace ns

instance Pretty (T.Header a) where
    pretty = header

include :: T.Include ann -> Doc
include T.Include{..} = text "include" <+> literal includePath

instance Pretty (T.Include a) where
    pretty = include

namespace :: T.Namespace ann -> Doc
namespace T.Namespace{..} = hsep
    [text "namespace", text namespaceLanguage, text namespaceName]

instance Pretty (T.Namespace a) where
    pretty = namespace

-- | Print a constant, type, or service definition.
definition :: Config -> T.Definition ann -> Doc
definition c (T.ConstDefinition cd) = constant c cd
definition c (T.TypeDefinition def) = typeDefinition c def
definition c (T.ServiceDefinition s) = service c s

instance Pretty (T.Definition a) where
    pretty = definition defaultConfig

constant :: Config -> T.Const ann -> Doc
constant c T.Const{..} = constDocstring $$ hsep
    [ text "const"
    , typeReference c constValueType
    , text constName
    , text "="
    , constantValue c constValue
    ]

instance Pretty (T.Const a) where
    pretty = constant defaultConfig

service :: Config -> T.Service ann -> Doc
service c@Config{indentWidth} T.Service{..} =
  serviceDocstring $$
    text "service" <+> text serviceName <> extends <+>
    block indentWidth (line <> line) (map (function c) serviceFunctions) <>
    typeAnnots c serviceAnnotations
  where
    extends = case serviceExtends of
      Nothing -> empty
      Just name -> space <> text "extends" <+> text name

instance Pretty (T.Service a) where
    pretty = service defaultConfig

-- | Pretty print a function definition.
--
function :: Config -> T.Function ann -> Doc
function c@Config{indentWidth} T.Function{..} = functionDocstring $$
  oneway <> returnType <+> text functionName <>
    encloseSep
        indentWidth lparen rparen comma
        (map (field c) functionParameters) <>
    exceptions <> typeAnnots c functionAnnotations <> semi
  where
    exceptions = case functionExceptions of
      Nothing -> empty
      Just es -> space <> text "throws" <+>
        encloseSep indentWidth lparen rparen comma (map (field c) es)
    returnType = case functionReturnType of
      Nothing -> text "void"
      Just rt -> typeReference c rt
    oneway =
      if functionOneWay
          then text "oneway" <> space
          else empty

instance Pretty (T.Function a) where
    pretty = function defaultConfig

typeDefinition :: Config -> T.Type ann -> Doc
typeDefinition c td = case td of
  T.TypedefType   t -> c `typedef`   t
  T.EnumType      t -> c `enum`      t
  T.StructType    t -> c `struct`    t
  T.UnionType     t -> c `union`     t
  T.ExceptionType t -> c `exception` t
  T.SenumType     t -> c `senum`     t

instance Pretty (T.Type a) where
    pretty = typeDefinition defaultConfig

typedef :: Config -> T.Typedef ann -> Doc
typedef c T.Typedef{..} = typedefDocstring $$
    text "typedef" <+> typeReference c typedefTargetType <+> text typedefName
    <> typeAnnots c typedefAnnotations

instance Pretty (T.Typedef a) where
    pretty = typedef defaultConfig

enum :: Config -> T.Enum ann -> Doc
enum c@Config{indentWidth} T.Enum{..} = enumDocstring $$
    text "enum" <+> text enumName <+>
      block indentWidth (comma <> line) (map (enumValue c) enumValues)
    <> typeAnnots c enumAnnotations

instance Pretty (T.Enum a) where
    pretty = enum defaultConfig

struct :: Config -> T.Struct ann -> Doc
struct c@Config{indentWidth} T.Struct{..} = structDocstring $$
    text "struct" <+> text structName <+>
      block indentWidth line (map (\f -> field c f <> semi) structFields)
    <> typeAnnots c structAnnotations

instance Pretty (T.Struct a) where
    pretty = struct defaultConfig

union :: Config -> T.Union ann -> Doc
union c@Config{indentWidth} T.Union{..} = unionDocstring $$
    text "union" <+> text unionName <+>
      block indentWidth line (map (\f -> field c f <> semi) unionFields)
    <> typeAnnots c unionAnnotations

instance Pretty (T.Union a) where
    pretty = union defaultConfig

exception :: Config -> T.Exception ann -> Doc
exception c@Config{indentWidth} T.Exception{..} = exceptionDocstring $$
    text "exception" <+> text exceptionName <+>
      block indentWidth line (map (\f -> field c f <> semi) exceptionFields)
    <> typeAnnots c exceptionAnnotations

instance Pretty (T.Exception a) where
    pretty = exception defaultConfig

senum :: Config -> T.Senum ann -> Doc
senum c@Config{indentWidth} T.Senum{..} = senumDocstring $$
    text "senum" <+> text senumName <+>
      encloseSep indentWidth lbrace rbrace comma (map literal senumValues)
    <> typeAnnots c senumAnnotations

instance Pretty (T.Senum a) where
    pretty = senum defaultConfig

field :: Config -> T.Field ann -> Doc
field c f = fieldDocstring $$
    hcat [fid, req, typeReference c typ, space, text fieldName, def, annots]
  where
    T.Field
        { fieldValueType = typ
        , fieldRequiredness = requiredness
        , ..
        } = f
    fid = case fieldIdentifier of
      Nothing -> empty
      Just i -> integer i <> colon <> space
    req = case requiredness of
      Nothing -> empty
      Just r  -> fieldRequiredness r <> space
    def = case fieldDefaultValue of
      Nothing -> empty
      Just v -> space <> equals <+> constantValue c v
    annots = typeAnnots c fieldAnnotations

instance Pretty (T.Field a) where
    pretty = field defaultConfig

fieldRequiredness :: T.FieldRequiredness -> Doc
fieldRequiredness T.Optional = text "optional"
fieldRequiredness T.Required = text "required"

instance Pretty T.FieldRequiredness where
    pretty = fieldRequiredness

enumValue :: Config -> T.EnumDef ann -> Doc
enumValue c T.EnumDef{..} = enumDefDocstring $$
    text enumDefName <> value <> typeAnnots c enumDefAnnotations
  where
    value = case enumDefValue of
      Nothing -> empty
      Just v  -> space <> text "=" <+> integer v

instance Pretty (T.EnumDef a) where
    pretty = enumValue defaultConfig

-- | Pretty print a field type.
typeReference :: Config -> T.TypeReference ann -> Doc
typeReference c ft = case ft of
  T.DefinedType t _ -> text t

  T.StringType anns -> text "string" <> typeAnnots c anns
  T.BinaryType anns -> text "binary" <> typeAnnots c anns
  T.SListType  anns -> text "slist"  <> typeAnnots c anns
  T.BoolType   anns -> text "bool"   <> typeAnnots c anns
  T.ByteType   anns -> text "byte"   <> typeAnnots c anns
  T.I16Type    anns -> text "i16"    <> typeAnnots c anns
  T.I32Type    anns -> text "i32"    <> typeAnnots c anns
  T.I64Type    anns -> text "i64"    <> typeAnnots c anns
  T.DoubleType anns -> text "double" <> typeAnnots c anns

  T.MapType k v anns ->
    text "map" <> angles (typeReference c k <> comma <+> typeReference c v)
               <> typeAnnots c anns
  T.SetType v anns ->
    text "set" <> angles (typeReference c v) <> typeAnnots c anns
  T.ListType v anns ->
    text "list" <> angles (typeReference c v) <> typeAnnots c anns

instance Pretty (T.TypeReference a) where
    pretty = typeReference defaultConfig

-- | Pretty print a constant value.
constantValue :: Config -> T.ConstValue ann -> Doc
constantValue c@Config{indentWidth} value = case value of
  T.ConstInt i -> integer i
  T.ConstFloat f -> double f
  T.ConstLiteral l -> literal l
  T.ConstIdentifier i _ -> text i
  T.ConstList vs ->
    encloseSep indentWidth lbracket rbracket comma $ map (constantValue c) vs
  T.ConstMap vs ->
    encloseSep indentWidth lbrace rbrace comma $
      map (\(k, v) -> constantValue c k <> colon <+> constantValue c v) vs

instance Pretty (T.ConstValue a) where
    pretty = constantValue defaultConfig

typeAnnots :: Config -> [T.TypeAnnotation] -> Doc
typeAnnots _ [] = empty
typeAnnots Config{indentWidth} anns =
    space <> encloseSep indentWidth lparen rparen comma (map typeAnnot anns)

typeAnnot :: T.TypeAnnotation -> Doc
typeAnnot T.TypeAnnotation{..} =
    text typeAnnotationName <> value
  where
    value = case typeAnnotationValue of
        Nothing -> empty
        Just v  -> space <> equals <+> literal v

instance Pretty T.TypeAnnotation where
    pretty = typeAnnot

literal :: Text -> Doc
literal = dquotes . text
    -- TODO: escaping?

text :: Text -> Doc
text = PP.text . unpack


($$) :: T.Docstring -> Doc -> Doc
($$) Nothing y = y
($$) (Just t) y = case lines (strip t) of
  [] -> y
  ls -> wrapComments ls <$> y
  where
    wrapComments ls = align . vsep
      $ text "/**"
      : map (\l -> text " *" <+> text l) ls
     ++ [text " */"]

infixr 1 $$


block :: Int -> Doc -> [Doc] -> Doc
block indent s items = braces $
    nest indent (linebreak <> (items `sepBy` s)) <> linebreak

sepBy :: [Doc] -> Doc -> Doc
sepBy [] _ = empty
sepBy [x] _ = x
sepBy (x:xs) s = x <> s <> sepBy xs s

encloseSep :: Int -> Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep _ left right _ [] = left <> right
encloseSep _ left right _ [v] = left <> v <> right
encloseSep indent left right s vs = group $
    nest indent (left <$$> go vs) <$$> right
  where go [] = empty
        go [x] = x
        go (x:xs) = (x <> s) <$> go xs
