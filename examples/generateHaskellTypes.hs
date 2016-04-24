{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

-- This is a fairly simple code generator that uses language-thrift to parse
-- IDL files and produces pretty printed Haskell code for all the types.
--
-- For services, it just generates a simple GADT that defines the inputs and
-- outputs for that service. Service inheritance is not supported and service
-- method exceptions are ignored.

#if __GLASGOW_HASKELL__ >= 709
import Prelude hiding ((<$>))
#endif

import Data.Char   (toLower, toUpper)
import Data.Maybe  (isNothing)
import Data.Text   (Text, unpack)
import System.Exit (exitFailure)
import System.IO   (hPrint, hPutStrLn, stderr, stdout)

import Text.PrettyPrint.ANSI.Leijen hiding (list, tupled)

import qualified Data.Text as Text

import Language.Thrift.Parser (parse)

import qualified Language.Thrift.AST as T

die :: String -> IO a
die s = hPutStrLn stderr s >> exitFailure

($$) :: T.Docstring -> Doc -> Doc
($$) Nothing y = y
($$) (Just t) y = case Text.lines t of
    [] -> y
    l:ls -> let docstring = align . vsep
                    $ (text "-- |" <+> text (unpack l))
                    : map ((text "--" <+>) . text . unpack) ls
          in align (docstring <$> y)
infixr 1 $$

list :: [Doc] -> Doc
list = encloseSep lbracket rbracket (text ", ")

tupled :: [Doc] -> Doc
tupled = encloseSep lparen rparen (text ", ")

renderConstValue :: T.ConstValue a -> Doc
renderConstValue (T.ConstInt i _) = integer i
renderConstValue (T.ConstFloat f _) = double f
renderConstValue (T.ConstLiteral l _) = dquotes $ text (unpack l) -- TODO escaping
renderConstValue (T.ConstIdentifier i _) = text (unpack i)
renderConstValue (T.ConstList l _) = list (map renderConstValue l)
renderConstValue (T.ConstMap m _) = text "Map.fromList" <+> list (map renderConstTuple m)
  where
    renderConstTuple (a, b) = tupled [renderConstValue a, renderConstValue b]

renderTypeReference :: Show a => T.TypeReference a -> Doc
renderTypeReference (T.DefinedType t _) = text (unpack t)
renderTypeReference (T.StringType _ _) = text "Text"
renderTypeReference (T.BinaryType _ _) = text "ByteString"
renderTypeReference (T.BoolType _ _) = text "Bool"
renderTypeReference (T.ByteType _ _) = text "Word8"
renderTypeReference (T.I16Type _ _) = text "Word16"
renderTypeReference (T.I32Type _ _) = text "Word32"
renderTypeReference (T.I64Type _ _) = text "Word64"
renderTypeReference (T.DoubleType _ _) = text "Double"
renderTypeReference (T.MapType k v _ _) =
    parens $ hsep [text "Map", renderTypeReference k, renderTypeReference v]
renderTypeReference (T.SetType i _ _) = parens $ text "Set" <+> renderTypeReference i
renderTypeReference (T.ListType i _ _) = brackets $ renderTypeReference i
renderTypeReference t = error $ "Unsupported field type: " ++ show t

renderStructField :: Show a => Text -> T.Field a -> Doc
renderStructField structName T.Field{..} = hang 4 $
    fieldDocstring $$ name </> hsep
      [ text "::"
      , (if isOptional
            then text "Maybe" <> space
            else empty)
        <> renderTypeReference fieldValueType
      ]
  where
    isOptional
      | isNothing fieldRequiredness = False
      | otherwise     = r == T.Optional && isNothing fieldDefaultValue
      where (Just r)  = fieldRequiredness
    name = text . unpack $ Text.concat [
        structName
      , underscoresToCamelCase False fieldName
      ]

renderTypedef :: Show a => T.Typedef a -> Doc
renderTypedef T.Typedef{..} = typedefDocstring $$ hsep
    [ text "type"
    , typeName typedefName
    , equals
    , renderTypeReference typedefTargetType
    ]

renderEnum :: T.Enum a -> Doc
renderEnum T.Enum{..} = enumDocstring $$
    text "data" <+> typeName enumName <>
    encloseSep (text " = ") empty (text " | ") (map renderDef enumValues)
    <$$> indent 4 derivingClause
  where
    renderDef T.EnumDef{..} = enumDefDocstring $$ typeName enumDefName

renderStruct :: Show a => T.Struct a -> Doc
renderStruct T.Struct{..} = structDocstring $$
    text "data" <+> typeName structName </>
    equals <+> typeName structName <$$>
    (if null structFields
        then empty
        else indent 2 renderFields) </> derivingClause
  -- TODO prefix should be configurable using annotations
  where
    renderFields = encloseSep (text "{ ") (line <> text "}") (text ", ") $
        map (renderStructField $ underscoresToCamelCase True structName)
             structFields

renderException :: Show a => T.Exception a -> Doc
renderException T.Exception{..} = renderStruct T.Struct
    { T.structName = exceptionName
    , T.structFields = exceptionFields
    , T.structAnnotations = exceptionAnnotations
    , T.structDocstring = exceptionDocstring
    , T.structSrcAnnot = exceptionSrcAnnot
    }

renderUnion :: Show a => T.Union a -> Doc
renderUnion T.Union{..} =
    hang 4
      (unionDocstring $$
          text "data" <+> typeName unionName <$>
          encloseSep (text "= ") empty (text " | ")
                     (map renderField unionFields))
    <$$> indent 4 derivingClause
  where
    renderField (T.Field _ _ ftype fname _ _ docstring _) =
        docstring $$ fieldName </> renderTypeReference ftype
      where
        fieldName = text . unpack $ Text.concat
          [ underscoresToCamelCase False unionName
          , underscoresToCamelCase False fname
          ]

derivingClause :: Doc
derivingClause =
    text "deriving" <+> tupled (map text ["Show", "Ord", "Eq"])

renderType :: Show a => T.Type a -> Doc
renderType (T.TypedefType   t) = renderTypedef t
renderType (T.EnumType      t) = renderEnum t
renderType (T.ExceptionType t) = renderException t
renderType (T.StructType    t) = renderStruct t
renderType (T.UnionType     t) = renderUnion t
renderType                  t  = error $ "Unsupported type: " ++ show t

typeName :: Text -> Doc
typeName = mkName False

renderConst :: Show a => T.Const a -> Doc
renderConst T.Const{..} = constDocstring $$
    sep [name, text "::", renderTypeReference constValueType] <$>
    sep [name, text "=", renderConstValue constValue]
  where
    name = mkName True constName


renderFunction :: Show a => Text -> T.Function a -> Doc
renderFunction serviceName T.Function{functionOneWay = False, ..} =
      functionDocstring $$
          typeName functionName <+> text "::" <>
          (if null functionParameters
              then space
              else linebreak <> renderParams) <>
          typeName serviceName <+> returnType <> linebreak
  where
    returnType = case functionReturnType of
        Nothing -> text "()"
        Just t  -> renderTypeReference t

    renderParams = indent 2 $
        encloseSep (text "{ ") (line <> text "} -> ") (text ", ") $
        map (renderStructField structName) functionParameters
      where
        structName = underscoresToCamelCase True functionName
renderFunction _ f = error $ "Unsupported function: " ++ show f


renderService :: Show a => T.Service a -> Doc
renderService T.Service{serviceExtends = Nothing, ..} = serviceDocstring $$
    text "data" <+> typeName serviceName <+> text "a where" <$$>
    indent 2 (vcat (map (renderFunction serviceName) serviceFunctions))
renderService s = error $ "Unsupported service: " ++ show s


renderDefinition :: Show a => T.Definition a -> Doc
renderDefinition (T.ConstDefinition   c) = renderConst   c
renderDefinition (T.TypeDefinition    t) = renderType    t
renderDefinition (T.ServiceDefinition s) = renderService s


generateOutput :: Show a => T.Program a -> IO ()
generateOutput (T.Program _ definitions) = do
    let doc = headers <$> empty <$>
              vcat (map ((<$> empty) . renderDefinition) definitions)
    displayIO stdout $ renderPretty 0.8 80 doc
  where
    import_ m items = sep [
        text "import"
      , text m
      , maybe empty (tupled . map string) items
      ]

    importQualified m s = sep [
        text "import qualified"
      , text m
      , text "as"
      , text s
      ]

    headers = vcat [
        import_ "Data.Map" (Just ["Map"])
      , import_ "Data.Set" (Just ["Set"])
      , import_ "Data.Text" (Just ["Text"])
      , import_ "Data.ByteString" (Just ["ByteString"])
      , import_ "Data.Word" (Just ["Word8", "Word16", "Word32", "Word64"])
      , text ""
      , importQualified "Data.Map" "Map"
      ]

mkName :: Bool -> Text -> Doc
mkName lowerFirst = text . unpack . underscoresToCamelCase lowerFirst

underscoresToCamelCase :: Bool -> Text -> Text
underscoresToCamelCase lowerFirst =
    camelCase lowerFirst . Text.split (== '_')

camelCase :: Bool -> [Text] -> Text
camelCase lowerFirst =
    maybeLower . Text.concat . map (transformIndex toUpper 0)
  where
    maybeLower = if lowerFirst then transformIndex toLower 0 else id

transformIndex :: (Char -> Char) -> Int -> Text -> Text
transformIndex f i s = Text.concat [
    Text.take i s
  , Text.singleton $ f (s `Text.index` i)
  , Text.drop (i + 1) s
  ]

main :: IO ()
main = do
    result <- parse "stdin" `fmap` getContents
    case result of
        Right p -> generateOutput p
        Left err -> do
            hPrint stderr err
            die "Parse Failed"
