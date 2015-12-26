{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- This is a fairly simple code generator that uses language-thrift to parse
-- IDL files and produces pretty printed Haskell code for all the types.
--
-- For services, it just generates a simple GADT that defines the inputs and
-- outputs for that service. Service inheritanec is not supported and
-- service method exceptions are ignored.

import Prelude hiding ((<$>))

import Data.Char   (toLower, toUpper)
import Data.Maybe  (isNothing)
import Data.Text   (Text, unpack)
import System.Exit (exitFailure)
import System.IO   (stderr, stdout)

import qualified Data.Text                    as Text
import qualified Text.PrettyPrint.ANSI.Leijen as AnsiPP
import           Text.PrettyPrint.Leijen      hiding (list, tupled)
import           Text.Trifecta                (Result (..), parseString)
import           Text.Trifecta.Delta          (Delta (Directed))

import Language.Thrift.Parser.Trifecta (thriftIDL)
import Language.Thrift.Types

die :: String -> IO a
die s = putStrLn s >> exitFailure

-- | '<$>' with the arguments flipped.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>

($$) :: Docstring -> Doc -> Doc
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

renderConstValue :: ConstValue a -> Doc
renderConstValue (ConstInt i) = integer i
renderConstValue (ConstFloat f) = double f
renderConstValue (ConstLiteral l) = dquotes $ text (unpack l) -- TODO escaping
renderConstValue (ConstIdentifier i _) = text (unpack i)
renderConstValue (ConstList l) = list (map renderConstValue l)
renderConstValue (ConstMap m) = text "Map.fromList" <+> list (map renderConstTuple m)
  where
    renderConstTuple (a, b) = tupled [
        renderConstValue a
      , renderConstValue b
      ]

renderTypeReference :: Show a => TypeReference a -> Doc
renderTypeReference (DefinedType t _) = text (unpack t)
renderTypeReference (StringType _) = text "Text"
renderTypeReference (BinaryType _) = text "ByteString"
renderTypeReference (BoolType _) = text "Bool"
renderTypeReference (ByteType _) = text "Word8"
renderTypeReference (I16Type _) = text "Word16"
renderTypeReference (I32Type _) = text "Word32"
renderTypeReference (I64Type _) = text "Word64"
renderTypeReference (DoubleType _) = text "Double"
renderTypeReference (MapType k v _) =
    parens $ hsep [text "Map", renderTypeReference k, renderTypeReference v]
renderTypeReference (SetType i _) = parens $ text "Set" <+> renderTypeReference i
renderTypeReference (ListType i _) = brackets $ renderTypeReference i
renderTypeReference t = error $ "Unsupported field type: " ++ show t

renderStructField :: Show a => Text -> Field a -> Doc
renderStructField structName (Field _ req ftype fname def _ docstring _) = hang 4 $
    docstring $$
    fieldName </>
    hsep [
        text "::"
      , (if isOptional
            then text "Maybe" <> space
            else empty) <> renderTypeReference ftype
      ]
  where
    isOptional
      | isNothing req = False
      | otherwise     = r == Optional && isNothing def
      where (Just r)  = req
    fieldName = text . unpack $ Text.concat [
        structName
      , underscoresToCamelCase False fname
      ]

renderType :: Show a => Type a -> Doc
renderType = go
  where
    derivingClause =
        text "deriving" <+> tupled (map text ["Show", "Ord", "Eq"])

    go (TypedefType (Typedef fieldType name _ docstring _)) = docstring $$
        hsep [text "type", typeName name, equals, renderTypeReference fieldType]
    go (EnumType (Enum name defs _ docstring _)) = docstring $$
        text "data" <+> typeName name <>
        encloseSep (text " = ") empty (text " | ") (map renderDef defs)
        <$$> indent 4 derivingClause
      where
        renderDef (EnumDef e _ _ docstring _) = docstring $$ typeName e
    go (ExceptionType (Exception name fields t docstring a)) =
        go (StructType (Struct name fields t docstring a))
    go (StructType (Struct name fields _ docstring _)) = docstring $$
        text "data" <+> typeName name </> equals <+> typeName name <$$>
        (if null fields
            then empty
            else indent 2 renderFields)
        </> derivingClause
      -- TODO prefix should be configurable using annotations
      where
        renderFields =
            encloseSep (text "{ ") (line <> text "}") (text ", ") $
            map (renderStructField structName) fields
        structName = underscoresToCamelCase True name
    go (UnionType (Union name fields _ docstring _)) =
        hang 4
          (docstring $$
              text "data" <+> typeName name <$>
              encloseSep (text "= ") empty (text " | ")
                         (map renderField fields))
        <$$> indent 4 derivingClause
      where
        structName = underscoresToCamelCase False name
        renderField (Field _ _ ftype fname _ _ docstring _) =
            docstring $$ fieldName </> renderTypeReference ftype
          where
            fieldName = text . unpack $ Text.concat [
                structName
              , underscoresToCamelCase False fname
              ]
    go t = error $ "Unsupported type: " ++ show t

typeName :: Text -> Doc
typeName = mkName False

generateOutput :: Show a => Program a -> IO ()
generateOutput (Program _ definitions) = do
    let doc = headers <$> empty <$>
              vcat (map ((<$> empty) . genDef) definitions)
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

    genDef :: Show a => Definition a -> Doc
    genDef (ConstDefinition (Const fieldType name value docstring _)) =
        docstring $$
            sep [fieldName name, text "::", renderTypeReference fieldType] <$>
            sep [fieldName name, text "=", renderConstValue value]
    genDef (TypeDefinition typeDef) = renderType typeDef
    genDef (ServiceDefinition (Service sname Nothing funcs _ docstring _)) =
        docstring $$
            text "data" <+> typeName sname <+> text "a where" <$$>
                indent 2 (vcat (map renderFunc funcs))
      where
        renderFunc (Function False rtype name params _ _ docstring _) =
          docstring $$
            typeName name <+> text "::" <>
            (if null params
              then space
              else linebreak <> renderParams name params) <>
            typeName sname <+> returnType
            <> linebreak
          where
            returnType = case rtype of
                Nothing -> text "()"
                Just t  -> renderTypeReference t

        renderParams fname params = indent 2 $
            encloseSep (text "{ ") (line <> text "} -> ") (text ", ") $
                map (renderStructField structName) params
          where
            structName = underscoresToCamelCase True fname

    fieldName = mkName True

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
    result <- getContents <&> parseString thriftIDL (Directed "stdin" 0 0 0 0)
    case result of
        Success p -> generateOutput p
        Failure doc -> do
            AnsiPP.displayIO stderr $ AnsiPP.renderPretty 0.8 80 doc
            die "Parse Failed"
