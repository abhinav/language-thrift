{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      :  Language.Thrift.Pretty
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module provides a pretty printer for Thrift IDLs. The pretty printer
-- prserves docstrings specified for types.
--
-- The specifics of the printer can be configured using 'Config' objects.
--
module Language.Thrift.Pretty
    (
      prettyPrint

    -- * Components

    , program
    , header
    , definition
    , function
    , fieldType
    , constantValue

    -- * Configuration

    , Config(..)
    , defaultConfig
    ) where

#if __GLASGOW_HASKELL__ >= 709
import Prelude hiding ((<$>))
#endif

import Data.Text               (Text, unpack)
import Text.PrettyPrint.Leijen hiding (encloseSep, indent, text)

import qualified Data.Text               as Text
import qualified Language.Thrift.Types   as T
import qualified Text.PrettyPrint.Leijen as PP


-- | Configuration for the pretty printer.
data Config = Config
    { indentWidth :: Int
    -- ^ Number of spaces to use for indentation.
    } deriving (Show, Ord, Eq)


-- | Default pretty printing configuration.
defaultConfig :: Config
defaultConfig = Config 4


-- | Top-level pretty printer for Thrift documents that uses the default
-- configuration ('defaultConfig') for pretty printing.
prettyPrint :: T.Program ann -> Doc
prettyPrint = program defaultConfig


-- | Pretty print a Thrift IDL.
program :: Config -> T.Program ann -> Doc
program c T.Program{..} =
    vsep (map header programHeaders) <$> line <>
    map (definition c) programDefinitions `sepBy` (line <> line)


-- | Print the headers for a program.
header :: T.Header ann -> Doc
header T.Include{..} =
    text "include" <+> literal includePath
header T.Namespace{..} = hsep
    [text "namespace", text namespaceLanguage, text namespaceName]


-- | Print a constant, type, or service definition.
definition :: Config -> T.Definition ann -> Doc
definition c T.ConstDefinition{..} = constDocstring $$ hsep
    [ text "const"
    , fieldType c constType
    , text constName
    , text "="
    , constantValue c constValue
    ]

definition c T.TypeDefinition{typeDefinition = def, ..} =
    typeDefinition c def <> typeAnnots c typeAnnotations

definition c@Config{indentWidth} T.ServiceDefinition{..} =
  serviceDocstring $$
    text "service" <+> text serviceName <> extends <+>
    block indentWidth (line <> line) (map (function c) serviceFunctions) <>
    typeAnnots c serviceAnnotations
  where
    extends = case serviceExtends of
      Nothing -> empty
      Just name -> space <> text "extends" <+> text name


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
      Just rt -> fieldType c rt
    oneway =
      if functionOneWay
          then text "oneway" <> space
          else empty


typeDefinition :: Config -> T.Type ann -> Doc
typeDefinition c@Config{indentWidth} t = case t of
  T.Typedef{..} -> typedefDocstring $$
    text "typedef" <+> fieldType c typedefType <+> text typedefName
  T.Enum{..} -> enumDocstring $$
    text "enum" <+> text enumName <+>
      block indentWidth (comma <> line) (map (enumValue c) enumValues)
  T.Struct{..} -> structDocstring $$
    text "struct" <+> text structName <+>
      block indentWidth line (map (\f -> field c f <> semi) structFields)
  T.Union{..} -> unionDocstring $$
    text "union" <+> text unionName <+>
      block indentWidth line (map (\f -> field c f <> semi) unionFields)
  T.Exception{..} -> exceptionDocstring $$
    text "exception" <+> text exceptionName <+>
      block indentWidth line (map (\f -> field c f <> semi) exceptionFields)
  T.Senum{..} -> senumDocstring $$
    text "senum" <+> text senumName <+>
      encloseSep indentWidth lbrace rbrace comma (map literal senumValues)


field :: Config -> T.Field ann -> Doc
field c T.Field{fieldType = typ, ..} = fieldDocstring $$
    hcat [fid, req, fieldType c typ, space, text fieldName, def, annots]
  where
    fid = case fieldIdentifier of
      Nothing -> empty
      Just i -> integer i <> colon <> space
    req = case fieldRequiredness of
      Nothing -> empty
      Just T.Optional -> text "optional "
      Just T.Required -> text "required "
    def = case fieldDefault of
      Nothing -> empty
      Just v -> space <> equals <+> constantValue c v
    annots = typeAnnots c fieldAnnotations


enumValue :: Config -> T.EnumDef ann -> Doc
enumValue c T.EnumDef{..} = enumDefDocstring $$
    text enumDefName <> value <> typeAnnots c enumDefAnnotations
  where
    value = case enumDefValue of
      Nothing -> empty
      Just v  -> space <> text "=" <+> integer v


-- | Pretty print a field type.
fieldType :: Config -> T.FieldType ann -> Doc
fieldType c ft = case ft of
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
    text "map" <> angles (fieldType c k <> comma <+> fieldType c v)
               <> typeAnnots c anns
  T.SetType v anns ->
    text "set" <> angles (fieldType c v) <> typeAnnots c anns
  T.ListType v anns ->
    text "list" <> angles (fieldType c v) <> typeAnnots c anns


-- | Pretty print a constant value.
constantValue :: Config -> T.ConstValue ann -> Doc
constantValue c@Config{indentWidth} value = case value of
  T.ConstInt i -> integer i
  T.ConstFloat f -> double f
  T.ConstLiteral l -> literal l
  T.ConstIdentifier i _ -> text i

  T.ConstList vs ->
    encloseSep indentWidth lbracket rbracket comma $
        map (constantValue c) vs
  T.ConstMap vs ->
    encloseSep indentWidth lbrace rbrace comma $
      map (\(k, v) -> constantValue c k <> colon <+> constantValue c v)
          vs


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


literal :: Text -> Doc
literal = dquotes . text
    -- TODO: escaping?

text :: Text -> Doc
text = PP.text . unpack


($$) :: T.Docstring -> Doc -> Doc
($$) Nothing y = y
($$) (Just t) y = case Text.lines (Text.strip t) of
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
  where go [x] = x
        go (x:xs) = (x <> s) <$> go xs
