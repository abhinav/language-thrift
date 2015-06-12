{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Language.Thrift.Types
    ( Program(..)
    , Header(..)
    , Definition(..)
    , Type(..)
    , FieldRequiredness(..)
    , Field(..)
    , EnumDef(..)
    , ConstValue(..)
    , FieldType(..)
    , Function(..)
    , TypeAnnotation(..)
    , Docstring
    ) where

import Data.Data    (Data, Typeable)
import Data.Text    (Text)
import GHC.Generics (Generic)

data Program srcAnnot = Program
    { programHeaders     :: [Header]
    , programDefinitions :: [Definition srcAnnot]
    }
    deriving (Show, Ord, Eq, Data, Typeable, Generic)

data Header
    = Include    { includePath :: Text }
    | Namespace  { namespaceLanguage, namespaceName :: Text }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

data Definition srcAnnot
    = ConstDefinition
        { constType      :: FieldType
        , constName      :: Text
        , constValue     :: ConstValue
        , constDocstring :: Docstring
        , constSrcAnnot  :: srcAnnot
        }
    | TypeDefinition
        { typeDefinition  :: Type srcAnnot
        , typeAnnotations :: [TypeAnnotation]
        }
    | ServiceDefinition
        { serviceName        :: Text
        , serviceExtends     :: Maybe Text
        , serviceFunctions   :: [Function srcAnnot]
        , serviceAnnotations :: [TypeAnnotation]
        , serviceDocstring   :: Docstring
        , serviceSrcAnnot    :: srcAnnot
        }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

data Type srcAnnot
    = Typedef
        { typedefType      :: FieldType
        , typedefName      :: Text
        , typedefDocstring :: Docstring
        , typedefSrcAnnot  :: srcAnnot
        }
    | Enum
        { enumName      :: Text
        , enumValues    :: [EnumDef srcAnnot]
        , enumDocstring :: Docstring
        , enumSrcAnnot  :: srcAnnot
        }
    | Struct
        { structName      :: Text
        , structFields    :: [Field srcAnnot]
        , structDocstring :: Docstring
        , structSrcAnnot  :: srcAnnot
        }
    | Union
        { unionName      :: Text
        , unionFields    :: [Field srcAnnot]
        , unionDocstring :: Docstring
        , unionSrcAnnot  :: srcAnnot
        }
    | Exception
        { exceptionName      :: Text
        , exceptionFields    :: [Field srcAnnot]
        , exceptionDocstring :: Docstring
        , exceptionSrcAnnot  :: srcAnnot
        }
    | Senum
        { senumName      :: Text
        , senumValues    :: [Text]
        , senumDocstring :: Docstring
        , senumSrcAnnot  :: srcAnnot
        }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

data FieldRequiredness = Required | Optional
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

data Field srcAnnot = Field
    { fieldIdentifier   :: Maybe Integer
    , fieldRequiredNess :: Maybe FieldRequiredness
    , fieldType         :: FieldType
    , fieldName         :: Text
    , fieldDefault      :: Maybe ConstValue
    , fieldAnnotations  :: [TypeAnnotation]
    , fieldDocstring    :: Docstring
    , fieldSrcAnnot     :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

data EnumDef srcAnnot = EnumDef
    { enumDefName        :: Text
    , enumDefValue       :: Maybe Integer
    , enumDefAnnotations :: [TypeAnnotation]
    , enumDefDocstring   :: Docstring
    , enumDefSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

data ConstValue
    = ConstInt Integer
    | ConstFloat Double
    | ConstLiteral Text
    | ConstIdentifier Text
    | ConstList [ConstValue]
    | ConstMap [(ConstValue, ConstValue)]
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

data FieldType
    = DefinedType Text

    -- Base types
    | StringType [TypeAnnotation]
    | BinaryType [TypeAnnotation]
    | SListType [TypeAnnotation]
    | BoolType [TypeAnnotation]
    | ByteType [TypeAnnotation]
    | I16Type [TypeAnnotation]
    | I32Type [TypeAnnotation]
    | I64Type [TypeAnnotation]
    | DoubleType [TypeAnnotation]

    -- Container types
    | MapType FieldType FieldType [TypeAnnotation]
    | SetType FieldType [TypeAnnotation]
    | ListType FieldType [TypeAnnotation]
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

data Function srcAnnot = Function
    { functionOneWay      :: Bool
    , functionReturnType  :: Maybe FieldType
    , functionName        :: Text
    , functionParameters  :: [Field srcAnnot]
    , functionExceptions  :: Maybe [Field srcAnnot]
    , functionAnnotations :: [TypeAnnotation]
    , functionDocstring   :: Docstring
    , functionSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

data TypeAnnotation = TypeAnnotation
    { typeAnnotationName  :: Text
    , typeAnnotationValue :: Text
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

type Docstring = Maybe Text
