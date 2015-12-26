{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- |
-- Module      :  Language.Thrift.Types
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module defines types that compose a Thrift IDL file.
--
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

-- | A program represents a single Thrift document.
data Program srcAnnot = Program
    { programHeaders     :: [Header srcAnnot]
    -- ^ Headers in a document define includes and namespaces.
    , programDefinitions :: [Definition srcAnnot]
    -- ^ Types and services defined in the document.
    }
    deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | Headers for a program.
data Header srcAnnot
    = -- | The IDL includes another Thrift file.
      --
      -- > include "common.thrift"
      -- >
      -- > typedef common.Foo Bar
      --
      Include
        { includePath :: Text
        -- ^ Path to the included file.
        , includeSrcAnnot :: srcAnnot
        }
    | -- | Namespace directives allows control of the namespace or package
      -- name used by the generated code for certain languages.
      --
      -- > namespace py my_service.generated
      Namespace
        { namespaceLanguage :: Text
        -- ^ The language for which the namespace is being specified. This may
        -- be @*@ to refer to all languages.
        , namespaceName     :: Text
        -- ^ Namespace or package path to use in the generated code for that
        -- language.
        , namespaceSrcAnnot :: srcAnnot
        }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | A definition either consists of new constants, new types, or new
-- services.
data Definition srcAnnot
    = -- | A declared constant.
      --
      -- > const i32 code = 1;
      ConstDefinition
        { constType      :: FieldType srcAnnot
        -- ^ Type of the constant.
        , constName      :: Text
        -- ^ Name of the constant.
        , constValue     :: ConstValue srcAnnot
        -- ^ Value of the constant.
        , constDocstring :: Docstring
        -- ^ Documentation.
        , constSrcAnnot  :: srcAnnot
        }
    | -- | A declared type.
      TypeDefinition
        { typeDefinition  :: Type srcAnnot
        -- ^ Details of the type definition.
        , typeAnnotations :: [TypeAnnotation]
        -- ^ Annotations added to the type.
        }
    | -- | A service definition.
      --
      -- > service MyService {
      -- >     // ...
      -- > }
      ServiceDefinition
        { serviceName        :: Text
        -- ^ Name of the service.
        , serviceExtends     :: Maybe Text
        -- ^ Name of the service this service extends.
        , serviceFunctions   :: [Function srcAnnot]
        -- ^ All the functions defined for the service.
        , serviceAnnotations :: [TypeAnnotation]
        -- ^ Annotations added to the service.
        , serviceDocstring   :: Docstring
        -- ^ Documentation.
        , serviceSrcAnnot    :: srcAnnot
        }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | Defines the various types that can be declared in Thrift.
data Type srcAnnot
    = -- | A typedef is just an alias for another type.
      --
      -- > typedef common.Foo Bar
      Typedef
        { typedefType      :: FieldType srcAnnot
        -- ^ The aliased type.
        , typedefName      :: Text
        -- ^ Name of the new type.
        , typedefDocstring :: Docstring
        -- ^ Documentation.
        , typedefSrcAnnot  :: srcAnnot
        }
    | -- | Enums are sets of named integer values.
      --
      -- > enum Role {
      -- >     User = 1, Admin = 2
      -- > }
      Enum
        { enumName      :: Text
        -- ^ Name of the enum type.
        , enumValues    :: [EnumDef srcAnnot]
        -- ^ Values defined in the enum.
        , enumDocstring :: Docstring
        -- ^ Documentation.
        , enumSrcAnnot  :: srcAnnot
        }
    | -- | A struct definition
      --
      -- > struct User {
      -- >     1: Role role = Role.User;
      -- > }
      Struct
        { structName      :: Text
        -- ^ Name of the struct.
        , structFields    :: [Field srcAnnot]
        -- ^ Fields defined in the struct.
        , structDocstring :: Docstring
        -- ^ Documentation.
        , structSrcAnnot  :: srcAnnot
        }
    | -- | A union of other types.
      --
      -- > union Value {
      -- >     1: string stringValue;
      -- >     2: i32 intValue;
      -- > }
      Union
        { unionName      :: Text
        -- ^ Name of the union.
        , unionFields    :: [Field srcAnnot]
        -- ^ Fields defined in the union.
        , unionDocstring :: Docstring
        -- ^ Documentation.
        , unionSrcAnnot  :: srcAnnot
        }
    | -- | Exception types.
      --
      -- > exception UserDoesNotExist {
      -- >     1: optional string message
      -- >     2: required string username
      -- > }
      Exception
        { exceptionName      :: Text
        , exceptionFields    :: [Field srcAnnot]
        , exceptionDocstring :: Docstring
        , exceptionSrcAnnot  :: srcAnnot
        }
    | -- | An string-only enum. These are a deprecated feature of Thrift and
      -- shouldn't be used.
      Senum
        { senumName      :: Text
        , senumValues    :: [Text]
        , senumDocstring :: Docstring
        -- ^ Documentation.
        , senumSrcAnnot  :: srcAnnot
        }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | Whether a field is required or optional.
data FieldRequiredness
    = Required -- ^ The field is @required@.
    | Optional -- ^ The field is @optional@.
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | A field inside a struct, exception, or function parameters list.
data Field srcAnnot = Field
    { fieldIdentifier   :: Maybe Integer
    -- ^ Position of the field.
    --
    -- While this is optional, it is recommended that Thrift files always
    -- contain specific field IDs.
    , fieldRequiredness :: Maybe FieldRequiredness
    -- ^ Whether this field is required or not.
    --
    -- Behavior may differ between languages if requiredness is not specified.
    -- Therefore it's recommended that requiredness for a field is always
    -- specified.
    , fieldType         :: FieldType srcAnnot
    -- ^ Type of value the field holds.
    , fieldName         :: Text
    -- ^ Name of the field.
    , fieldDefault      :: Maybe (ConstValue srcAnnot)
    -- ^ Default value of the field, if any.
    , fieldAnnotations  :: [TypeAnnotation]
    -- ^ Field annotations.
    , fieldDocstring    :: Docstring
    -- ^ Documentation.
    , fieldSrcAnnot     :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | A named value inside an enum.
data EnumDef srcAnnot = EnumDef
    { enumDefName        :: Text
    -- ^ Name of the value.
    , enumDefValue       :: Maybe Integer
    -- ^ Value attached to the enum for that name.
    , enumDefAnnotations :: [TypeAnnotation]
    -- ^ Annotations added to this enum field.
    , enumDefDocstring   :: Docstring
    -- ^ Documentation
    , enumDefSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | A constant literal value in the IDL. Only a few basic types, lists, and
-- maps can be presented in Thrift files as literals.
--
-- Constants are used for IDL-level constants and default values for fields.
data ConstValue srcAnnot
    = ConstInt Integer
    -- ^ An integer. @42@
    | ConstFloat Double
    -- ^ A float. @4.2@
    | ConstLiteral Text
    -- ^ A literal string. @"hello"@
    | ConstIdentifier Text srcAnnot
    -- ^ A reference to another constant. @Foo.bar@
    | ConstList [ConstValue srcAnnot]
    -- ^ A literal list containing other constant values. @[42]@
    | ConstMap [(ConstValue srcAnnot, ConstValue srcAnnot)]
    -- ^ A literal list containing other constant values.
    -- @{"hellO": 1, "world": 2}@
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | A reference to a type.
data FieldType srcAnnot
    = DefinedType Text srcAnnot
    -- ^ A custom defined type referred to by name.

    | StringType [TypeAnnotation]
    -- ^ @string@ and annotations.
    | BinaryType [TypeAnnotation]
    -- ^ @binary@ and annotations.
    | SListType [TypeAnnotation]
    -- ^ @slist@ and annotations.
    | BoolType [TypeAnnotation]
    -- ^ @bool@ and annotations.
    | ByteType [TypeAnnotation]
    -- ^ @byte@ and annotations.
    | I16Type [TypeAnnotation]
    -- ^ @i16@ and annotations.
    | I32Type [TypeAnnotation]
    -- ^ @i32@ and annotations.
    | I64Type [TypeAnnotation]
    -- ^ @i64@ and annotations.
    | DoubleType [TypeAnnotation]
    -- ^ @double@ and annotations.

    -- Container types
    | MapType (FieldType srcAnnot) (FieldType srcAnnot) [TypeAnnotation]
    -- ^ @map\<foo, bar\>@ and annotations.
    | SetType (FieldType srcAnnot) [TypeAnnotation]
    -- ^ @set\<baz\>@ and annotations.
    | ListType (FieldType srcAnnot) [TypeAnnotation]
    -- ^ @list\<qux\>@ and annotations.
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | A function defined inside a service.
data Function srcAnnot = Function
    { functionOneWay      :: Bool
    -- ^ Whether the function is @oneway@. If it's one way, it cannot receive
    -- repsonses.
    , functionReturnType  :: Maybe (FieldType srcAnnot)
    -- ^ Return type of the function, or @Nothing@ if it's @void@ or @oneway@.
    , functionName        :: Text
    -- ^ Name of the function.
    , functionParameters  :: [Field srcAnnot]
    -- ^ Parameters accepted by the function.
    , functionExceptions  :: Maybe [Field srcAnnot]
    -- ^ Exceptions raised by the function, if any.
    , functionAnnotations :: [TypeAnnotation]
    -- ^ Annotations added to the function.
    , functionDocstring   :: Docstring
    -- ^ Documentation.
    , functionSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | Type annoations may be added in various places in the form,
--
-- > (foo = "bar", baz, qux = "quux")
--
-- These do not usually affect code generation but allow for custom logic if
-- writing your own code generator.
data TypeAnnotation = TypeAnnotation
    { typeAnnotationName  :: Text
    -- ^ Name of the annotation.
    , typeAnnotationValue :: Maybe Text
    -- ^ Value for the annotation.
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | Docstrings are Javadoc-style comments attached various defined objects.
--
-- > /**
-- >  * Fetches an item.
-- >  */
-- > Item getItem()
type Docstring = Maybe Text
