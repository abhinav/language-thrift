{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Language.Thrift.Internal.Types
    (
    -- * AST

      Program(..)
    , headers
    , definitions

    , Header(..)
    , _Include
    , _Namespace

    , Include(..)
    , path

    , Namespace(..)
    , language

    , Definition(..)
    , _Const
    , _Service
    , _Type

    , Const(..)
    , Service(..)
    , functions
    , extends

    , Type(..)
    , _Typedef
    , _Enum
    , _Struct
    , _Union
    , _Exception
    , _Senum

    , Typedef(..)
    , targetType

    , Enum(..)
    , Struct(..)
    , Union(..)
    , Exception(..)
    , Senum(..)

    , FieldRequiredness(..)
    , _Required
    , _Optional

    , Field(..)
    , identifier
    , requiredness
    , defaultValue

    , EnumDef(..)

    , ConstValue(..)
    , _ConstInt
    , _ConstFloat
    , _ConstLiteral
    , _ConstIdentifier
    , _ConstList
    , _ConstMap

    , TypeReference(..)
    , _DefinedType
    , _StringType
    , _BinaryType
    , _SListType
    , _BoolType
    , _ByteType
    , _I16Type
    , _I32Type
    , _I64Type
    , _DoubleType
    , _MapType
    , _SetType
    , _ListType

    , Function(..)
    , oneWay
    , returnType
    , parameters
    , exceptions

    , TypeAnnotation(..)
    , Docstring

    -- * Typeclasses

    , HasAnnotations(..)
    , HasDocstring(..)
    , HasFields(..)
    , HasName(..)
    , HasSrcAnnot(..)
    , HasValue(..)
    , HasValues(..)
    , HasValueType(..)
    ) where

import Data.Data    (Data, Typeable)
import Data.Text    (Text)
import GHC.Generics (Generic)
import Prelude      hiding (Enum)

import Language.Thrift.Internal.TH

import qualified Control.Lens as L

class HasSrcAnnot t where
    srcAnnot :: L.Lens' (t a) a

class HasName t where
    name :: L.Lens' t Text

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

makeFieldsFor ["typeAnnotationValue"] ''TypeAnnotation

instance HasName TypeAnnotation where
    name = $(accessorLens 'typeAnnotationName)

class HasAnnotations t where
    annotations :: L.Lens' t [TypeAnnotation]

-- | Docstrings are Javadoc-style comments attached various defined objects.
--
-- > /**
-- >  * Fetches an item.
-- >  */
-- > Item getItem()
type Docstring = Maybe Text

class HasDocstring t where
    docstring :: L.Lens' t Docstring

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

L.makePrisms ''ConstValue


-- | A reference to a type.
data TypeReference srcAnnot
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
    | MapType
        (TypeReference srcAnnot)
        (TypeReference srcAnnot)
        [TypeAnnotation]
    -- ^ @map\<foo, bar\>@ and annotations.
    | SetType (TypeReference srcAnnot) [TypeAnnotation]
    -- ^ @set\<baz\>@ and annotations.
    | ListType (TypeReference srcAnnot) [TypeAnnotation]
    -- ^ @list\<qux\>@ and annotations.
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

L.makePrisms ''TypeReference

class HasValueType t where
    valueType :: L.Lens' (t a) (TypeReference a)


-- | Whether a field is required or optional.
data FieldRequiredness
    = Required -- ^ The field is @required@.
    | Optional -- ^ The field is @optional@.
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

L.makePrisms ''FieldRequiredness

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
    , fieldValueType    :: TypeReference srcAnnot
    -- ^ Type of value the field holds.
    , fieldName         :: Text
    -- ^ Name of the field.
    , fieldDefaultValue :: Maybe (ConstValue srcAnnot)
    -- ^ Default value of the field, if any.
    , fieldAnnotations  :: [TypeAnnotation]
    -- ^ Field annotations.
    , fieldDocstring    :: Docstring
    -- ^ Documentation.
    , fieldSrcAnnot     :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

L.makeLensesFor
    [ ("fieldIdentifier", "identifier")
    , ("fieldRequiredness", "requiredness")
    , ("fieldDefaultValue", "defaultValue")
    ] ''Field

instance HasName (Field a) where
    name = $(accessorLens 'fieldName)

instance HasValueType Field where
    valueType = $(accessorLens 'fieldValueType)

instance HasSrcAnnot Field where
    srcAnnot = $(accessorLens 'fieldSrcAnnot)

instance HasDocstring (Field a) where
    docstring = $(accessorLens 'fieldDocstring)

instance HasAnnotations (Field a) where
    annotations = $(accessorLens 'fieldAnnotations)

class HasFields t where
    fields :: L.Lens' (t a) [Field a]

-- | A function defined inside a service.
data Function srcAnnot = Function
    { functionOneWay      :: Bool
    -- ^ Whether the function is @oneway@. If it's one way, it cannot receive
    -- repsonses.
    , functionReturnType  :: Maybe (TypeReference srcAnnot)
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

L.makeLensesFor
    [ ("functionOneWay", "oneWay")
    , ("functionReturnType", "returnType")
    , ("functionParameters", "parameters")
    , ("functionExceptions", "exceptions")
    ] ''Function

instance HasName (Function a) where
    name = $(accessorLens 'functionName)

instance HasSrcAnnot Function where
    srcAnnot = $(accessorLens 'functionSrcAnnot)

instance HasDocstring (Function a) where
    docstring = $(accessorLens 'functionDocstring)

instance HasAnnotations (Function a) where
    annotations = $(accessorLens 'functionAnnotations)

-- | A service definition.
--
-- > service MyService {
-- >     // ...
-- > }
data Service srcAnnot = Service
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

L.makeLensesFor
    [ ("serviceExtends", "extends")
    , ("serviceFunctions", "functions")
    ] ''Service

instance HasName (Service a) where
    name = $(accessorLens 'serviceName)

instance HasSrcAnnot Service where
    srcAnnot = $(accessorLens 'serviceSrcAnnot)

instance HasDocstring (Service a) where
    docstring = $(accessorLens 'serviceDocstring)

instance HasAnnotations (Service a) where
    annotations = $(accessorLens 'serviceAnnotations)

-- | A declared constant.
--
-- > const i32 code = 1;
data Const srcAnnot = Const
    { constValueType :: TypeReference srcAnnot
    -- ^ Type of the constant.
    , constName      :: Text
    -- ^ Name of the constant.
    , constValue     :: ConstValue srcAnnot
    -- ^ Value of the constant.
    , constDocstring :: Docstring
    -- ^ Documentation.
    , constSrcAnnot  :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

makeFieldsFor ["constValue"] ''Const

instance HasName (Const a) where
    name = $(accessorLens 'constName)

instance HasSrcAnnot Const where
    srcAnnot = $(accessorLens 'constSrcAnnot)

instance HasValueType Const where
    valueType = $(accessorLens 'constValueType)

instance HasDocstring (Const a) where
    docstring = $(accessorLens 'constDocstring)

-- | A typedef is just an alias for another type.
--
-- > typedef common.Foo Bar
data Typedef srcAnnot = Typedef
    { typedefTargetType  :: TypeReference srcAnnot
    -- ^ The aliased type.
    , typedefName        :: Text
    -- ^ Name of the new type.
    , typedefAnnotations :: [TypeAnnotation]
    -- ^ Annotations added to the typedef.
    , typedefDocstring   :: Docstring
    -- ^ Documentation.
    , typedefSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

L.makeLensesFor [("typedefTargetType", "targetType")] ''Typedef

instance HasName (Typedef a) where
    name = $(accessorLens 'typedefName)

instance HasSrcAnnot Typedef where
    srcAnnot = $(accessorLens 'typedefSrcAnnot)

instance HasDocstring (Typedef a) where
    docstring = $(accessorLens 'typedefDocstring)

instance HasAnnotations (Typedef a) where
    annotations = $(accessorLens 'typedefAnnotations)

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

makeFieldsFor ["enumDefValue"] ''EnumDef

instance HasName (EnumDef a) where
    name = $(accessorLens 'enumDefName)

instance HasSrcAnnot EnumDef where
    srcAnnot = $(accessorLens 'enumDefSrcAnnot)

instance HasDocstring (EnumDef a) where
    docstring = $(accessorLens 'enumDefDocstring)

instance HasAnnotations (EnumDef a) where
    annotations = $(accessorLens 'enumDefAnnotations)

-- | Enums are sets of named integer values.
--
-- > enum Role {
-- >     User = 1, Admin = 2
-- > }
data Enum srcAnnot = Enum
    { enumName        :: Text
    -- ^ Name of the enum type.
    , enumValues      :: [EnumDef srcAnnot]
    -- ^ Values defined in the enum.
    , enumAnnotations :: [TypeAnnotation]
    -- ^ Annotations added to the enum.
    , enumDocstring   :: Docstring
    -- ^ Documentation.
    , enumSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

makeFieldsFor ["enumValues"] ''Enum

instance HasName (Enum a) where
    name = $(accessorLens 'enumName)

instance HasSrcAnnot Enum where
    srcAnnot = $(accessorLens 'enumSrcAnnot)

instance HasDocstring (Enum a) where
    docstring = $(accessorLens 'enumDocstring)

instance HasAnnotations (Enum a) where
    annotations = $(accessorLens 'enumAnnotations)

-- | A struct definition
--
-- > struct User {
-- >     1: Role role = Role.User;
-- > }
data Struct srcAnnot = Struct
    { structName        :: Text
    -- ^ Name of the struct.
    , structFields      :: [Field srcAnnot]
    -- ^ Fields defined in the struct.
    , structAnnotations :: [TypeAnnotation]
    -- ^ Annotations added to the struct.
    , structDocstring   :: Docstring
    -- ^ Documentation.
    , structSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

instance HasName (Struct a) where
    name = $(accessorLens 'structName)

instance HasFields Struct where
    fields = $(accessorLens 'structFields)

instance HasSrcAnnot Struct where
    srcAnnot = $(accessorLens 'structSrcAnnot)

instance HasDocstring (Struct a) where
    docstring = $(accessorLens 'structDocstring)

instance HasAnnotations (Struct a) where
    annotations = $(accessorLens 'structAnnotations)

-- | A union of other types.
--
-- > union Value {
-- >     1: string stringValue;
-- >     2: i32 intValue;
-- > }
data Union srcAnnot = Union
    { unionName        :: Text
    -- ^ Name of the union.
    , unionFields      :: [Field srcAnnot]
    -- ^ Fields defined in the union.
    , unionAnnotations :: [TypeAnnotation]
    -- ^ Annotations added to the union.
    , unionDocstring   :: Docstring
    -- ^ Documentation.
    , unionSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

instance HasName (Union a) where
    name = $(accessorLens 'unionName)

instance HasFields Union where
    fields = $(accessorLens 'unionFields)

instance HasSrcAnnot Union where
    srcAnnot = $(accessorLens 'unionSrcAnnot)

instance HasDocstring (Union a) where
    docstring = $(accessorLens 'unionDocstring)

instance HasAnnotations (Union a) where
    annotations = $(accessorLens 'unionAnnotations)

-- | Exception types.
--
-- > exception UserDoesNotExist {
-- >     1: optional string message
-- >     2: required string username
-- > }
data Exception srcAnnot = Exception
    { exceptionName        :: Text
    -- ^ Name of the exception.
    , exceptionFields      :: [Field srcAnnot]
    -- ^ Fields defined in the exception.
    , exceptionAnnotations :: [TypeAnnotation]
    -- ^ Annotations added to the exception.
    , exceptionDocstring   :: Docstring
    -- ^ Documentation.
    , exceptionSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

instance HasName (Exception a) where
    name = $(accessorLens 'exceptionName)

instance HasFields Exception where
    fields = $(accessorLens 'exceptionFields)

instance HasSrcAnnot Exception where
    srcAnnot = $(accessorLens 'exceptionSrcAnnot)

instance HasDocstring (Exception a) where
    docstring = $(accessorLens 'exceptionDocstring)

instance HasAnnotations (Exception a) where
    annotations = $(accessorLens 'exceptionAnnotations)

-- | An string-only enum. These are a deprecated feature of Thrift and
-- shouldn't be used.
data Senum srcAnnot = Senum
    { senumName        :: Text
    , senumValues      :: [Text]
    , senumAnnotations :: [TypeAnnotation]
    -- ^ Annotations added to the senum.
    , senumDocstring   :: Docstring
    -- ^ Documentation.
    , senumSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

makeFieldsFor ["senumValues"] ''Senum

instance HasName (Senum a) where
    name = $(accessorLens 'senumName)

instance HasSrcAnnot Senum where
    srcAnnot = $(accessorLens 'senumSrcAnnot)

instance HasDocstring (Senum a) where
    docstring = $(accessorLens 'senumDocstring)

instance HasAnnotations (Senum a) where
    annotations = $(accessorLens 'senumAnnotations)

-- | Defines the various types that can be declared in Thrift.
data Type srcAnnot
    = -- | @typedef@
      TypedefType (Typedef srcAnnot)
    | -- | @enum@
      EnumType (Enum srcAnnot)
    | -- | @struct@
      StructType (Struct srcAnnot)
    | -- | @union@
      UnionType (Union srcAnnot)
    | -- | @exception@
      ExceptionType (Exception srcAnnot)
    | -- | @senum@
      SenumType (Senum srcAnnot)
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

_Typedef :: L.Prism' (Type ann) (Typedef ann)
_Typedef = L.prism' TypedefType $ \t ->
    case t of
        TypedefType a -> Just a
        _             -> Nothing

_Enum :: L.Prism' (Type ann) (Enum ann)
_Enum = L.prism' EnumType $ \t ->
    case t of
        EnumType a -> Just a
        _          -> Nothing

_Struct :: L.Prism' (Type ann) (Struct ann)
_Struct = L.prism' StructType $ \t ->
    case t of
        StructType a -> Just a
        _            -> Nothing

_Union :: L.Prism' (Type ann) (Union ann)
_Union = L.prism' UnionType $ \t ->
    case t of
        UnionType a -> Just a
        _           -> Nothing

_Exception :: L.Prism' (Type ann) (Exception ann)
_Exception = L.prism' ExceptionType $ \t ->
    case t of
        ExceptionType a -> Just a
        _               -> Nothing

_Senum :: L.Prism' (Type ann) (Senum ann)
_Senum = L.prism' SenumType $ \t ->
    case t of
        SenumType a -> Just a
        _           -> Nothing


-- | A definition either consists of new constants, new types, or new
-- services.
data Definition srcAnnot
    = -- | A declared constant.
      ConstDefinition (Const srcAnnot)
    | -- | A custom type.
      TypeDefinition (Type srcAnnot)
    | -- | A service definition.
      ServiceDefinition (Service srcAnnot)
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

_Const :: L.Prism' (Definition ann) (Const ann)
_Const = L.prism' ConstDefinition $ \def ->
    case def of
        ConstDefinition c -> Just c
        _                 -> Nothing

_Type :: L.Prism' (Definition ann) (Type ann)
_Type = L.prism' TypeDefinition $ \def ->
    case def of
        TypeDefinition c -> Just c
        _                 -> Nothing

_Service :: L.Prism' (Definition ann) (Service ann)
_Service = L.prism' ServiceDefinition $ \def ->
    case def of
        ServiceDefinition c -> Just c
        _                 -> Nothing


-- | Namespace directives allows control of the namespace or package
-- name used by the generated code for certain languages.
--
-- > namespace py my_service.generated
data Namespace srcAnnot = Namespace
    { namespaceLanguage :: Text
    -- ^ The language for which the namespace is being specified. This may
    -- be @*@ to refer to all languages.
    , namespaceName     :: Text
    -- ^ Namespace or package path to use in the generated code for that
    -- language.
    , namespaceSrcAnnot :: srcAnnot
    }
    deriving (Show, Ord, Eq, Data, Typeable, Generic)

L.makeLensesFor [("namespaceLanguage", "language")] ''Namespace

instance HasName (Namespace a) where
    name = $(accessorLens 'namespaceName)

instance HasSrcAnnot Namespace where
    srcAnnot = $(accessorLens 'namespaceSrcAnnot)

-- | The IDL includes another Thrift file.
--
-- > include "common.thrift"
-- >
-- > typedef common.Foo Bar
--
data Include srcAnnot = Include
    { includePath     :: Text
    -- ^ Path to the included file.
    , includeSrcAnnot :: srcAnnot
    }
    deriving (Show, Ord, Eq, Data, Typeable, Generic)

L.makeLensesFor [("includePath", "path")] ''Include

instance HasSrcAnnot Include where
    srcAnnot = $(accessorLens 'includeSrcAnnot)

-- | Headers for a program.
data Header srcAnnot
    = -- | Request to include another Thrift file.
      HeaderInclude (Include srcAnnot)
    | -- | A @namespace@ specifier.
      HeaderNamespace (Namespace srcAnnot)
    deriving (Show, Ord, Eq, Data, Typeable, Generic)

_Include :: L.Prism' (Header ann) (Include ann)
_Include = L.prism' HeaderInclude $ \h ->
    case h of
        HeaderInclude inc -> Just inc
        _                 -> Nothing

_Namespace :: L.Prism' (Header ann) (Namespace ann)
_Namespace = L.prism' HeaderNamespace $ \h ->
    case h of
        HeaderNamespace ns -> Just ns
        _                  -> Nothing


-- | A program represents a single Thrift document.
data Program srcAnnot = Program
    { programHeaders     :: [Header srcAnnot]
    -- ^ Headers in a document define includes and namespaces.
    , programDefinitions :: [Definition srcAnnot]
    -- ^ Types and services defined in the document.
    }
    deriving (Show, Ord, Eq, Data, Typeable, Generic)

L.makeLensesFor
    [ ("programHeaders", "headers")
    , ("programDefinitions", "definitions")
    ] ''Program
