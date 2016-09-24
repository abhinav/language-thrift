{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
module Language.Thrift.Internal.AST
    (
    -- * AST

      Program(..)
    , headers
    , definitions

    , Header(..)

    , Include(..)
    , path

    , Namespace(..)
    , language

    , Definition(..)

    , Const(..)
    , Service(..)
    , functions
    , extends

    , Type(..)

    , Typedef(..)
    , targetType

    , Enum(..)

    , StructKind(..)
    , Struct(..)
    , kind

    , Union
    , unionName
    , unionFields
    , unionAnnotations
    , unionDocstring
    , unionSrcAnnot

    , Exception
    , exceptionName
    , exceptionFields
    , exceptionAnnotations
    , exceptionDocstring
    , exceptionSrcAnnot

    , Senum(..)

    , FieldRequiredness(..)

    , Field(..)
    , identifier
    , requiredness
    , defaultValue

    , EnumDef(..)

    , ConstValue(..)

    , TypeReference(..)

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

import Language.Thrift.Internal.Lens

class HasSrcAnnot t where
    srcAnnot :: Lens (t a) a

class HasName t where
    name :: Lens t Text

class HasValue s a | s -> a where
    value :: Lens s a

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

instance HasName TypeAnnotation where
    name = lens typeAnnotationName (\s a -> s { typeAnnotationName = a })

instance HasValue TypeAnnotation (Maybe Text) where
    value = lens typeAnnotationValue (\s a -> s { typeAnnotationValue = a })

class HasAnnotations t where
    annotations :: Lens t [TypeAnnotation]

-- | Docstrings are Javadoc-style comments attached various defined objects.
--
-- > /**
-- >  * Fetches an item.
-- >  */
-- > Item getItem()
type Docstring = Maybe Text

class HasDocstring t where
    docstring :: Lens t Docstring

-- | A constant literal value in the IDL. Only a few basic types, lists, and
-- maps can be presented in Thrift files as literals.
--
-- Constants are used for IDL-level constants and default values for fields.
data ConstValue srcAnnot
    = ConstInt Integer srcAnnot
    -- ^ An integer. @42@
    | ConstFloat Double srcAnnot
    -- ^ A float. @4.2@
    | ConstLiteral Text srcAnnot
    -- ^ A literal string. @"hello"@
    | ConstIdentifier Text srcAnnot
    -- ^ A reference to another constant. @Foo.bar@
    | ConstList [ConstValue srcAnnot] srcAnnot
    -- ^ A literal list containing other constant values. @[42]@
    | ConstMap [(ConstValue srcAnnot, ConstValue srcAnnot)] srcAnnot
    -- ^ A literal list containing other constant values.
    -- @{"hellO": 1, "world": 2}@
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

instance HasSrcAnnot ConstValue where
    srcAnnot = lens getter setter
      where
        getter (ConstInt        _ a) = a
        getter (ConstFloat      _ a) = a
        getter (ConstLiteral    _ a) = a
        getter (ConstIdentifier _ a) = a
        getter (ConstList       _ a) = a
        getter (ConstMap        _ a) = a

        setter (ConstInt        x _) a = ConstInt        x a
        setter (ConstFloat      x _) a = ConstFloat      x a
        setter (ConstLiteral    x _) a = ConstLiteral    x a
        setter (ConstIdentifier x _) a = ConstIdentifier x a
        setter (ConstList       x _) a = ConstList       x a
        setter (ConstMap        x _) a = ConstMap        x a

-- | A reference to a type.
data TypeReference srcAnnot
    = DefinedType Text srcAnnot
    -- ^ A custom defined type referred to by name.

    | StringType [TypeAnnotation] srcAnnot
    -- ^ @string@ and annotations.
    | BinaryType [TypeAnnotation] srcAnnot
    -- ^ @binary@ and annotations.
    | SListType [TypeAnnotation] srcAnnot
    -- ^ @slist@ and annotations.
    | BoolType [TypeAnnotation] srcAnnot
    -- ^ @bool@ and annotations.
    | ByteType [TypeAnnotation] srcAnnot
    -- ^ @byte@ and annotations.
    | I16Type [TypeAnnotation] srcAnnot
    -- ^ @i16@ and annotations.
    | I32Type [TypeAnnotation] srcAnnot
    -- ^ @i32@ and annotations.
    | I64Type [TypeAnnotation] srcAnnot
    -- ^ @i64@ and annotations.
    | DoubleType [TypeAnnotation] srcAnnot
    -- ^ @double@ and annotations.

    -- Container types
    | MapType
        (TypeReference srcAnnot)
        (TypeReference srcAnnot)
        [TypeAnnotation]
        srcAnnot
    -- ^ @map\<foo, bar\>@ and annotations.
    | SetType (TypeReference srcAnnot) [TypeAnnotation] srcAnnot
    -- ^ @set\<baz\>@ and annotations.
    | ListType (TypeReference srcAnnot) [TypeAnnotation] srcAnnot
    -- ^ @list\<qux\>@ and annotations.
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

instance HasSrcAnnot TypeReference where
    srcAnnot = lens getter setter
      where
        getter (DefinedType _ a) = a
        getter (StringType  _ a) = a
        getter (BinaryType  _ a) = a
        getter (SListType   _ a) = a
        getter (BoolType    _ a) = a
        getter (ByteType    _ a) = a
        getter (I16Type     _ a) = a
        getter (I32Type     _ a) = a
        getter (I64Type     _ a) = a
        getter (DoubleType  _ a) = a
        getter (MapType _ _ _ a) = a
        getter (SetType   _ _ a) = a
        getter (ListType  _ _ a) = a

        setter (DefinedType x _) a = DefinedType x a
        setter (StringType  x _) a = StringType  x a
        setter (BinaryType  x _) a = BinaryType  x a
        setter (SListType   x _) a = SListType   x a
        setter (BoolType    x _) a = BoolType    x a
        setter (ByteType    x _) a = ByteType    x a
        setter (I16Type     x _) a = I16Type     x a
        setter (I32Type     x _) a = I32Type     x a
        setter (I64Type     x _) a = I64Type     x a
        setter (DoubleType  x _) a = DoubleType  x a
        setter (MapType k v x _) a = MapType k v x a
        setter (SetType   t x _) a = SetType   t x a
        setter (ListType  t x _) a = ListType  t x a

class HasValueType t where
    valueType :: Lens (t a) (TypeReference a)

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
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

identifier :: Lens (Field a) (Maybe Integer)
identifier = lens fieldIdentifier (\s a -> s { fieldIdentifier = a })

requiredness :: Lens (Field a) (Maybe FieldRequiredness)
requiredness = lens fieldRequiredness (\s a -> s { fieldRequiredness = a })

defaultValue :: Lens (Field a) (Maybe (ConstValue a))
defaultValue = lens fieldDefaultValue (\s a -> s { fieldDefaultValue = a })

instance HasName (Field a) where
    name = lens fieldName (\s a -> s { fieldName = a })

instance HasValueType Field where
    valueType = lens fieldValueType (\s a -> s { fieldValueType = a })

instance HasSrcAnnot Field where
    srcAnnot = lens fieldSrcAnnot (\s a -> s { fieldSrcAnnot = a })

instance HasDocstring (Field a) where
    docstring = lens fieldDocstring (\s a -> s { fieldDocstring = a })

instance HasAnnotations (Field a) where
    annotations = lens fieldAnnotations (\s a -> s { fieldAnnotations = a })

class HasFields t where
    fields :: Lens (t a) [Field a]

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
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

oneWay :: Lens (Function a) Bool
oneWay = lens functionOneWay (\s a -> s { functionOneWay = a })

returnType :: Lens (Function a) (Maybe (TypeReference a))
returnType = lens functionReturnType (\s a -> s { functionReturnType = a })

parameters :: Lens (Function a) [Field a]
parameters = lens functionParameters (\s a -> s { functionParameters = a })

exceptions :: Lens (Function a) (Maybe [Field a])
exceptions = lens functionExceptions (\s a -> s { functionExceptions = a })

instance HasName (Function a) where
    name = lens functionName (\s a -> s { functionName = a })

instance HasSrcAnnot Function where
    srcAnnot = lens functionSrcAnnot (\s a -> s { functionSrcAnnot = a })

instance HasDocstring (Function a) where
    docstring = lens functionDocstring (\s a -> s { functionDocstring = a })

instance HasAnnotations (Function a) where
    annotations = lens functionAnnotations (\s a -> s { functionAnnotations = a })

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
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

functions :: Lens (Service a) [Function a]
functions = lens serviceFunctions (\s a -> s { serviceFunctions = a })

extends :: Lens (Service a) (Maybe Text)
extends = lens serviceExtends (\s a -> s { serviceExtends = a })

instance HasName (Service a) where
    name = lens serviceName (\s a -> s { serviceName = a })

instance HasSrcAnnot Service where
    srcAnnot = lens serviceSrcAnnot (\s a -> s { serviceSrcAnnot = a })

instance HasDocstring (Service a) where
    docstring = lens serviceDocstring (\s a -> s { serviceDocstring = a })

instance HasAnnotations (Service a) where
    annotations = lens serviceAnnotations (\s a -> s { serviceAnnotations = a })

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
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

instance HasValue (Const a) (ConstValue a) where
    value = lens constValue (\s a -> s { constValue = a })

instance HasName (Const a) where
    name = lens constName (\s a -> s { constName = a })

instance HasSrcAnnot Const where
    srcAnnot = lens constSrcAnnot (\s a -> s { constSrcAnnot = a })

instance HasValueType Const where
    valueType = lens constValueType (\s a -> s { constValueType = a })

instance HasDocstring (Const a) where
    docstring = lens constDocstring (\s a -> s { constDocstring = a })

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
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

targetType :: Lens (Typedef a) (TypeReference a)
targetType = lens typedefTargetType (\s a -> s { typedefTargetType = a })

instance HasName (Typedef a) where
    name = lens typedefName (\s a -> s { typedefName = a })

instance HasSrcAnnot Typedef where
    srcAnnot = lens typedefSrcAnnot (\s a -> s { typedefSrcAnnot = a })

instance HasDocstring (Typedef a) where
    docstring = lens typedefDocstring (\s a -> s { typedefDocstring = a })

instance HasAnnotations (Typedef a) where
    annotations = lens typedefAnnotations (\s a -> s { typedefAnnotations = a })

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
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

instance HasValue (EnumDef a) (Maybe Integer) where
    value = lens enumDefValue (\s a -> s { enumDefValue = a })

instance HasName (EnumDef a) where
    name = lens enumDefName (\s a -> s { enumDefName = a })

instance HasSrcAnnot EnumDef where
    srcAnnot = lens enumDefSrcAnnot (\s a -> s { enumDefSrcAnnot = a })

instance HasDocstring (EnumDef a) where
    docstring = lens enumDefDocstring (\s a -> s { enumDefDocstring = a })

instance HasAnnotations (EnumDef a) where
    annotations = lens enumDefAnnotations (\s a -> s { enumDefAnnotations = a })

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
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

class HasValues s a | s -> a where
    values :: Lens s a

instance HasValues (Enum a) [EnumDef a] where
    values = lens enumValues (\s a -> s { enumValues = a })

instance HasName (Enum a) where
    name = lens enumName (\s a -> s { enumName = a })

instance HasSrcAnnot Enum where
    srcAnnot = lens enumSrcAnnot (\s a -> s { enumSrcAnnot = a })

instance HasDocstring (Enum a) where
    docstring = lens enumDocstring (\s a -> s { enumDocstring = a })

instance HasAnnotations (Enum a) where
    annotations = lens enumAnnotations (\s a -> s { enumAnnotations = a })

-- | The kind of the struct.
data StructKind
    = StructKind       -- ^ @struct@
    | UnionKind        -- ^ @union@
    | ExceptionKind    -- ^ @exception@
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

-- | A struct, union, or exception definition.
--
-- > struct User {
-- >     1: Role role = Role.User;
-- > }
--
-- > union Value {
-- >     1: string stringValue;
-- >     2: i32 intValue;
-- > }
--
-- > exception UserDoesNotExist {
-- >     1: optional string message
-- >     2: required string username
-- > }
data Struct srcAnnot = Struct
    { structKind        :: StructKind
    -- ^ Kind of the structure.
    , structName        :: Text
    -- ^ Name of the struct.
    , structFields      :: [Field srcAnnot]
    -- ^ Fields defined in the struct.
    , structAnnotations :: [TypeAnnotation]
    -- ^ Annotations added to the struct.
    , structDocstring   :: Docstring
    -- ^ Documentation.
    , structSrcAnnot    :: srcAnnot
    }
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

kind :: Lens (Struct a) StructKind
kind = lens structKind (\s a -> s { structKind = a })

instance HasName (Struct a) where
    name = lens structName (\s a -> s { structName = a })

instance HasFields Struct where
    fields = lens structFields (\s a -> s { structFields = a })

instance HasSrcAnnot Struct where
    srcAnnot = lens structSrcAnnot (\s a -> s { structSrcAnnot = a })

instance HasDocstring (Struct a) where
    docstring = lens structDocstring (\s a -> s { structDocstring = a })

instance HasAnnotations (Struct a) where
    annotations = lens structAnnotations (\s a -> s { structAnnotations = a })

-- | A union of other types.
type Union = Struct
{-# DEPRECATED Union "The type has been consolidated into Struct." #-}

unionName :: Union a -> Text
unionName = structName
{-# DEPRECATED unionName "Use structName." #-}

unionFields :: Union a -> [Field a]
unionFields = structFields
{-# DEPRECATED unionFields "Use structFields." #-}

unionAnnotations :: Union a -> [TypeAnnotation]
unionAnnotations = structAnnotations
{-# DEPRECATED unionAnnotations "Use structAnnotations." #-}

unionDocstring :: Union a -> Docstring
unionDocstring = structDocstring
{-# DEPRECATED unionDocstring "Use structDocstring." #-}

unionSrcAnnot :: Union a -> a
unionSrcAnnot = structSrcAnnot
{-# DEPRECATED unionSrcAnnot "Use structSrcAnnot." #-}

-- | Exception types.
type Exception = Struct
{-# DEPRECATED Exception "The type has been consolidated into Struct." #-}

exceptionName :: Exception a -> Text
exceptionName = structName
{-# DEPRECATED exceptionName "Use structName." #-}

exceptionFields :: Exception a -> [Field a]
exceptionFields = structFields
{-# DEPRECATED exceptionFields "Use structFields." #-}

exceptionAnnotations :: Exception a -> [TypeAnnotation]
exceptionAnnotations = structAnnotations
{-# DEPRECATED exceptionAnnotations "Use structAnnotations." #-}

exceptionDocstring :: Exception a -> Docstring
exceptionDocstring = structDocstring
{-# DEPRECATED exceptionDocstring "Use structDocstring." #-}

exceptionSrcAnnot :: Exception a -> a
exceptionSrcAnnot = structSrcAnnot
{-# DEPRECATED exceptionSrcAnnot "Use structSrcAnnot." #-}

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
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

instance HasValues (Senum a) [Text] where
    values = lens senumValues (\s a -> s { senumValues = a })

instance HasName (Senum a) where
    name = lens senumName (\s a -> s { senumName = a })

instance HasSrcAnnot Senum where
    srcAnnot = lens senumSrcAnnot (\s a -> s { senumSrcAnnot = a })

instance HasDocstring (Senum a) where
    docstring = lens senumDocstring (\s a -> s { senumDocstring = a })

instance HasAnnotations (Senum a) where
    annotations = lens senumAnnotations (\s a -> s { senumAnnotations = a })

-- | Defines the various types that can be declared in Thrift.
data Type srcAnnot
    = -- | @typedef@
      TypedefType (Typedef srcAnnot)
    | -- | @enum@
      EnumType (Enum srcAnnot)
    | -- | @struct@/@union@/@exception@
      StructType (Struct srcAnnot)
    | -- | @senum@
      SenumType (Senum srcAnnot)
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

instance HasName (Type a) where
    name = lens getter setter
      where
        getter (TypedefType   t) = view name t
        getter (EnumType      t) = view name t
        getter (StructType    t) = view name t
        getter (SenumType     t) = view name t

        setter (TypedefType   t) n = TypedefType   $ set name n t
        setter (EnumType      t) n = EnumType      $ set name n t
        setter (StructType    t) n = StructType    $ set name n t
        setter (SenumType     t) n = SenumType     $ set name n t

instance HasSrcAnnot Type where
    srcAnnot = lens getter setter
      where
        getter (TypedefType   t) = view srcAnnot t
        getter (EnumType      t) = view srcAnnot t
        getter (StructType    t) = view srcAnnot t
        getter (SenumType     t) = view srcAnnot t

        setter (TypedefType   t) a = TypedefType   $ set srcAnnot a t
        setter (EnumType      t) a = EnumType      $ set srcAnnot a t
        setter (StructType    t) a = StructType    $ set srcAnnot a t
        setter (SenumType     t) a = SenumType     $ set srcAnnot a t

-- | A definition either consists of new constants, new types, or new
-- services.
data Definition srcAnnot
    = -- | A declared constant.
      ConstDefinition (Const srcAnnot)
    | -- | A custom type.
      TypeDefinition (Type srcAnnot)
    | -- | A service definition.
      ServiceDefinition (Service srcAnnot)
  deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

instance HasName (Definition a) where
    name = lens getter setter
      where
        getter (ConstDefinition   d) = view name d
        getter (TypeDefinition    d) = view name d
        getter (ServiceDefinition d) = view name d

        setter (ConstDefinition   d) n = ConstDefinition   $ set name n d
        setter (TypeDefinition    d) n = TypeDefinition    $ set name n d
        setter (ServiceDefinition d) n = ServiceDefinition $ set name n d

instance HasSrcAnnot Definition where
    srcAnnot = lens getter setter
      where
        getter (ConstDefinition   d) = view srcAnnot d
        getter (TypeDefinition    d) = view srcAnnot d
        getter (ServiceDefinition d) = view srcAnnot d

        setter (ConstDefinition   d) a = ConstDefinition   $ set srcAnnot a d
        setter (TypeDefinition    d) a = TypeDefinition    $ set srcAnnot a d
        setter (ServiceDefinition d) a = ServiceDefinition $ set srcAnnot a d


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
    deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

language :: Lens (Namespace a) Text
language = lens namespaceLanguage (\s a -> s { namespaceLanguage = a })

instance HasName (Namespace a) where
    name = lens namespaceName (\s a -> s { namespaceName = a })

instance HasSrcAnnot Namespace where
    srcAnnot = lens namespaceSrcAnnot (\s a -> s { namespaceSrcAnnot = a })

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
    deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

path :: Lens (Include a) Text
path = lens includePath (\s a -> s { includePath = a })

instance HasSrcAnnot Include where
    srcAnnot = lens includeSrcAnnot (\s a -> s { includeSrcAnnot = a })

-- | Headers for a program.
data Header srcAnnot
    = -- | Request to include another Thrift file.
      HeaderInclude (Include srcAnnot)
    | -- | A @namespace@ specifier.
      HeaderNamespace (Namespace srcAnnot)
    deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

-- | A program represents a single Thrift document.
data Program srcAnnot = Program
    { programHeaders     :: [Header srcAnnot]
    -- ^ Headers in a document define includes and namespaces.
    , programDefinitions :: [Definition srcAnnot]
    -- ^ Types and services defined in the document.
    }
    deriving (Show, Ord, Eq, Data, Typeable, Generic, Functor)

headers :: Lens (Program a) [Header a]
headers = lens programHeaders (\s a -> s { programHeaders = a })

definitions :: Lens (Program a) [Definition a]
definitions = lens programDefinitions (\s a -> s { programDefinitions = a })
