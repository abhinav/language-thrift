{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Language.Thrift.Arbitrary () where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Data.Text       (Text)
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic)
import Test.QuickCheck

import qualified Data.Text as Text

import Language.Thrift.Internal.Reserved (isReserved)

import qualified Language.Thrift.AST as T

#ifdef MIN_VERSION_QuickCheck
#if !MIN_VERSION_QuickCheck(2, 8, 0)
scale :: (Int -> Int) -> Gen a -> Gen a
scale f g = sized (\n -> resize (f n) g)
#endif
#endif


-- | Halve the maximum size of generated values.
--
-- Generally speaking, it's a good idea to use this for calls that will
-- recursively generate lists of things so that they terminate at some point.
halfSize :: Gen a -> Gen a
halfSize = scale (\n -> truncate (fromIntegral n / 2 :: Double))

------------------------------------------------------------------------------

instance Arbitrary Text where
    arbitrary = Text.pack <$> listOf1 (elements charset)
      where
        charset = ['a'..'z'] ++ ['A'..'Z']

    shrink t
        | Text.length t < 2 = []
        | otherwise =
               [xs]
            ++ [Text.cons x xs' | xs' <- shrink xs]
            ++ [Text.cons x' xs | x' <- shrink x]
          where
            Just (x, xs) = Text.uncons t

newtype Docstring = Docstring { getDocstring :: Maybe Text }

instance Arbitrary Docstring where
    arbitrary = Docstring <$> oneof [return Nothing, comment]
      where
        commentLine = Text.unwords <$> listOf arbitrary
        comment = do
            s <- Text.strip . Text.unlines <$> listOf1 (halfSize commentLine)
            if Text.null s
                then return Nothing
                else return (Just s)

    shrink (Docstring t) = Docstring <$> shrink t

newtype Identifier = Identifier { getIdentifier :: Text }
    deriving (Show, Typeable, Generic)

instance Arbitrary Identifier where
    arbitrary = Identifier <$> arbitrary `suchThat` (not . isReserved . Text.unpack)

    shrink (Identifier t) =
        [Identifier t' | t' <- shrink t, not (isReserved (Text.unpack t'))]

------------------------------------------------------------------------------

instance Arbitrary (T.Program ()) where
    shrink = genericShrink
    arbitrary = T.Program <$> arbitrary <*> arbitrary

instance Arbitrary (T.Header ()) where
    shrink = genericShrink
    arbitrary = oneof
        [ T.HeaderInclude   <$> arbitrary
        , T.HeaderNamespace <$> arbitrary
        ]

instance Arbitrary (T.Include ()) where
    shrink = genericShrink
    arbitrary = T.Include <$> arbitrary <*> pure ()

instance Arbitrary (T.Namespace ()) where
    shrink = genericShrink
    arbitrary =
        T.Namespace
            <$> elements scopes
            <*> (getIdentifier <$> arbitrary)
            <*> pure ()
      where
        scopes = ["*", "py", "rb", "java", "hs", "cpp"]


instance Arbitrary (T.Definition ()) where
    shrink = genericShrink
    arbitrary = oneof
        [ T.ConstDefinition   <$> arbitrary
        , T.TypeDefinition    <$> arbitrary
        , T.ServiceDefinition <$> arbitrary
        ]

instance Arbitrary (T.Const ()) where
    shrink = genericShrink
    arbitrary =
        T.Const
            <$> arbitrary
            <*> (getIdentifier <$> arbitrary)
            <*> arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()

instance Arbitrary (T.Service ()) where
    shrink = genericShrink
    arbitrary =
        T.Service
            <$> (getIdentifier <$> arbitrary)
            <*> frequency
                    [ (1, return Nothing)
                    , (3, Just . getIdentifier <$> arbitrary)
                    ]
            <*> arbitrary
            <*> halfSize arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()

instance Arbitrary (T.Function ()) where
    shrink = genericShrink
    arbitrary =
        T.Function
            <$> arbitrary
            <*> halfSize arbitrary
            <*> (getIdentifier <$> arbitrary)
            <*> halfSize arbitrary
            <*> halfSize arbitrary
            <*> halfSize arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()

instance Arbitrary (T.Type ()) where
    shrink = genericShrink
    arbitrary = oneof
        [ T.TypedefType   <$> arbitrary
        , T.EnumType      <$> arbitrary
        , T.StructType    <$> arbitrary
        , T.UnionType     <$> arbitrary
        , T.ExceptionType <$> arbitrary
        , T.SenumType     <$> arbitrary
        ]

instance Arbitrary (T.Typedef ()) where
    shrink = genericShrink
    arbitrary = T.Typedef
        <$> arbitrary
        <*> (getIdentifier <$> arbitrary)
        <*> halfSize arbitrary
        <*> (getDocstring <$> arbitrary)
        <*> pure ()

instance Arbitrary (T.Enum ()) where
    shrink = genericShrink
    arbitrary = T.Enum
        <$> (getIdentifier <$> arbitrary)
        <*> arbitrary
        <*> halfSize arbitrary
        <*> (getDocstring <$> arbitrary)
        <*> pure ()

instance Arbitrary (T.EnumDef ()) where
    shrink = genericShrink
    arbitrary =
        T.EnumDef
            <$> (getIdentifier <$> arbitrary)
            <*> arbitrary
            <*> halfSize arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()

instance Arbitrary (T.Struct ()) where
    shrink = genericShrink
    arbitrary = T.Struct
        <$> (getIdentifier <$> arbitrary)
        <*> arbitrary
        <*> halfSize arbitrary
        <*> (getDocstring <$> arbitrary)
        <*> pure ()

instance Arbitrary (T.Union ()) where
    shrink = genericShrink
    arbitrary = T.Union
        <$> (getIdentifier <$> arbitrary)
        <*> arbitrary
        <*> halfSize arbitrary
        <*> (getDocstring <$> arbitrary)
        <*> pure ()

instance Arbitrary (T.Exception ()) where
    shrink = genericShrink
    arbitrary = T.Exception
        <$> (getIdentifier <$> arbitrary)
        <*> arbitrary
        <*> halfSize arbitrary
        <*> (getDocstring <$> arbitrary)
        <*> pure ()

instance Arbitrary (T.Senum ()) where
    shrink = genericShrink
    arbitrary = T.Senum
        <$> (getIdentifier <$> arbitrary)
        <*> arbitrary
        <*> halfSize arbitrary
        <*> (getDocstring <$> arbitrary)
        <*> pure ()

instance Arbitrary (T.Field ()) where
    shrink = genericShrink
    arbitrary =
        T.Field
            <$> (fmap getPositive <$> arbitrary)
            <*> arbitrary
            <*> halfSize arbitrary
            <*> (getIdentifier <$> arbitrary)
            <*> halfSize arbitrary
            <*> halfSize arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()

instance Arbitrary (T.TypeReference ()) where
    shrink = genericShrink
    arbitrary = oneof
        [ T.DefinedType           <$> (getIdentifier <$> arbitrary)
                                  <*> pure ()
        , halfSize $ T.StringType <$> arbitrary <*> pure ()
        , halfSize $ T.BinaryType <$> arbitrary <*> pure ()
        , halfSize $ T.SListType  <$> arbitrary <*> pure ()
        , halfSize $ T.BoolType   <$> arbitrary <*> pure ()
        , halfSize $ T.ByteType   <$> arbitrary <*> pure ()
        , halfSize $ T.I16Type    <$> arbitrary <*> pure ()
        , halfSize $ T.I32Type    <$> arbitrary <*> pure ()
        , halfSize $ T.I64Type    <$> arbitrary <*> pure ()
        , halfSize $ T.DoubleType <$> arbitrary <*> pure ()
        , halfSize $ T.MapType    <$> arbitrary <*> arbitrary <*> arbitrary <*> pure ()
        , halfSize $ T.SetType    <$> arbitrary <*> arbitrary <*> pure ()
        , halfSize $ T.ListType   <$> arbitrary <*> arbitrary <*> pure ()
        ]

instance Arbitrary T.FieldRequiredness where
    shrink = genericShrink
    arbitrary = elements [T.Required, T.Optional]

instance Arbitrary T.TypeAnnotation where
    shrink = genericShrink
    arbitrary = T.TypeAnnotation <$> (getIdentifier <$> arbitrary) <*> arbitrary


newtype BasicConstValue = BasicConstValue {
    getBasicConstValue :: T.ConstValue ()
  } deriving (Typeable, Generic)


instance Arbitrary BasicConstValue where
    shrink = genericShrink
    arbitrary = BasicConstValue <$> oneof
        [ T.ConstFloat      <$> choose (0.0, 10000.0) <*> pure ()
        , T.ConstInt        <$> arbitrary <*> pure ()
        , T.ConstLiteral    <$> arbitrary <*> pure ()
        , T.ConstIdentifier <$> (getIdentifier <$> arbitrary)
                            <*> pure ()
        ]

-- | newtype wrapper around const values so that we're not generating lists
-- and maps that go on forever.
newtype FiniteConstValue =
    FiniteConstValue { getFiniteConstValue :: T.ConstValue () }
  deriving (Typeable, Generic)

instance Arbitrary FiniteConstValue where
    shrink = genericShrink
    arbitrary = FiniteConstValue <$> oneof
        [ basicConsts
        , T.ConstList <$> constList <*> pure ()
        , T.ConstMap  <$> constMap <*> pure ()
        ]
      where
        basicConsts = getBasicConstValue <$> arbitrary
        constList
            = listOf $ halfSize $ getFiniteConstValue <$> arbitrary
        constMap
            = listOf $
                (,) <$> basicConsts
                    <*> halfSize (getFiniteConstValue <$> arbitrary)


instance Arbitrary (T.ConstValue ()) where
    shrink = genericShrink
    arbitrary = getFiniteConstValue <$> arbitrary
