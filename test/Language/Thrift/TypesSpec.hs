{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Thrift.TypesSpec where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Control.Monad           (unless)
import Data.Text               (Text)
import Data.Typeable           (Typeable)
import GHC.Generics            (Generic)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.PrettyPrint.Leijen (Doc)

import Text.Trifecta       (parseString)
import Text.Trifecta.Delta (Delta (..))

import qualified Data.Text     as Text
import qualified Text.Trifecta as Tri

import qualified Language.Thrift.Parser as P
import qualified Language.Thrift.Pretty as PP
import qualified Language.Thrift.Types  as T


-- | Halve the maximum size of generated values.
--
-- Generally speaking, it's a good idea to use this for calls that will
-- recursively generate lists of things so that they terminate at some point.
halfSize :: Gen a -> Gen a
halfSize = scale (\n -> truncate (fromIntegral n / 2 :: Double))

newtype Identifier = Identifier { getIdentifier :: Text }

instance Arbitrary Identifier where
    arbitrary = Identifier <$> arbitrary
    shrink (Identifier t) = [Identifier t' | t' <- shrink t]

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
        commentLine =
            Text.unwords <$> listOf (getIdentifier <$> arbitrary)
        comment = do
            s <- Text.strip . Text.unlines <$> listOf1 (halfSize commentLine)
            if Text.null s
                then return Nothing
                else return (Just s)

    shrink (Docstring t) = [Docstring x | x <- shrink t]


instance Arbitrary (T.Program ()) where
    shrink = genericShrink
    arbitrary = T.Program <$> arbitrary <*> arbitrary


instance Arbitrary (T.Definition ()) where
    shrink = genericShrink
    arbitrary = oneof [arbitraryConst, arbitraryType, arbitraryService]
      where
        arbitraryConst =
            T.ConstDefinition
                <$> arbitrary
                <*> (getIdentifier <$> arbitrary)
                <*> arbitrary
                <*> (getDocstring <$> arbitrary)
                <*> pure ()

        arbitraryType =
            T.TypeDefinition
                <$> arbitrary
                <*> arbitrary

        arbitraryService =
            T.ServiceDefinition
                <$> (getIdentifier <$> arbitrary)
                <*> (fmap getIdentifier <$> arbitrary)
                <*> arbitrary
                <*> arbitrary
                <*> (getDocstring <$> arbitrary)
                <*> pure ()


instance Arbitrary T.Header where
    shrink = genericShrink
    arbitrary = oneof
        [ T.Include   <$> (getIdentifier <$> arbitrary)
        , T.Namespace <$> elements scopes
                      <*> (getIdentifier <$> arbitrary)
        ]
      where
        scopes = ["py", "rb", "java", "hs", "cpp"]


instance Arbitrary (T.Field ()) where
    shrink = genericShrink
    arbitrary =
        T.Field
            <$> (fmap getPositive <$> arbitrary)
            <*> arbitrary
            <*> halfSize arbitrary
            <*> (getIdentifier <$> arbitrary)
            <*> halfSize arbitrary
            <*> arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()


instance Arbitrary (T.Function ()) where
    shrink = genericShrink
    arbitrary =
        T.Function
            <$> elements [True, False]
            <*> arbitrary
            <*> (getIdentifier <$> arbitrary)
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()


instance Arbitrary T.TypeAnnotation where
    shrink = genericShrink
    arbitrary =
        T.TypeAnnotation
            <$> (getIdentifier <$> arbitrary)
            <*> (getIdentifier <$> arbitrary)


instance Arbitrary (T.EnumDef ()) where
    shrink = genericShrink
    arbitrary =
        T.EnumDef
            <$> (getIdentifier <$> arbitrary)
            <*> arbitrary
            <*> arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()


instance Arbitrary (T.Type ()) where
    shrink = genericShrink
    arbitrary = oneof
        [ T.Typedef
            <$> arbitrary
            <*> (getIdentifier <$> arbitrary)
            <*> (getDocstring <$> arbitrary)
            <*> pure ()
        , T.Enum
            <$> (getIdentifier <$> arbitrary)
            <*> arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()
        , T.Struct
            <$> (getIdentifier <$> arbitrary)
            <*> arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()
        , T.Union
            <$> (getIdentifier <$> arbitrary)
            <*> arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()
        , T.Exception
            <$> (getIdentifier <$> arbitrary)
            <*> arbitrary
            <*> (getDocstring <$> arbitrary)
            <*> pure ()
        , T.Senum
            <$> (getIdentifier <$> arbitrary)
            <*> (map getIdentifier <$> arbitrary)
            <*> (getDocstring <$> arbitrary)
            <*> pure ()
        ]


instance Arbitrary (T.FieldRequiredness) where
    shrink = genericShrink
    arbitrary = elements [T.Required, T.Optional]

instance Arbitrary T.FieldType where
    shrink = genericShrink
    arbitrary = oneof
        [ T.DefinedType . getIdentifier <$> arbitrary

        , T.StringType <$> arbitrary
        , T.BinaryType <$> arbitrary
        , T.SListType  <$> arbitrary
        , T.BoolType   <$> arbitrary
        , T.ByteType   <$> arbitrary
        , T.I16Type    <$> arbitrary
        , T.I32Type    <$> arbitrary
        , T.I64Type    <$> arbitrary
        , T.DoubleType <$> arbitrary

        , halfSize $
            T.MapType <$> arbitrary <*> arbitrary <*> arbitrary
        , halfSize $
            T.SetType  <$> arbitrary <*> arbitrary
        , halfSize $
            T.ListType <$> arbitrary <*> arbitrary
        ]


newtype BasicConstValue = BasicConstValue {
    getBasicConstValue :: T.ConstValue
  } deriving (Typeable, Generic)


instance Arbitrary BasicConstValue where
    shrink = genericShrink
    arbitrary = BasicConstValue <$> oneof
        [ T.ConstFloat                      <$> choose (0.0, 10000.0)
        , T.ConstInt                        <$> arbitrary
        , T.ConstLiteral    . getIdentifier <$> arbitrary
        , T.ConstIdentifier . getIdentifier <$> arbitrary
        ]

-- | newtype wrapper around const values so that we're not generating lists
-- and maps that go on forever.
newtype FiniteConstValue =
    FiniteConstValue { getFiniteConstValue :: T.ConstValue }
  deriving (Typeable, Generic)

instance Arbitrary FiniteConstValue where
    shrink = genericShrink
    arbitrary = FiniteConstValue <$> oneof
        [ basicConsts
        , T.ConstList <$> constList
        , T.ConstMap  <$> constMap
        ]
      where
        basicConsts = getBasicConstValue <$> arbitrary
        constList
            = listOf $ halfSize $ getFiniteConstValue <$> arbitrary
        constMap
            = listOf $
                (,) <$> basicConsts
                    <*> halfSize (getFiniteConstValue <$> arbitrary)


instance Arbitrary T.ConstValue where
    shrink = genericShrink
    arbitrary = getFiniteConstValue <$> arbitrary


spec :: Spec
spec =
    describe "Can round trip" $ do

        prop "field types" $
            roundtrip PP.fieldType P.fieldType

        prop "constant values" $
            roundtrip PP.constantValue P.constantValue

        prop "functions" $
            roundtrip PP.function (Tri.whiteSpace *> P.function)

        prop "definitions" $
            roundtrip PP.definition (Tri.whiteSpace *> P.definition)

        prop "headers" $
            roundtrip (const PP.header) P.header

        prop "documents" $
            roundtrip PP.program P.program


roundtrip
    :: (Show a, Eq a)
    => (PP.Config -> a -> Doc)
    -> P.ThriftParser Tri.Parser () a
    -> a
    -> IO ()
roundtrip printer parser value = do
  let pretty = show . printer (PP.Config 4)
      triParser = P.runThriftParser (return ()) parser
      result =
          parseString triParser (Directed "memory" 0 0 0 0) (pretty value)
  case result of
    Tri.Success parsed ->
      unless (parsed == value) $ expectationFailure $
        "expected: " ++ show value ++ "\n but got: " ++ show parsed ++
        "\n\n expected (pretty): " ++ pretty value ++
        "\n but got (pretty): " ++ pretty parsed
    Tri.Failure msg -> expectationFailure $
      "failed to parse "  ++ pretty value ++
      "\n with " ++ show msg
