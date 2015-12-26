{-# LANGUAGE OverloadedStrings #-}
module Language.Thrift.TypesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Text.PrettyPrint.Leijen (Doc)
import Text.Parser.Token       (whiteSpace)

import Language.Thrift.Arbitrary ()
import TestUtils

import qualified Language.Thrift.Parser as P
import qualified Language.Thrift.Pretty as PP

spec :: Spec
spec =
    describe "Parser and Printer" $ do

        prop "can round-trip type references" $
            roundtrip PP.typeReference P.typeReference

        prop "can round-trip constant values" $
            roundtrip PP.constantValue P.constantValue

        prop "can round-trip typedefs" $
            roundtrip PP.typedef (whiteSpace >> P.typedef)

        prop "can round-trip enums" $
            roundtrip PP.enum (whiteSpace >> P.enum)

        prop "can round-trip structs" $
            roundtrip PP.struct (whiteSpace >> P.struct)

        prop "can round-trip unions" $
            roundtrip PP.union (whiteSpace >> P.union)

        prop "can round-trip exceptions" $
            roundtrip PP.exception (whiteSpace >> P.exception)

        prop "can round-trip senums" $
            roundtrip PP.senum (whiteSpace >> P.senum)

        prop "can round-trip services" $
            roundtrip PP.service (whiteSpace >> P.service)

        prop "can round-trip constants" $
            roundtrip PP.constant (whiteSpace >> P.constant)

        prop "can round-trip includes" $
            roundtrip (const PP.include) (whiteSpace >> P.include)

        prop "can round-trip namespaces" $
            roundtrip (const PP.namespace) (whiteSpace >> P.namespace)

        prop "can round-trip documents" $
            roundtrip PP.program P.program


roundtrip
    :: (Show a, Eq a) => (PP.Config -> a -> Doc) -> Parser a -> a -> IO ()
roundtrip printer parser value =
    assertParses parser value (show $ printer (PP.Config 4) value)
