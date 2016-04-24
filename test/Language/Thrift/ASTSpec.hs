{-# LANGUAGE OverloadedStrings #-}
module Language.Thrift.ASTSpec (spec) where

import Control.Monad         (void)
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Megaparsec       (SourcePos)

import qualified Text.PrettyPrint.ANSI.Leijen as PP (Doc, plain)

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
            roundtrip PP.typedef (P.whiteSpace >> P.typedef)

        prop "can round-trip enums" $
            roundtrip PP.enum (P.whiteSpace >> P.enum)

        prop "can round-trip structs" $
            roundtrip PP.struct (P.whiteSpace >> P.struct)

        prop "can round-trip unions" $
            roundtrip PP.union (P.whiteSpace >> P.union)

        prop "can round-trip exceptions" $
            roundtrip PP.exception (P.whiteSpace >> P.exception)

        prop "can round-trip senums" $
            roundtrip PP.senum (P.whiteSpace >> P.senum)

        prop "can round-trip services" $
            roundtrip PP.service (P.whiteSpace >> P.service)

        prop "can round-trip constants" $
            roundtrip PP.constant (P.whiteSpace >> P.constant)

        prop "can round-trip includes" $
            roundtrip (const PP.include) (P.whiteSpace >> P.include)

        prop "can round-trip namespaces" $
            roundtrip (const PP.namespace) (P.whiteSpace >> P.namespace)

        prop "can round-trip documents" $
            roundtrip PP.program P.program


roundtrip
    :: (Show (n ()), Eq (n ()), Functor n)
    => (PP.Config  -> n () ->  PP.Doc)
    -> Parser (n SourcePos)
    -> n ()
    -> IO ()
roundtrip printer parser value =
    assertParses (void `fmap` parser) value
        (show . PP.plain $ printer (PP.Config 4) value)
