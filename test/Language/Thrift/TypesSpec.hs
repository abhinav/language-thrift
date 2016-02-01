{-# LANGUAGE OverloadedStrings #-}
module Language.Thrift.TypesSpec (spec) where

import Control.Monad         (void)
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Megaparsec       (SourcePos)

import qualified Text.PrettyPrint.ANSI.Leijen as PPA (Doc, plain)
import qualified Text.PrettyPrint.Leijen      as PP (Doc)

import Language.Thrift.Arbitrary ()
import TestUtils

import qualified Language.Thrift.Parser      as P
import qualified Language.Thrift.Pretty      as PP
import qualified Language.Thrift.Pretty.ANSI as PPA

spec :: Spec
spec =
    describe "Parser and Printer" $ do

        prop "can round-trip type references" $
            roundtrip PP.typeReference PPA.typeReference P.typeReference

        prop "can round-trip constant values" $
            roundtrip PP.constantValue PPA.constantValue P.constantValue

        prop "can round-trip typedefs" $
            roundtrip PP.typedef PPA.typedef (P.whiteSpace >> P.typedef)

        prop "can round-trip enums" $
            roundtrip PP.enum PPA.enum (P.whiteSpace >> P.enum)

        prop "can round-trip structs" $
            roundtrip PP.struct PPA.struct (P.whiteSpace >> P.struct)

        prop "can round-trip unions" $
            roundtrip PP.union PPA.union (P.whiteSpace >> P.union)

        prop "can round-trip exceptions" $
            roundtrip PP.exception PPA.exception (P.whiteSpace >> P.exception)

        prop "can round-trip senums" $
            roundtrip PP.senum PPA.senum (P.whiteSpace >> P.senum)

        prop "can round-trip services" $
            roundtrip PP.service PPA.service (P.whiteSpace >> P.service)

        prop "can round-trip constants" $
            roundtrip PP.constant PPA.constant (P.whiteSpace >> P.constant)

        prop "can round-trip includes" $
            roundtrip
                (const PP.include)
                (const PPA.include)
                (P.whiteSpace >> P.include)

        prop "can round-trip namespaces" $
            roundtrip
                (const PP.namespace)
                (const PPA.namespace)
                (P.whiteSpace >> P.namespace)

        prop "can round-trip documents" $
            roundtrip PP.program PPA.program P.program


roundtrip
    :: (Show (n ()), Eq (n ()), Functor n)
    => (PP.Config  -> n () ->  PP.Doc)
    -> (PPA.Config -> n () -> PPA.Doc)
    -> Parser (n SourcePos)
    -> n ()
    -> IO ()
roundtrip printer ansiPrinter parser value = do
    assertParses (void `fmap` parser) value
        (show $ printer (PP.Config 4) value)

    -- For the ANSI pretty printer, we need to discard the color information
    -- for the document to be parseable.
    assertParses (void `fmap` parser) value
        (show . PPA.plain $ ansiPrinter (PPA.Config 4) value)
