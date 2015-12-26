{-# LANGUAGE OverloadedStrings #-}
module Language.Thrift.TypesSpec where

import Control.Monad           (unless)
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.PrettyPrint.Leijen (Doc)

import Text.Trifecta       (parseString)
import Text.Trifecta.Delta (Delta (..))

import qualified Text.Trifecta as Tri

import Language.Thrift.Arbitrary ()

import qualified Language.Thrift.Parser as P
import qualified Language.Thrift.Pretty as PP

spec :: Spec
spec =
    describe "Can round trip" $ do

        prop "type references" $
            roundtrip PP.typeReference P.typeReference

        prop "constant values" $
            roundtrip PP.constantValue P.constantValue

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
