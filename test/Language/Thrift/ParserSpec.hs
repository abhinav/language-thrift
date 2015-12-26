{-# LANGUAGE CPP #-}
module Language.Thrift.ParserSpec (spec) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Test.Hspec
import Text.Parser.Combinators (eof)

import TestUtils

import qualified Language.Thrift.Parser as P
import qualified Language.Thrift.Types  as T

spec :: Spec
spec = describe "Parser" $ do

    it "can parse empty documents" $
        assertParses P.program (T.Program [] []) ""

    testParseFailure

testParseFailure :: Spec
testParseFailure = do

    it "cannot parse invalid documents" $
      parseFailureCases P.program
        [ "namespace foo"
        , "const i32 100 = \"foo\""
        , "include \"foo"
        , "enum { }"
        , "struct { }"
        , "service A extends B"
        , "service A extends {}"
        ]

    it "cannot parse invalid type references" $
      parseFailureCases (P.typeReference <* eof)
        [ "list<i32"
        , "set<string, string>"
        , "map<i64>"
        , "something_else<foo>"
        ]

parseFailureCases :: Show a => Parser a -> [String] -> Expectation
parseFailureCases p = mapM_ (p `shouldNotParse`)
