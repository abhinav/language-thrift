{-# LANGUAGE OverloadedStrings #-}
module TestUtils
    ( assertParses
    , shouldNotParse
    , Parser
    ) where

import Test.Hspec (expectationFailure, shouldBe)

import qualified Text.Trifecta       as T
import qualified Text.Trifecta.Delta as T

import qualified Language.Thrift.Parser as P

type Parser a = P.ThriftParser T.Parser () a

parse :: Parser a -> String -> T.Result a
parse parser = T.parseString trifectaParser (T.Directed "memory" 0 0 0 0)
  where
    trifectaParser = P.runThriftParser (return ()) parser

-- | @assertParses parser expected input@ will assert that running @parser@ on
-- @input@ will produce @expected@.
assertParses :: (Show a, Eq a) => Parser a -> a -> String -> IO ()
assertParses parser expected input =
    case parse parser input of
        T.Success got -> got `shouldBe` expected
        T.Failure msg -> expectationFailure $
            "failed to parse:\n" ++ indent 8 input ++
            "\n\texpected: " ++ show expected ++
            "\n\tgot (error): " ++ show msg

-- | @parser `shouldNotParse` input@ will assert that running @parser@ on
-- @input@ will fail.
shouldNotParse :: Show a => Parser a -> String -> IO ()
shouldNotParse parser input =
    case parse parser input of
        T.Success result -> expectationFailure $
            "expected parse failure for\n" ++ indent 8 input ++
            "\n\tgot success: " ++ show result
        T.Failure _ -> return ()


-- | Indent all lines in the string the given number of spaces.
indent :: Int -> String -> String
indent n = unlines . map go . lines
  where
    go l = if null l then l else space ++ l
    space = replicate n ' '
