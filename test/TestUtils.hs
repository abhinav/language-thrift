{-# LANGUAGE OverloadedStrings #-}
module TestUtils
    ( assertParses
    , shouldNotParse
    , Parser
    ) where

import Control.Monad (when)
import Test.Hspec    (expectationFailure)

import qualified Language.Thrift.Parser as T
import qualified Text.Megaparsec        as P

type Parser = T.Parser String

parse :: Parser a -> String -> Either P.ParseError a
parse parser = P.parse (T.runParser parser) "memory"

-- | @assertParses parser expected input@ will assert that running @parser@ on
-- @input@ will produce @expected@.
assertParses :: (Show a, Eq a) => Parser a -> a -> String -> IO ()
assertParses parser expected input =
    case parse parser input of
        Right got -> when (got /= expected) $ expectationFailure $
            "\n\texpected: " ++ show expected ++
            "\n\tgot: " ++ show got ++
            "\n\tin:\n" ++ indent 8 input
        Left err -> expectationFailure $
            "failed to parse:\n" ++ indent 8 input ++
            "\n\texpected: " ++ show expected ++
            "\n\tgot (error): " ++ show err

-- | @parser `shouldNotParse` input@ will assert that running @parser@ on
-- @input@ will fail.
shouldNotParse :: Show a => Parser a -> String -> IO ()
shouldNotParse parser input =
    case parse parser input of
        Right result -> expectationFailure $
            "expected parse failure for\n" ++ indent 8 input ++
            "\n\tgot success: " ++ show result
        Left _ -> return ()


-- | Indent all lines in the string the given number of spaces.
indent :: Int -> String -> String
indent n = unlines . map go . lines
  where
    go l = if null l then l else space ++ l
    space = replicate n ' '
