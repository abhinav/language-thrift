{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- This program reads Thrift IDLs from stdin and writes their reformatted
-- versions back to stdout.
--
-- Docstrings in the IDL are preserved but COMMENTS WILL BE LOST.

import System.IO               (stderr)
import Text.Trifecta           (Result (..), parseString)
import Text.Trifecta.Delta     (Delta (Directed))

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Language.Thrift.Parser.Trifecta (thriftIDL)

main :: IO ()
main = do
    result <-
        parseString thriftIDL (Directed "stdin" 0 0 0 0) `fmap` getContents
    case result of
        Success p   -> PP.putDoc (PP.pretty p) >> putStrLn ""
        Failure doc -> PP.displayIO stderr $ PP.renderPretty 0.8 80 doc
