{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- This program reads Thrift IDLs from stdin and writes their reformatted
-- versions back to stdout.
--
-- Docstrings in the IDL are preserved but COMMENTS WILL BE LOST.

import System.IO               (stderr)
import Text.PrettyPrint.Leijen (putDoc)
import Text.Trifecta           (Result (..), parseString)
import Text.Trifecta.Delta     (Delta (Directed))

import qualified Text.PrettyPrint.ANSI.Leijen as AnsiPP

import Language.Thrift.Parser.Trifecta (thriftIDL)
import Language.Thrift.Pretty          (prettyPrint)

main :: IO ()
main = do
    result <- parseString thriftIDL (Directed "stdin" 0 0 0 0) <$> getContents
    case result of
        Success p -> putDoc (prettyPrint p) >> putStrLn ""
        Failure doc ->
            AnsiPP.displayIO stderr $ AnsiPP.renderPretty 0.8 80 doc
