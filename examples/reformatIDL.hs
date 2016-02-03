{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- This program reads Thrift IDLs from stdin and writes their reformatted
-- versions back to stdout.
--
-- Docstrings in the IDL are preserved but COMMENTS WILL BE LOST.

import System.Exit (exitFailure)
import System.IO   (stderr)

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Language.Thrift.Parser (parse)

main :: IO ()
main = do
    result <- parse "stdin" `fmap` getContents
    case result of
        Right p  -> PP.putDoc (PP.pretty p) >> putStrLn ""
        Left err -> print err >> exitFailure
