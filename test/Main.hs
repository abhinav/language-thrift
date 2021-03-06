module Main (main) where

import Test.Hspec.Runner

import qualified Spec

main :: IO ()
main =
    hspecWith
        defaultConfig { configQuickCheckMaxSize = Just 30 }
        Spec.spec
