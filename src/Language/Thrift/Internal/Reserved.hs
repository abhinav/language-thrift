{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Language.Thrift.Internal.Reserved
-- Copyright   :  (c) Abhinav Gupta 2016
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module provides information about reserved Thrift identifiers.
module Language.Thrift.Internal.Reserved
    ( isReserved
    ) where

import Data.Set  (Set)
import Data.Text (Text)

import qualified Data.Set as Set

isReserved :: Text -> Bool
isReserved = (`Set.member` reservedKeywords)

reservedKeywords :: Set Text
reservedKeywords = Set.fromList
    [ "include"
    , "namespace"
    , "cpp_namespace"
    , "php_namespace"
    , "py_module"
    , "perl_package"
    , "ruby_namespace"
    , "java_package"
    , "cocoa_package"
    , "csharp_namespace"
    , "typedef"
    , "enum"
    , "struct"
    , "union"
    , "exception"
    , "required"
    , "optional"
    , "senum"
    , "const"
    , "string"
    , "binary"
    , "slist"
    , "bool"
    , "byte"
    , "i8"
    , "i16"
    , "i32"
    , "i64"
    , "double"
    , "map"
    , "set"
    , "list"
    , "service"
    , "extends"
    , "oneway"
    , "void"
    , "throws"
    ]
