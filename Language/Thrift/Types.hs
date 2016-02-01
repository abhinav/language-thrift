-- |
-- Module      :  Language.Thrift.Types
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module defines types that compose a Thrift IDL file.
--
-- Most of the types have an optional @srcAnnot@ parameter that represents a
-- source annotation. The parser produces types annotated with their position
-- in the Thrift file ('Text.Megaparsec.SourcePos'). When constructing the AST
-- by hand, you can use @()@. The types are @Functor@s so you can use 'fmap'
-- to change the annotation on all objects in a tree.
module Language.Thrift.Types
    ( module Language.Thrift.Internal.Types
    ) where

import Language.Thrift.Internal.Types
import Language.Thrift.Pretty         ()
