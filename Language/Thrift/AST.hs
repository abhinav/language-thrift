-- |
-- Module      :  Language.Thrift.AST
-- Copyright   :  (c) Abhinav Gupta 2016
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
--
-- Lenses for attributes of most types are provided for use with the `lens`
-- library.
--
-- Types representing the AST all have 'Text.PrettyPrint.ANSI.Leijen.Pretty'
-- instances to go with them.
module Language.Thrift.AST
    ( module Language.Thrift.Internal.AST
    ) where

import Language.Thrift.Internal.AST
import Language.Thrift.Pretty       ()
