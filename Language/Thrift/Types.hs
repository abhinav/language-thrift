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
-- parser-specific source annotation. With @trifecta@ this can hold the
-- 'Text.Trifecta.Delta.Delta'. If you do not have need of this, you can use
-- @()@ as the parameter.
--
module Language.Thrift.Types
    ( module Language.Thrift.Internal.Types
    ) where

import Language.Thrift.Internal.Types
import Language.Thrift.Pretty         ()
import Language.Thrift.Pretty.ANSI    ()
