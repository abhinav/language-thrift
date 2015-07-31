-- |
-- Module      :  Language.Thrift.Parser.Trifecta
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Provides a parser for Thrift IDLs based on Trifecta.
--
module Language.Thrift.Parser.Trifecta (thriftIDL) where

import Text.Trifecta
import Text.Trifecta.Delta (Delta)

import qualified Language.Thrift.Parser as P
import qualified Language.Thrift.Types  as T

-- | Parser for Thrift IDLs based on Trifecta.
--
-- Use with 'parseFromFile', 'parseByteString' or friends.
thriftIDL :: Parser (T.Program Delta)
thriftIDL = P.thriftIDL position
