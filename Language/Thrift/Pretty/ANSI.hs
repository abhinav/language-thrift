{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Language.Thrift.Pretty.ANSI
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module is the same as "Language.Thrift.Pretty" but the pretty printer
-- used is the one provided by ansi-wl-pprint.
--
-- The specifics of the printer can be configured using 'Config' objects.
--
-- As with "Language.Thrift.Pretty", this module exports instances of the
-- 'Pretty' typeclass for @ansi-wl-pprint@.
module Language.Thrift.Pretty.ANSI
    (
      prettyPrint

    -- * Components

    , program

    , header
    , include
    , namespace

    , definition
    , constant
    , typeDefinition
    , service

    , typedef
    , enum
    , struct
    , union
    , exception
    , senum

    , typeReference
    , constantValue

    -- * Configuration

    , Config(..)
    , defaultConfig
    ) where

import           Text.PrettyPrint.ANSI.Leijen hiding (encloseSep, indent, text)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

#include "PrettyInc.hs"
