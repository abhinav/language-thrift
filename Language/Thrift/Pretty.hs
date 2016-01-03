{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Language.Thrift.Pretty
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module provides a pretty printer for Thrift IDLs. The pretty printer
-- preserves docstrings specified for types.
--
-- The specifics of the printer can be configured using 'Config' objects.
--
-- The module also exports instances of the 'Pretty' typeclass for elements of
-- the AST.
module Language.Thrift.Pretty
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

#define PrettyPrinter Text.PrettyPrint.Leijen
#include "Pretty/PrettyInc.hs"
#undef PrettyPrinter
