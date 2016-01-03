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
-- This module provides a pretty printer for Thrift IDLs that produces colored
-- output. It is essentially the same as "Language.Thrift.Pretty" with the
-- exception of the colored output.
--
-- The behavior of the printer can be customized using 'Config' objects.
--
-- The system uses ANSI escape codes to produce colored output. That makes the
-- text output of this pretty printer unparseable without printing to a
-- supported terminal. If this is undesirable, use
-- 'Text.PrettyPrint.ANSI.Leijen.plain' to discard coloring information, or
-- simply use the "Language.Thrift.Pretty" pretty printer instead.
--
-- As with "Language.Thrift.Pretty", this module exports instances of the
-- 'Pretty' typeclass for elements of the AST.
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

#define PrettyPrinter Text.PrettyPrint.ANSI.Leijen
#define PrettyPrinterSupportsHighlighting
#include "PrettyInc.hs"
#undef PrettyPrinterSupportsHighlighting
#undef PrettyPrinter
