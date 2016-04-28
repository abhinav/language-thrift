{-# LANGUAGE RankNTypes #-}
-- | This module implements a minimal lens type.
module Language.Thrift.Internal.Lens
    ( Lens
    , lens
    , set
    , view
    ) where

import Data.Functor.Identity (Identity (..))

import qualified Control.Applicative as A

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter f s = setter s `fmap` f (getter s)

set :: Lens s a -> a -> s -> s
set l a = runIdentity . l (\_ -> Identity a)

view :: Lens s a -> s -> a
view l = A.getConst . l A.Const
