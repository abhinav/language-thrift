{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Thrift.Internal.TH
    ( makeFieldsFor
    , accessorLens
    ) where

import Control.Lens        (lens, (%~), (&))
import Control.Lens.TH     (LensRules, defaultFieldRules, lensField,
                            makeLensesWith)
import Language.Haskell.TH

-- | A version of 'Control.Lens.makeFields' that declares lenses only for the
-- given selectors.
makeFieldsFor :: [String] -> Name -> DecsQ
makeFieldsFor fields = makeLensesWith (fieldRulesFor fields)

fieldRulesFor :: [String] -> LensRules
fieldRulesFor fields = defaultFieldRules & lensField %~ mkLookup
  where
    mkLookup go t fs f
        | nameBase f `elem` fields = go t fs f
        | otherwise                = []

-- | A template haskell function to generate a simple lens for a record
-- accessor.
--
-- > $(accessorLens 'someAccessor)
--
-- Generates
--
-- > lens someAccessor (\s a -> s { someAccessor = a })
accessorLens :: Name -> ExpQ
accessorLens name = do
    s <- newName "s"
    a <- newName "a"

    let getter = appE [| lens |] (varE name)  -- lens $name
        setter =
            -- \s a -> s { name = a }
            lamE [varP s, varP a] $
                -- s { name = a }
                recUpdE (varE s) [fieldExp name (varE a)]

    appE getter setter
