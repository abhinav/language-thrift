module Language.Thrift.Pretty.Types
    ( Config(..)
    , defaultConfig
    ) where

-- | Configuration for the pretty printer.
data Config = Config
    { indentWidth :: Int
    -- ^ Number of spaces to use for indentation.
    } deriving (Show, Ord, Eq)


-- | Default pretty printing configuration.
defaultConfig :: Config
defaultConfig = Config 4
