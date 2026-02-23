{-# LANGUAGE TemplateHaskell #-}

module TextState where

import Lens.Micro.TH (makeLenses)

data DisplayType =
    Inline
    | Block
    deriving (Show, Eq)

data TextState = TextState {
    _foregroundColor :: (Int, Int, Int)
    , _backgroundColor :: Maybe (Int, Int, Int)
    , _bold :: Bool
    , _italicized :: Bool
    , _underlined :: Bool
    , _struckthrough :: Bool
    , _real :: Bool
    , _display :: DisplayType
} deriving (Eq, Show)
$(makeLenses ''TextState)
