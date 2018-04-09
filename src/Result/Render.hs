-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Result.Render where

import Miso
import Miso.String

import Result.Update


render :: View Action
render =
  div_ [] [text "End"]
