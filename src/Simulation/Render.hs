-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Simulation.Render where

import Miso
import Miso.String
import Miso.Html.Element

import Settings.State
import Simulation.Update

render :: State -> View Action
render state =
  div_ []
    [header state]

header :: State -> View Action
header state =
  table_ [align_ "left"]
    [ tr_ [] [text "Остаток денежного фонда: "]
    , tr_ [] [text "Стоимость прививки: "]
    , tr_ [] [text "Оставшееся время: "]
    ]
