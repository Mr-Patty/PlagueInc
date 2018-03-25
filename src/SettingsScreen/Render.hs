-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SettingsScreen.Render where


  -- | Miso framework import
import Miso
import Miso.String
import Miso.Html.Element

import SettingsScreen.State
import SettingsScreen.Update


render :: State -> View Action
render state =
  div_ [align_ "center"]
    [ table_
      [
        align_ "center"
      ]
      [
      -- text $ ms "+"
        tr_ [] [input_ [onChange (\m -> SetNumber $ read $ checkString $ fromMisoString m)]]
      , tr_ [] [input_ [onChange (\m -> SetSeason $ readSeason $ read $ checkString $ fromMisoString m)]]
      , tr_ [] [input_ [onChange (\m -> SetTime $ read $ checkString $ fromMisoString m)]]
      , tr_ [] [input_ [onChange (\m -> SetPrice $ read $ checkString $ fromMisoString m)]]
      , tr_ [] [input_ [onChange (\m -> SetFond $ read $ checkString $ fromMisoString m)]]
      ],
      table_ []
      (fmap (\x -> renderCitySetting x) [1..(number state)]),
      div_ [] [button_ [onClick $ SetNumber 2] [text "!"]],
      div_ [] [text $ ms $ show $ (number state)]
    ]

checkString :: String -> String
checkString "" = "0"
checkString str = str


renderCitySetting :: Int -> View Action
renderCitySetting n =
  tr_ [] [text $ ms $ show n]
