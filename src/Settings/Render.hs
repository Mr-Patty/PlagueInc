-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Settings.Render where


  -- | Miso framework import
import Miso
import Miso.String
import Miso.Html.Element
import qualified Data.Map as M

import State
import Settings.Update


render :: State -> View Action
render state =
  table_
  [ align_ "center"
  , width_ "300"
  , height_ "500"
  ]
    [ td_
      [
        align_ "center", width_ "300", height_ "500"
      ]
      [
      -- text $ ms "+"
        tr_ []
          [ div_ [] [text "Число городов: "]
          , input_ [onChange (\m -> SetNumber $ read $ checkString $ fromMisoString m)]
          ]
      , tr_ []
          [ div_ [] [text "Сезон года(1-12): "]
          , input_ [onChange (\m -> SetSeason $ readSeason $ read $ checkString $ fromMisoString m)]
          ]
      , tr_ []
          [ div_ [] [text "Период моделирования(в днях): "]
          , input_ [onChange (\m -> SetTime $ read $ checkString $ fromMisoString m)]
          ]
      , tr_ []
          [ div_ [] [text "Стоимость 1-й прививки(в тыс.): "]
          , input_ [onChange (\m -> SetPrice $ read $ checkString $ fromMisoString m)]
          ]
      , tr_ []
          [ div_ [] [text "Денежный фонд(в мл.): "]
          , input_ [onChange (\m -> SetFond $ read $ checkString $ fromMisoString m)]
          ]
      , div_ [] [button_ [onClick $ Next] [text "Далее"]]
      ],
      td_ []
      (fmap (\x -> renderCitySetting x) [0..(number state - 1)])
    ]

checkString :: String -> String
checkString "" = "0"
checkString str = str


renderCitySetting :: Int -> View Action
renderCitySetting n =
  table_ []
  [ text $ ms $ "Город № " ++ (show $ n + 1)
  , tr_ []
    [ div_ [] [text "Численность населения(в мл., максимум 15мл.): "]
    , input_ [onChange (\m -> SetPopulation n $ read $ checkString $ fromMisoString m )]
    ]
  , tr_ []
    [ div_ [] [text "% больных: "]
    , input_ [onChange (\m -> SetSick n $ read $ checkString $ fromMisoString m )]
    ]
  , tr_ []
    [ div_ [] [text "% привитых: "]
    , input_ [onChange (\m -> SetVaccine n $ read $ checkString $ fromMisoString m )]
    ]
  , tr_ []
    [ div_ [] [text "Насыщенность транспортного сообщения(в %): "]
    , input_ [onChange (\m -> SetTraffic n $ read $ checkString $ fromMisoString m )]
    ]
  , hr_ []
  ]
  -- tr_ [] [text $ ms $ show n]
