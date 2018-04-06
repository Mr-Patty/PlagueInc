-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Settings.Render where


  -- | Miso framework import
import Miso
import Miso.String
import Miso.Html.Element

import State
import Settings.Update


render :: State -> View Action
render state =
  table_ [align_ "center"]
    [ td_
      [
        align_ "center"
      ]
      [
      -- text $ ms "+"
        tr_ []
          [ div_ [] [text "Число городов: "]
          , input_ [onChange (\m -> SetNumber $ read $ checkString $ fromMisoString m)]
          ]
      , tr_ []
          [ div_ [] [text "Сезон года: "]
          , input_ [onChange (\m -> SetSeason $ readSeason $ read $ checkString $ fromMisoString m)]
          ]
      , tr_ []
          [ div_ [] [text "Период моделирования: "]
          , input_ [onChange (\m -> SetTime $ read $ checkString $ fromMisoString m)]
          ]
      , tr_ []
          [ div_ [] [text "Стоимость 1-й прививки: "]
          , input_ [onChange (\m -> SetPrice $ read $ checkString $ fromMisoString m)]
          ]
      , tr_ []
          [ div_ [] [text "Денежный фонд: "]
          , input_ [onChange (\m -> SetFond $ read $ checkString $ fromMisoString m)]
          ]
      , div_ [] [button_ [onClick $ Next] [text "Далее"]]
      ],
      td_ []
      (fmap (\x -> renderCitySetting x) [1..(number state)])
    ]

checkString :: String -> String
checkString "" = "0"
checkString str = str


renderCitySetting :: Int -> View Action
renderCitySetting n =
  table_ []
  [ text $ ms $ "Город №" ++ (show n)
  , tr_ []
    [ div_ [] [text "Численность населения: "]
    , input_ [onChange (\m -> SetPopulation n $ read $ checkString $ fromMisoString m )]
    ]
  , tr_ []
    [ div_ [] [text "% больных: "]
    , input_ [onChange (\m -> SetSeek n $ read $ checkString $ fromMisoString m )]
    ]
  , tr_ []
    [ div_ [] [text "% привитых: "]
    , input_ [onChange (\m -> SetPriv n $ read $ checkString $ fromMisoString m )]
    ]
  , tr_ []
    [ div_ [] [text "Насыщенность транспортного сообщения: "]
    , input_ [onChange (\m -> SetTrans n $ read $ checkString $ fromMisoString m )]
    ]
  , hr_ []
  ]
  -- tr_ [] [text $ ms $ show n]
