-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Simulation.Render where

import Miso
import Miso.String
import Miso.Html.Element
import Miso.Svg
import Miso.Svg.Element
import qualified Data.Map as M
import Data.List.Index

import State
import Simulation.Update

render :: State -> View Action
render state =
  div_ [] [header state, towns state]

towns :: State -> View Action
towns state =
  div_
    [
    Miso.style_ $ M.fromList
      [
      ("margin-left", "-150px"),
      ("margin-top", "-150px"),
      ("position", "absolute"),
      ("left", "50%"),
      ("top", "50%")
      ]
    ]
    $ [tr_ [] (imap (\ i a -> oneTown (change state) 3 i a) (Prelude.tail $ cities state))]
    -- text "lol"
    -- town (cities state)
oneTown :: Maybe Int -> Int -> Int -> City -> View Action
oneTown change count n city =
  div_
    [
    Miso.style_ $ M.fromList
      [
      ("left", ms $ "-" ++ (show $ 200 + n * 50) ++ "px"),
      ("top", "-200px")
      ]
    ]
    [
    case change of
      Nothing -> town city n
      (Just m) -> if m == n then changeCity city n else town city n
    ]

changeCity :: City -> Int -> View Action
changeCity city n =
  table_ [align_ "center", Miso.width_ "300", Miso.height_ "300"]
    [ td_
      [
        align_ "center"
      ]
      [
        tr_ []
          [ div_ [] [text $ ms $ "Число привитых: " ++ (show $ priv city)]
          ]
      , tr_ []
          [ div_ [] [text $ ms $ "Заболевшие: " ++ (show $ seek city)]
          ]
      , tr_ []
          [ div_ [] [text "Кол-во прививок: "]
          , input_ [onChange (\m -> SetInject n (read $ checkString $ fromMisoString m))]
          ]
      , div_ [] [button_ [onClick $ Ok n] [text "Ok"]]
      ]
    ]

town :: City -> Int -> View Action
town city n =
  svg_ [Miso.Svg.height_ "300", Miso.Svg.width_ "300", onClick (Click n) ]
    [
    circle_ [ cx_ "150", cy_ "150", r_ (ms $ population city * 50), stroke_ "black", strokeWidth_ "1", fill_ "white" ] [],
    circle_ [ cx_ "150", cy_ "150", r_ (ms $ seek city * 40), fill_ "red" ] []
    ]

header :: State -> View Action
header state =
  table_ [align_ "left"]
    [ tr_ [] [text $ ms $ "Остаток денежного фонда: " ++ (show $ fond state)]
    , tr_ [] [text $ ms $ "Стоимость прививки: " ++ (show $ price state)]
    , tr_ [] [text $ ms $ "Оставшееся время: " ++ (show $ time state)]
    , tr_ []
      [ td_ [] [button_ [onClick $ Finish] [text "Закончить"]]
      , td_ [] [button_ [onClick $ Next] [text "Далее"]]
      ]
    ]

checkString :: String -> String
checkString "" = "0"
checkString str = str
