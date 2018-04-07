-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Simulation.Render where

import Prelude
import Miso
import Miso.String
import Miso.Html.Element
import Miso.Svg
import Miso.Svg.Element
import qualified Data.Map as M
import Data.List.Index

import State
import Simulation.Update

radius :: Float
radius = 300

wrap :: Float
wrap = 400

coefRadPop :: Float
coefRadPop = 50

render :: State -> View Action
render state =
  div_ [] [header state, towns state]

towns :: State -> View Action
towns state =
  div_
    [
    Miso.style_ $ M.fromList
      [
      -- ("margin-left", "-150px"),
      -- ("margin-top", "-150px")
      -- ("position", "absolute"),
      -- ("left", "50%"),
      -- ("top", "50%")
      ]
    ]
    $ [div_
        []
        (imap (\ i a -> oneTown (isFocus state) (Prelude.length $ cities state) i a) (cities state))]
    -- text "lol"
    -- town (cities state)
oneTown :: Maybe Int -> Int -> Int -> City -> View Action
oneTown isFocus count i city =
  div_
    [
    Miso.style_ $ M.fromList
      [
      -- ("left", ms $ "-" ++ (show $ 200 + n * 50) ++ "px"),
      -- ("top", "-200px")
        ("left", ms $ (show left) ++ "px")
      , ("top", ms $ (show top) ++ "px")
      , ("position", "absolute")
      ]
    ]
    [
    case isFocus of
      Nothing -> town city i
      (Just m) -> if m == i then changeCity city i else town city i
    ]
    where
      f = 2.0 * pi * (fromIntegral i) / (fromIntegral count)
      left = 400.0 + wrap + radius * (sin f)
      top = wrap + radius * (cos f)

changeCity :: City -> Int -> View Action
changeCity city n =
  table_ [align_ "center", Miso.width_ (ms $ show $ 2 * radius), Miso.height_ (ms $ show $ 2 * radius)]
    [ td_
      [
        align_ "center"
      ]
      [
        tr_ []
          [ div_ [] [text $ ms $ "Число привитых: " ++ (show $ vaccine city)]
          ]
      , tr_ []
          [ div_ [] [text $ ms $ "Заболевшие: " ++ (show $ sick city)]
          ]
      , tr_ []
          [ div_ [] [text "Кол-во прививок: "]
          , input_ [onChange (\m -> SetInject n (read $ checkString $ fromMisoString m))]
          ]
      , div_ [] [button_ [onClick $ Ok n] [text "Ok"]]
      ]
    ]
  where
    radius = population city * coefRadPop

town :: City -> Int -> View Action
town city n =
  svg_ [Miso.Svg.height_ (ms $ show $ 2 * radiusP), Miso.Svg.width_ (ms $ show $ 2 * radiusP), onClick (Click n) ]
    [
    circle_ [ cx_ (ms $ show $ radiusP), cy_ (ms $ show $ radiusP), r_ (ms $ radiusP), stroke_ "black", strokeWidth_ "1", fill_ "white" ] [],
    circle_ [ cx_ (ms $ show $ radiusP), cy_ (ms $ show $ radiusP), r_ (ms $ radiusS), fill_ "red" ] []
    ]
  where
    radiusP = population city * coefRadPop
    radiusS = sick city * 40

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
