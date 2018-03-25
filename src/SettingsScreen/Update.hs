

module SettingsScreen.Update where

import Miso
import Miso.String
import Data.Time.Clock

import SettingsScreen.State

data Action
  = SetNumber Int
  | SetSeason Season
  | SetTime Int
  | SetPrice Float
  | SetFond Double
  | Next
  | Check
  | NoOp
  deriving (Show, Eq)

update :: Action -> State -> Effect Action State
update Check state = state <# do
  putStrLn "Hello World" >> pure NoOp
update (SetNumber n) state = noEff $ state {number = n}
update (SetSeason s) state = noEff $ state {season = s}
update (SetTime t) state = noEff $ state {time = t}
update (SetPrice p) state = noEff $ state {price = p}
update (SetFond f) state = noEff $ state {fond = f}
update NoOp state = noEff state
