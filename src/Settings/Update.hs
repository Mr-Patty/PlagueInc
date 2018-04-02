

module Settings.Update where

import Miso
import Miso.String
import Data.Time.Clock

import Settings.State

data Action
  = SetNumber Int
  | SetSeason Season
  | SetTime Int
  | SetPrice Float
  | SetFond Double
  | SetPopulation Int Int
  | SetSeek Double Int
  | SetPriv Double Int
  | SetTrans Float Int
  | Check
  | Next
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
update (SetPopulation p n) state = noEff state
update NoOp state = noEff state
