

module Settings.Update where

import Prelude
import Miso
import Miso.String
import Data.Time.Clock
import Data.List.Index

import State

data Action
  = SetNumber Int
  | SetSeason Season
  | SetTime Int
  | SetPrice Float
  | SetFond Double
  | SetPopulation Int Float
  | SetSick Int Float
  | SetVaccine Int Float
  | SetTraffic Int Float
  | Check
  | Next
  | NoOp
  deriving (Show, Eq)

update :: Action -> State -> Effect Action State
update Check state = state <# do
  putStrLn "Hello World" >> pure NoOp
update (SetNumber n) state = state {number = n, cities = setCities (cities state) n} <# do
  putStrLn (show n) >> pure NoOp
update (SetSeason s) state = noEff $ state {season = s}
update (SetTime t) state = noEff $ state {time = t}
update (SetPrice p) state = noEff $ state {price = p}
update (SetFond f) state = noEff $ state {fond = f * 1000}
update (SetPopulation n p) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {population = (clamp 0 15 p) * 1000}) else c) (cities state))}
update (SetSick n s) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {sick = clamp 0 1 s}) else c) (cities state))}
update (SetVaccine n p) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {vaccine = clamp 0 1 p}) else c) (cities state))}
update (SetTraffic n t) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {trafficIn = clamp 0 1 t}) else c) (cities state))}
update Next state = noEff state
update NoOp state = noEff state

setCities :: [City] -> Int -> [City]
setCities cities n =
  if m <= n then
    cities ++ (fmap (\_ -> initDefaultCity) [m..(n - m - 1)])
  else
    Prelude.take n cities
  where
    m = Prelude.length cities
