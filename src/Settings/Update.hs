

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
  | SetPopulation Int Int
  | SetSeek Int Double
  | SetPriv Int Float
  | SetTrans Int Float
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
update (SetFond f) state = noEff $ state {fond = f}
update (SetPopulation n p) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {population = p}) else c) (cities state))}
update (SetSeek n s) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {seek = s}) else c) (cities state))}
update (SetPriv n p) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {priv = p}) else c) (cities state))}
update (SetTrans n t) state =
    noEff $ state {cities = (imap (\i c -> if i == n then (c {trans = t}) else c) (cities state))}
update Next state = noEff state
update NoOp state = noEff state

setCities :: [City] -> Int -> [City]
setCities cities n =
  if m <= n then
    cities ++ (fmap (\_ -> initDefaultCity) [m..(n - m)])
  else
    Prelude.take n cities
  where
    m = Prelude.length cities
