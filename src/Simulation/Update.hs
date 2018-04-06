module Simulation.Update where

import Miso
import Miso.String
import Data.List.Index

import State

data Action
  = Next
  | SetInject Int Float
  | Ok Int
  | Click Int
  | Finish
  | NoOp

update :: Action -> State -> Effect Action State
update (NoOp) state = noEff $ state
update (Click n) state = noEff $ state {change = Just n}
update (Ok n) state = noEff $ state {change = Nothing}
update (SetInject n priv) state = noEff $
  state {cities = (imap (\i c -> if i == n then (c {priv = priv}) else c) (cities state))}
update _ state = noEff state
