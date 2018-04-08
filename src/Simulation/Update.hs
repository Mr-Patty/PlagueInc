module Simulation.Update where

import Miso
import Miso.String
import Data.List.Index

import State

data Action
  = Next
  | SetInject Int Int
  | Ok Int
  | Click Int
  | Finish
  | NoOp

update :: Action -> State -> Effect Action State
update (NoOp) state = noEff $ state
update (Click n) state = noEff $ state {isFocus = Just n}
update (Ok n) state = noEff $ state {isFocus = Nothing}
update (SetInject n vaccine) state = noEff $
  state {cities = (imap (\i c -> if i == n then (c {vaccine = Just vaccine}) else c) (cities state))}
update _ state = noEff state
