module Simulation.Update where

import Miso
import Miso.String

import State

data Action
  = Next
  | Finish

update :: Action -> State -> Effect Action State
update action state = noEff state
