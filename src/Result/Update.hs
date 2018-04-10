module Result.Update where

import Prelude
import Miso
import Miso.String

import State

data Action
  = NoOp
  | Restart
  | Click Int
  | Ok Int

update :: Action -> State -> Effect Action State
update (Click n) state = noEff $ state {isFocus = Just n}
update (Ok n) state = noEff $ state {isFocus = Nothing}
update _ state = noEff state
