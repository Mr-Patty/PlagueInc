module Program.Update where

import Miso
import Miso.String

import qualified Program.State as State
import qualified Settings.Update as SetUpdate
import qualified Settings.State as SettingsState

data Action =
  SettingsScreen SetUpdate.Action
  | GoToSettingsScreen
  | Check
  | NoOp
  -- | GoToSimulationScreen
  -- | SimulationScreen

update :: Action -> State.State -> Effect Action State.State
update (SettingsScreen action) state =
  case (State.screen state) of
    State.SettingsScreen settingsScreen ->
        noEff $ state
      -- noEff $ state {State.screen = State.SettingsScreen newSettingScreenState}
    -- Update.update action settingsScreen
    _ -> noEff state
-- update GoToSettingsScreen state =
update Check state = state <# do
  putStrLn "Hello World" >> pure NoOp
update NoOp state = noEff state
