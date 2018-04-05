module Program.Update where

import Miso
import Miso.String

import qualified Program.State as PState
import qualified Settings.Update as SetUpdate
import qualified State as State
import qualified Simulation.Update as SimUpdate

data Action =
  SettingsScreen SetUpdate.Action
  | SimulationScreen SimUpdate.Action
  | GoToSettingsScreen
  | GoToSimulationScreen
  | Check
  | NoOp
  -- | GoToSimulationScreen
  -- | SimulationScreen

update :: Action -> PState.State -> Effect Action PState.State
update (SettingsScreen SetUpdate.Next) state =
  case (PState.screen state) of
    PState.SettingsScreen settingsScreen  ->
      noEff $ state {PState.screen = PState.SimulationScreen settingsScreen }
    _ -> noEff state
update (SettingsScreen action) state =
  case (PState.screen state) of
    PState.SettingsScreen settingsScreen ->
      let (Effect m a) = SetUpdate.update action settingsScreen
      in noEff $ state {PState.screen = PState.SettingsScreen m}
    _ -> noEff state
update (SimulationScreen action) state =
  case (PState.screen state) of
    PState.SimulationScreen simulationScreen ->
      let (Effect m a) = SimUpdate.update action simulationScreen
      in noEff $ state {PState.screen = PState.SimulationScreen m}
    _ -> noEff state
update Check state = state <# do
  putStrLn "Hello World" >> pure NoOp
update NoOp state = noEff state
