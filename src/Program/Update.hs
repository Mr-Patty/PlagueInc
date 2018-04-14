module Program.Update where

import Miso
import Miso.String
import Control.Monad
import Control.Concurrent

import qualified State as State
import qualified Program.State as PState
import qualified Settings.Update as SetUpdate
import qualified Simulation.Update as SimUpdate
import qualified Result.Update as ResUpdate

data Action =
  SettingsScreen SetUpdate.Action
  | SimulationScreen SimUpdate.Action
  | ResultScreen ResUpdate.Action
  | GoToSettingsScreen
  | GoToSimulationScreen
  | NextDay
  | Check
  | NoOp

update :: Action -> PState.State -> Effect Action PState.State
update (NextDay) state =
  case PState.screen state of
    PState.SimulationScreen simState ->
      if (State.time simState) - (State.startTime simState) <= 0 then
        noEff $ state {PState.screen = PState.ResultScreen simState}
      else
        if (State.startWeek simState) then
          let (Effect m a) = SimUpdate.update SimUpdate.NextDay simState
          in noEff $ state {PState.screen = PState.SimulationScreen m}
        else noEff state
    _ -> noEff state

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

update (SimulationScreen SimUpdate.Finish) state =
  case (PState.screen state) of
    PState.SimulationScreen simState ->
      if (State.time simState) - (State.startTime simState) <= 0 then
        noEff $ state {PState.screen = PState.ResultScreen simState}
      else
        let (Effect m a) = SimUpdate.update SimUpdate.NextDay simState
        in state {PState.screen = PState.SimulationScreen m} <# do
          threadDelay 100000
          return (SimulationScreen SimUpdate.Finish)
    otherwise -> noEff state

update (SimulationScreen action) state =
  case (PState.screen state) of
    PState.SimulationScreen simulationScreen ->
      let (Effect m a) = SimUpdate.update action simulationScreen
      in noEff $ state {PState.screen = PState.SimulationScreen m}
    _ -> noEff state

update (ResultScreen ResUpdate.Restart) state =
  case (PState.screen state) of
    PState.ResultScreen resState ->
      noEff $ state {PState.screen = PState.SettingsScreen newState}
      where
        newGen = (State.gen resState)
        newState = (State.initDefault newGen)
    _ -> noEff state

update (ResultScreen action) state =
  case (PState.screen state) of
    PState.ResultScreen resState ->
      let (Effect m a) = ResUpdate.update action resState
      in noEff $ state {PState.screen = PState.ResultScreen m}
    _ -> noEff state

update Check state = state <# do
  putStrLn "Hello World" >> pure NoOp
update NoOp state = noEff state


-- (finishExperiment simState)
