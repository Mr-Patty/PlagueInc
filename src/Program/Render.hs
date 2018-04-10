module Program.Render where


import Miso
import Miso.String
import qualified Program.State as State
import qualified Program.Update as Update
import qualified Settings.Render as SettingsRender
import qualified Simulation.Render as SimRender
import qualified Result.Render as ResRender

render :: State.State -> View Update.Action
render state =
  case State.screen state of
    State.SettingsScreen settingsState -> fmap Update.SettingsScreen $ SettingsRender.render settingsState
    State.SimulationScreen simunaltionState -> fmap Update.SimulationScreen $ SimRender.render simunaltionState
    State.ResultScreen resState ->  fmap Update.ResultScreen $ ResRender.render resState
