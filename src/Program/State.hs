module Program.State where

import Data.Time.Clock
import System.Random
-- import qualified Settings.State as Settings
import qualified State as State

data State = State
  {
  screen :: Screen
  } deriving(Eq, Show)

data Screen
  = SettingsScreen State.State
  | SimulationScreen State.State
  | ResultScreen
  deriving(Eq, Show)

initDefault :: StdGen -> State
initDefault time = State
  {
  screen = SettingsScreen (State.initDefault time)
  }
