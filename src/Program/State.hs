module Program.State where

import Data.Time.Clock
import qualified Settings.State as Settings

data State = State
  {
  screen :: Screen
  } deriving(Eq, Show)

data Screen
  = SettingsScreen Settings.State
  -- | SimulationScreen
  -- | EndScreen
  deriving(Eq, Show)

initDefault :: UTCTime -> State
initDefault time = State
  {
  screen = SettingsScreen (Settings.initDefault time)
  }
