module Program.Render where


import Miso
import Miso.String
import qualified Program.State as State
import qualified Settings.Render as SettingsRender
import qualified Program.Update as Update

render :: State.State -> View Update.Action
render state =
  case State.screen state of
    State.SettingsScreen settingsState -> fmap Update.SettingsScreen $ SettingsRender.render settingsState
