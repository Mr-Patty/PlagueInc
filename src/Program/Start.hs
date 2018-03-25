-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Program.Start where

  -- | Miso framework import
import Miso
import Miso.String
import Data.Time.Clock

import qualified SettingsScreen.State as State
import qualified SettingsScreen.Update as Update
import qualified SettingsScreen.Render as Render


start :: IO ()
start = do
    t2 <- getCurrentTime
    let model = State.initDefault t2
    startApp App { model = model, ..}
  where
    initialAction = Update.Check -- initial action to be executed on application load
    -- model         = initDefault t2             -- initial model
    update        = Update.update   -- update function
    view          = Render.render     -- view function
    events        = defaultEvents -- default delegated events
    subs          = []            -- empty subscription list
    mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')
