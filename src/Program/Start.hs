-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Program.Start where

  -- | Miso framework import
import Miso
import Miso.String
import Data.Time.Clock
import System.Random

import qualified Program.State as State
import qualified Program.Update as Update
import qualified Program.Render as Render


start :: IO ()
start = do
    t2 <- getCurrentTime
    gen <- newStdGen
    let model = State.initDefault gen
    startApp App { model = model, ..}
  where
    initialAction = Update.Check -- initial action to be executed on application load
    -- model         = initDefault t2             -- initial model
    update        = Update.update   -- update function
    view          = Render.render     -- view function
    events        = defaultEvents -- default delegated events
    subs          = []            -- empty subscription list
    mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')
