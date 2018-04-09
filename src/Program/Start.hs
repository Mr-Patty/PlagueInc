-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Program.Start where

  -- | Miso framework import
import Miso
import Miso.String
import Miso.Subscription
import Data.Time.Clock
import System.Random
import Control.Monad
import Control.Concurrent

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
    initialAction = Update.Check
    update        = Update.update   -- update function
    view          = Render.render     -- view function
    events        = defaultEvents -- default delegated events
    subs          = [ timeSub ]
    mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')

timeSub :: Sub Update.Action
timeSub sink = do
  _ <- forkIO $ timeUpdate sink
  return ()

timeUpdate :: Sub Update.Action
timeUpdate sink = forever $ do
  threadDelay 100000
  sink Update.NextDay >> return ()
