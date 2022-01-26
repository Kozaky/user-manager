module Logger (mkLogger) where

import Colog (LogAction, Message, Msg (Msg), cfilter, richMessageAction)
import Conferer (Config, fetchFromConfig)
import Foundation (App)
import GHC.IO.Handle (hSetBuffering)
import System.IO (BufferMode (LineBuffering), stderr, stdout)
import Prelude hiding (log)

mkLogger :: Config -> IO (LogAction App Message)
mkLogger cfg = do
  setupLogBuffers

  logLevel <- Conferer.fetchFromConfig "logger.level" cfg
  return $ cfilter (\(Msg sev _ _) -> sev >= read logLevel) richMessageAction

setupLogBuffers :: IO ()
setupLogBuffers = hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
