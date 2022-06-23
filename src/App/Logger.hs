module App.Logger (mkLogger) where

import Colog (LogAction, Message, Msg (Msg), cfilter, richMessageAction)
import Conferer (Config, fetchFromConfig)
import GHC.IO.Handle (hSetBuffering)
import System.IO (BufferMode (LineBuffering), stderr, stdout)
import UnliftIO (MonadIO)
import Prelude hiding (log)

mkLogger :: (Applicative m, MonadIO m) => Config -> IO (LogAction m Message)
mkLogger cfg = do
  setupLogBuffers

  logLevel <- Conferer.fetchFromConfig "logger.level" cfg
  return $ cfilter (\(Msg sev _ _) -> sev >= read logLevel) richMessageAction

setupLogBuffers :: IO ()
setupLogBuffers = hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
