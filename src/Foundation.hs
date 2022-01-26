module Foundation where

import Context (Ctx)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import UnliftIO (MonadUnliftIO)

-- | Data type representing the most general effects our application should be
-- able to perform.
newtype App a = App {unApp :: ReaderT (Ctx App) IO a}
  deriving newtype
    ( Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader (Ctx App),
      Functor
    )
