module API.UserSpec where

import API.User (UserAPI)
import App (app)
import qualified Conferer
import Context (Context (Context))
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either (isRight)
import Data.Functor.Identity (Identity (Identity))
import qualified Data.Text as T
import Database (mkPool)
import Error.Types (ApiError (errorMessage))
import Logger (mkLogger)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application, Proxy (Proxy))
import Servant.API (NoContent, type (:<|>) ((:<|>)))
import Servant.Client
  ( BaseUrl (baseUrlPort),
    ClientM,
    client,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
  )
import Test.Hspec (Spec, around, describe, it, runIO, shouldBe)
import Types.User (CreateUserReq, Request (Request), UserDTO, mkEmail)
import Prelude hiding (id)

mkApp :: IO Application
mkApp = do
  cfg <- Conferer.mkConfig $ T.pack ""
  pool <- mkPool cfg
  logger <- mkLogger cfg

  let ctx = Context cfg pool logger
  return $ app ctx

withApp :: (Warp.Port -> IO ()) -> IO ()
withApp = Warp.testWithApplication mkApp

mkUserReq :: T.Text -> T.Text -> T.Text -> CreateUserReq
mkUserReq name emailText password =
  case mkEmail emailText of
    Right email ->
      Request (Identity name) (Identity email) (Identity password)
    Left err -> error . LB.unpack $ errorMessage err

userApi :: Proxy UserAPI
userApi = Proxy

createUser :: Request Identity -> ClientM T.Text
getUser :: T.Text -> ClientM UserDTO
editUser :: T.Text -> Request Maybe -> ClientM NoContent
createUser :<|> getUser :<|> editUser = client userApi

spec :: Spec
spec = do
  -- `around` will start our Server before the tests and turn it off after
  around withApp $ do
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost/"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
    -- testing scenarios start here
    describe "POST /user" $ do
      it "Create an user and then retrieve it" $ \port -> do
        let userReq = mkUserReq (T.pack "test") (T.pack "test@test.com") (T.pack "test")
        resCreateUser <- runClientM (createUser userReq) (clientEnv port)
        isRight resCreateUser `shouldBe` True
        let (Right id) = resCreateUser

        resRetrieveUser <- runClientM (getUser id) (clientEnv port)
        isRight resRetrieveUser `shouldBe` True
      it "Creating two users with same email should fail" $ \port -> do
        let userReq1 = mkUserReq (T.pack "test") (T.pack "test1@test.com") (T.pack "test")
        resCreateUser1 <- runClientM (createUser userReq1) (clientEnv port)
        isRight resCreateUser1 `shouldBe` True

        resCreateUser <- runClientM (createUser userReq1) (clientEnv port)
        isRight resCreateUser `shouldBe` False