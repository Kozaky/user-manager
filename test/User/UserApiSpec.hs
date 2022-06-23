module User.UserAPISpec where

import User.UserAPI (UserAPI)
import Data.Either (isRight)
import Data.Functor.Identity (Identity (Identity))
import qualified Data.Text as T
import Utils (withApp)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant (Proxy (Proxy))
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
import User.UserTypes (CreateUserReq, Request (Request), UserDTO, mkEmail)
import Prelude hiding (id)

mkUserReq :: T.Text -> T.Text -> T.Text -> IO CreateUserReq
mkUserReq name emailText password =
  case mkEmail emailText of
    Just email ->
      return $ Request (Identity name) (Identity email) (Identity password)
    Nothing -> fail "Incorrect email"

userAPI :: Proxy UserAPI
userAPI = Proxy

createUser :: Request Identity -> ClientM T.Text
getUser :: T.Text -> ClientM UserDTO
editUser :: T.Text -> Request Maybe -> ClientM NoContent
createUser :<|> getUser :<|> editUser = client userAPI

spec :: Spec
spec = do
  -- `around` will start our Server before the tests and turn it off after
  around withApp $ do
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost/"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    -- testing scenarios start here
    describe "POST /user Integration Tests" $ do
      it "Create an user and then retrieve it" $ \port -> do
        userReq <- mkUserReq (T.pack "test") (T.pack "test@test.com") (T.pack "test")
        resCreateUser <- runClientM (createUser userReq) (clientEnv port)
        isRight resCreateUser `shouldBe` True
        let (Right id) = resCreateUser

        resRetrieveUser <- runClientM (getUser id) (clientEnv port)
        isRight resRetrieveUser `shouldBe` True
      it "Creating two users with same email should fail" $ \port -> do
        userReq1 <- mkUserReq (T.pack "test") (T.pack "test1@test.com") (T.pack "test")
        resCreateUser1 <- runClientM (createUser userReq1) (clientEnv port)
        isRight resCreateUser1 `shouldBe` True

        resCreateUser <- runClientM (createUser userReq1) (clientEnv port)
        isRight resCreateUser `shouldBe` False
