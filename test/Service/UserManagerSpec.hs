module Service.UserManagerSpec where

import Control.Monad.State (StateT (runStateT), gets, modify)
import Data.Bson (Document, ObjectId, Value, merge, val, (=:))
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (Identity (Identity))
import qualified Data.Map as M
import Repo.User (UserRepository (..))
import Service.MongoDbManager (DbFailure)
import Service.UserManager (createUser, editUser, getUser)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Types.User (CreateUserReq, EditUserReq, Request (..), mkEmail, pattern Email)

newtype TestDb m a = TestDb (StateT (M.Map ObjectId Document) m a)
  deriving newtype (Functor, Applicative, Monad)

runTestDb :: (Monad m) => M.Map ObjectId Document -> TestDb m a -> m a
runTestDb s (TestDb x) = fst <$> runStateT x s

instance (Monad m) => UserRepository (TestDb m) where
  save :: Document -> TestDb m Value
  save doc = do
    let objId = read "62a8d83ed5cd0a07f1000003"
    TestDb . modify $ \users -> M.insert objId doc users
    return $ val objId

  find :: ObjectId -> TestDb m (Maybe Document)
  find objId = TestDb . gets $ \users -> M.lookup objId users

  update :: ObjectId -> EditUserReq -> TestDb m (Either DbFailure ())
  update objId req = do
    TestDb . modify $ \users -> M.update (mergeWithOld req) objId users
    return $ Right ()
    where
      mergeWithOld :: EditUserReq -> Document -> Maybe Document
      mergeWithOld newDoc previousDoc = Just $ merge previousDoc (mkUpdates newDoc)

      mkUpdates :: EditUserReq -> Document
      mkUpdates (Request name email password) =
        maybe [] (\v -> ["name" =: v]) name
          ++ maybe [] (\(Email v) -> ["email" =: v]) email
          ++ maybe [] (\v -> ["password" =: v]) password

spec :: Spec
spec = do
  describe "User Manager" $ do
    it "Create user" $ do
      case mkEmail "email@email.com" of
        Nothing -> fail "Incorrect email"
        Just email -> do
          let req =
                Request
                  { name = Identity "hola",
                    email = Identity email,
                    password = Identity "hola"
                  }

          res <- runTestDb M.empty $ createUser req
          res `shouldBe` "62a8d83ed5cd0a07f1000003"

    it "Find user" $ do
      req <- givenCreateUserReq

      res <- runTestDb M.empty $ createUser req >>= getUser
      res `shouldSatisfy` isRight

    it "Find user - Not Found" $ do
      res <- runTestDb M.empty $ getUser "62a8d83ed5cd0a07f1000003"
      res `shouldSatisfy` isLeft

    it "Edit user" $ do
      let editReq =
            Request
              { name = Just "newName",
                email = Nothing,
                password = Nothing
              }

      res <- runTestDb M.empty $ editUser "62a8d83ed5cd0a07f1000003" editReq

      res `shouldSatisfy` isRight

givenCreateUserReq :: IO CreateUserReq
givenCreateUserReq =
  case mkEmail "email@email.com" of
    Nothing -> fail "Incorrect email"
    Just email ->
      return
        Request
          { name = Identity "hola",
            email = Identity email,
            password = Identity "hola"
          }
