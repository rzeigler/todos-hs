{-# LANGUAGE OverloadedStrings, NamedFieldPuns, DataKinds, TypeOperators #-}

module Api
  ( app
  )
where

import Data.Maybe (fromMaybe)
import Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Text                      ( Text )
import           Data.Aeson
import qualified Db                            as D
import           Db                             ( todoName
                                                , todoCompleted
                                                )

import           Servant
import           Servant.API


newtype ApiTodo = ApiTodo D.Todo

instance FromJSON ApiTodo where
  parseJSON = withObject "Todo" $ \v -> fmap
    ApiTodo
    (   D.Todo
    <$> (v .: "name")
    <*> fmap (fromMaybe False) (v .:? "completed")
    )

newtype ApiSavedTodo = ApiSavedTodo D.SavedTodo

instance ToJSON ApiSavedTodo where
  toJSON (ApiSavedTodo (D.SavedTodo (id, D.Todo { todoName, todoCompleted })))
    = object
      [ "id" .= id
      , "name" .= todoName
      , "completed" .= todoCompleted
      ]

data CompleteState = All | Complete | Incomplete

parseCompleteState :: Text -> Either Text CompleteState
parseCompleteState "all"        = Right All
parseCompleteState "complete"   = Right Complete
parseCompleteState "incomplete" = Right Incomplete
parseCompleteState s            = Left s

instance FromHttpApiData CompleteState where
  parseQueryParam = parseCompleteState

type TodoAPI
  = "todos" :> QueryParam "complete" CompleteState :> Get '[JSON] [ApiSavedTodo]
    :<|> "todos" :> ReqBody '[JSON] ApiTodo :> Post '[JSON] ApiSavedTodo
    :<|> "todos" :> Capture "todoid" Int :> Get '[JSON] ApiSavedTodo
    :<|> "todos" :> Capture "todoid" Int :> ReqBody '[JSON] ApiTodo :> Put '[JSON] ApiSavedTodo


todoAPI :: Proxy TodoAPI
todoAPI = Proxy

server :: D.Db -> Server TodoAPI
server db = liftIO . getTodos 
    :<|> liftIO . postTodo
    :<|> getTodo
    :<|> putTodo
  where
    getTodos state = (fmap . fmap) ApiSavedTodo (D.runDb db invoke)
        where
            invoke = case state of
                            Nothing         -> D.getAllTodos
                            Just All        -> D.getAllTodos
                            Just Complete   -> D.getCompleteTodos
                            Just Incomplete -> D.getPendingTodos
                            
    postTodo (ApiTodo body) = ApiSavedTodo <$> D.runDb db (D.createTodo body)
    getTodo id = 
        Handler $ ExceptT $ (maybeTo404 . fmap ApiSavedTodo) <$> D.runDb db (D.getTodo id)
    putTodo id (ApiTodo body) = Handler $ ExceptT $ (maybeTo404 . fmap ApiSavedTodo) <$> D.runDb db (D.putTodo id body)

maybeTo404 :: Maybe a -> Either ServerError a
maybeTo404 (Just a) = Right a
maybeTo404 Nothing = Left err404

app :: D.Db -> Application
app db = serve todoAPI (server db)
