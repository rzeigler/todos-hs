{-# LANGUAGE OverloadedStrings, QuasiQuotes, TupleSections, GeneralizedNewtypeDeriving, RankNTypes  #-}
module Db
  ( Todo(..)
  , SavedTodo
  , Handler
  , runHandler
  , withInitializedDb
  , getAllTodos
  , getCompleteTodos
  , getPendingTodos
  , createTodo
  , deleteTodo
  , getTodo
  , savedTodoId
  , savedTodoData
  )
where
import           Prelude                 hiding ( unlines )
import           Control.Monad.Extra            ( unlessM )
import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text
                                                , unlines
                                                , unpack
                                                )
import           Data.Time.Clock                ( UTCTime )
import           Database.SQLite.Simple         ( Query
                                                , Connection
                                                , Only(..)
                                                , withConnection
                                                , query_
                                                , query
                                                , execute_
                                                , execute
                                                , FromRow
                                                , fromRow
                                                , field
                                                , ToRow
                                                , toRow
                                                )
import           Database.SQLite.Simple.QQ      ( sql )

-- Queries live here

hasTodoTable = [sql|
        SELECT count(*)
        FROM sqlite_master WHERE type='table' AND name='todos'
|]

createTodoTable = [sql|
        CREATE TABLE todos (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT NOT NULL,
                desc TEXT NOT NULL,
                created_at TEXT NOT NULL,
                completed_at TEXT
        )
|]

getAll = [sql|SELECT id, name, desc, created_at, completed_at FROM todos|]

getPending =
  [sql|SELECT id, name, desc, created_at, completed_at FROM todos WHERE completed_at IS NULL|]

getComplete =
  [sql|SELECT id, name, desc, created_at, completed_at FROM todos WHERE completed_at IS NOT NULL|]

setComplete = [sql|UPDATE todos SET completed_at = ? WHERE id = ?|]

setIncomplete = [sql|UPDATE todos SET completed_at = NULL where id = ?|]

create =
  [sql|INSERT INTO todos(name, desc, created_at, completed_at) VALUES(?, ?, ?, ?)|]

delete = [sql|DELETE FROM todos WHERE id = ?|]

get =
  [sql|SELECT id, name, desc, created_at, completed_at FROM todos WHERE id = ? LIMIT 1|]

lastId = [sql|SELECT last_insert_rowid()|]

unOnly (Only a) = a

isInitialized :: Connection -> IO Bool
isInitialized conn = fmap checkInit $ query_ conn hasTodoTable
 where
  toBool    = (> 0) :: Int -> Bool
  -- Assume the head is defined or the IO will fail
  -- Its a count query, should have 1
  checkInit = toBool . unOnly . head

initialize :: Connection -> IO ()
initialize conn = execute_ conn createTodoTable

lazyInitialize :: Connection -> IO ()
lazyInitialize conn = unlessM (isInitialized conn) (initialize conn)


-- Get around error regarding impredicative polymorphism
newtype Handler = Handler { runHandler :: forall a . (Connection -> IO a) -> IO a }

withInitializedDb :: Text -> IO Handler
withInitializedDb path = fmap (const (Handler withPathConn))
  $ withConnection pathStr lazyInitialize
 where
  pathStr      = unpack path
  withPathConn = withConnection pathStr

data Todo = Todo { todoName :: Text
                 , todoDesc :: Text
                 , todoCreated :: UTCTime
                 , todoCompleted :: Maybe UTCTime }
        deriving (Eq, Show)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> field

instance ToRow Todo where
  toRow t = toRow (todoName t, todoDesc t, todoCreated t, todoCompleted t)


newtype SavedTodo = SavedTodo (Int, Todo)
        deriving (Eq, Show)

savedTodoId :: SavedTodo -> Int
savedTodoId (SavedTodo (i, _)) = i

savedTodoData :: SavedTodo -> Todo
savedTodoData (SavedTodo (_, d)) = d

instance FromRow SavedTodo where
  fromRow = fmap SavedTodo ((,) <$> field <*> fromRow)

getAllTodos :: Connection -> IO [Todo]
getAllTodos conn = query_ conn getAll

getPendingTodos :: Connection -> IO [Todo]
getPendingTodos conn = query_ conn getPending

getCompleteTodos :: Connection -> IO [Todo]
getCompleteTodos conn = query_ conn getComplete

completeTodoAt :: UTCTime -> Int -> Connection -> IO ()
completeTodoAt now i conn = execute conn setComplete (now, i)

incompleteTodo :: Int -> Connection -> IO ()
incompleteTodo i conn = execute conn setIncomplete (Only i)

createTodo :: Todo -> Connection -> IO SavedTodo
createTodo todo conn = execute conn create todo
  *> fmap reloaded (query_ conn lastId)
 where
  unOnlyInt :: Only Int -> Int
  unOnlyInt = unOnly
  reloaded  = (SavedTodo . (, todo) . unOnlyInt . head)

deleteTodo :: Int -> Connection -> IO ()
deleteTodo i conn = execute conn delete (Only i)

getTodo :: Int -> Connection -> IO (Maybe SavedTodo)
getTodo i conn = fmap listToMaybe $ query conn get (Only i)
