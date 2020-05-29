{-# LANGUAGE OverloadedStrings #-}
module Todos
  ( mainImpl
  )
where

import           Prelude                 hiding ( putStrLn )

import           Db
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.IO                   ( putStrLn )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.Clock.System         ( getSystemTime
                                                , systemToUTCTime
                                                )

getNow = fmap systemToUTCTime getSystemTime

showText :: Show a => a -> Text
showText = pack . show

mainImpl :: IO ()
mainImpl = do
  handler <- withInitializedDb "todos.sqlite"
  now     <- getNow
  let todo = Todo { todoName      = "#1"
                  , todoDesc      = "First Todo"
                  , todoCreated   = now
                  , todoCompleted = Nothing
                  }
  created <- (runHandler handler) (createTodo todo)
  let id = savedTodoId created
  reloaded <- (runHandler handler) (getTodo id)
  putStrLn $ showText created
  putStrLn $ showText reloaded

