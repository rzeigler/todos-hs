{-# LANGUAGE OverloadedStrings #-}

module Todos
  ( mainImpl
  )
where

import           Network.Wai.Handler.Warp
import qualified Data.Aeson                    as A
import           Data.Aeson.Text
import           Db
import           Api
import           Servant

mainImpl :: IO ()
mainImpl = do
  putStrLn "Starting on 3030"
  db <- withInitializedDb "todos.sqlite"
  run 3030 (app db)
