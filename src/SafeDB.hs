{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Exception


data Connection
data Pool
data Transaction

--
-- low-level database functions
--
begin :: Connection -> IO Transaction
begin = undefined

commit :: Transaction -> IO ()
commit = undefined

rollback :: Transaction -> IO ()
rollback = undefined

query :: Connection -> String -> IO [String]
query = undefined

withTxn :: Connection -> IO a -> IO a
withTxn conn act = do
  txn <- begin conn
  r <- act `onException` rollback txn
  commit txn
  return r

getConn :: Pool -> IO Connection
getConn = undefined

returnConn :: Pool -> Connection -> IO ()
returnConn = undefined

withConn :: Pool -> (Connection -> IO a) -> IO a
withConn pool act =
    bracket (getConn pool) (returnConn pool) act

--
--

newtype DB c a = DB {
      fromDB :: IO a
    }

newtype SafeConn c = Safe Connection

safeQuery :: SafeConn c -> String -> DB c [String]
safeQuery (Safe conn) str = DB (query conn str)

withSafeConn :: Pool 
             -> (forall c. SafeConn c -> DB c a) 
             -> IO a
withSafeConn pool act =
  withConn pool $ \conn ->
    fromDB ( act (Safe conn))

