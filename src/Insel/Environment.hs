{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}

module Insel.Environment  where

import Control.Monad.Free (Free, foldFree, liftF)
import System.IO (withFile, IOMode(..), hPutStr, hGetLine)
import Data.Monoid ((<>))

import Data.IORef (readIORef, writeIORef, newIORef, IORef)
import Control.Monad.Reader (ask, runReaderT, ReaderT)
import Control.Monad.State (liftIO)

-- 状態の取り回しがざっくりできるようになったので
-- Yamlのパースを行いたい
-- これは, Read/Write時に必要になる。
-- Libraryという形でAPIを作成しここから利用する形でいいのでは?
-- haskell-dhallを一応見てみる？

data Env = Env {
  location :: IORef String
}

type App = ReaderT Env IO

data InselEnv a
  = ReadLoc a
  | WriteLoc String a
  | GetState a       -- rename
  deriving (Functor)

getState :: Free InselEnv ()
getState = liftF $ GetState ()

readLoc :: Free InselEnv ()
readLoc = liftF $ ReadLoc ()

writeLoc :: String -> Free InselEnv ()
writeLoc resource = liftF $ WriteLoc resource ()

stateInterp :: InselEnv a -> App a
stateInterp (GetState next) = do
  env <- ask
  liftIO $ print =<< readIORef (location env)
  pure next
stateInterp (ReadLoc next) = do
  env <- ask
  liftIO $ do
    withFile "config/insel.conf" ReadMode $ \h -> do
      loc <- hGetLine h
      writeIORef (location env) loc
  pure next
stateInterp (WriteLoc loc next) = do
  env <- ask
  liftIO $ do
    writeIORef (location env) loc
    withFile "config/insel.conf" WriteMode $ \h -> do
      hPutStr h loc
  pure next

-- instruction sets
stateOps :: String -> Free InselEnv ()
stateOps resource = do
  readLoc
  getState
  writeLoc resource
  getState

runApp :: Free InselEnv a -> IO a
runApp f = do
  ref <- newIORef ""
  runReaderT (foldFree stateInterp f) (Env ref)

intoResource :: String -> IO ()
intoResource resource = do
  runApp $ stateOps resource
