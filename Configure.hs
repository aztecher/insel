{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}

module Insel.Configure where

import Control.Monad.Free (Free, foldFree, liftF)
import System.IO (withFile, IOMode(..), hPutStr)
import Data.Monoid ((<>))

-- CLIツールで状態情報をconfigに書き出すため、
-- Runtime I/Oでファイルを読む必要がある。
-- それに関する規模縮小版I/OとしてのDSLを作成する。
-- (Configuration DSL)

-- 大域変数を利用する ReaderT a IO
-- このStringの部分(環境の部分になる)はEnvみたいな感じにしたい
--
-- ConfigDSL概要
--   設定用DSL
--   1. 設定読み込み
--     file open (read) -> 必要な部分のパース -> 取得する
--   2. 設定書き込み
--     file open (write) -> 必要な部分の修正 -> 保存

