{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Options.Declarative        as Opt
import Library.Shell.Declarative  as Shell
import Control.Monad.IO.Class

-- Insel DSL
import Insel.Environment (intoResource)

greet :: Opt.Flag "g" '["greet"] "STRING" "greeting message" (Opt.Def "Hello" String)
      -> Opt.Flag "" '["decolate"] "" "decolate message" Bool
      -> Opt.Arg "NAME" String
      -> Opt.Cmd "Greeting command" ()
greet msg deco name = do
    let f x | get deco = "*** " ++ x ++ " ***"
            | otherwise = x
    liftIO $ putStrLn $ f $ get msg ++ ", " ++ get name ++ "!"

connect :: Opt.Flag "h" '["host"] "HOST" "host name"   (Opt.Def "localhost" String)
        -> Opt.Flag "p" '["port"] "PORT" "port number" (Opt.Def "8080"      Int   )
        -> Opt.Cmd "Connect command" ()
connect host port = do
    let addr = get host ++ ":" ++ show (get port)
    liftIO $ putStrLn $ "connect to " ++ addr


into :: Opt.Arg "RESOURCE" String
     -> Opt.Cmd "Change state to connect other resource" ()
into resource = do
  liftIO $ intoResource (get resource)

-- insel command overview
--
-- お遊びで作る。実用性とかはとりあえず考えない。(<- 重要)
-- 最初にいるのはinsel
-- insel into (resource) で対象resourceを決定する.
-- insel outof (resource) で対象resourceから離れinselに戻る.
--
-- コマンドは1発ずつのプログラム実行に対応するので, Stateの保持には何かしらファイルを作らないとね.
-- このStateの取り回し, IOまでの表現力いらないからFreeMonadで小さめに閉じたいな.
--
-- subcommand
--   各コンポーネント(API)に対する接続: into (= connection establishment =/ get, post, ...)
-- subsubcommand
--   各コンポーネント名 (resources): erst, zweit, dritt
--
-- insel (execute) でinselで入った対象に対して可能なOperationを実行する.
--   状態がinselなら, --helpをだす (into, outof)
--   状態がinsel以外なら, 対応するexecutionでhogehogeする.
--   --   execute: post, get, file open, ...
--
--
-- intoのresourceが決まっていればOpenUnionみたいなので固定してしまってもいいかも
-- できるのか？型レベル文字列のOpenUnionでコンパイル処理みたいなの


-- -- 何が嬉しいか全くわからないコマンドが出来上がった
-- ls ::    Shell.Command '["ls"] ()
--       -> Shell.Flag "lat" ()
--       -> String
-- ls c f = cmd c ++ " " ++ "-" ++ flag f
--
--
-- getPodWide ::    Shell.Command '["kubectl"] ()
--               -> Shell.SubCommand "get"  ()
--               -> Shell.SubCommand "pod"  ()
--               -> Shell.Flag       "o"    ()
--               -> Shell.Argument   "wide" ()
--               -> String
-- getPodWide c s1 s2 f a = Shell.cmd c  ++ " "
--                       ++ Shell.sub s1 ++ " "
--                       ++ Shell.sub s2 ++ " -"
--                       ++ Shell.flag f ++ " "
--                       ++ Shell.arg a

main :: IO ()
main = putStrLn $ unwords $ reverse $ Shell.cmd interpCmd
  where
    -- interpCmd  = interp command subcommand
    interpCmd  = (interp (interp (interp (interp kubectl get) pod) o) wide)
    kubectl    = Shell.Command    () :: Shell.Command    '["kubectl"] ()
    get        = Shell.SubCommand () :: Shell.SubCommand "get"        ()
    pod        = Shell.SubCommand () :: Shell.SubCommand "pod"        ()
    o          = Shell.Flag       () :: Shell.Flag       "o"          ()
    wide       = Shell.Argument   () :: Shell.Argument   "wide"       ()

-- main = do
--   putStrLn $ ls cmd flag
--   putStrLn $ getPodWide kube get pod o wide
--   where
--     cmd  = Shell.Command    () :: Shell.Command    "ls"      ()
--     flag = Shell.Flag       () :: Shell.Flag       "lat"     ()
--     kube = Shell.Command    () :: Shell.Command    "kubectl" ()
--     get  = Shell.SubCommand () :: Shell.SubCommand "get"     ()
--     pod  = Shell.SubCommand () :: Shell.SubCommand "pod"     ()
--     o    = Shell.Flag       () :: Shell.Flag       "o"       ()
--     wide = Shell.Argument   () :: Shell.Argument   "wide"    ()

-- main = run_ $
--     Opt.Group "Insel commmand controlls the resource connection and executions"
--     [ Opt.subCmd "into" into,
--       Opt.subCmd "subcmd" $ Opt.Group "subcommand" [
--         Opt.subCmd "greet" greet,
--         Opt.subCmd "connect" connect
--       ]
--     ]

