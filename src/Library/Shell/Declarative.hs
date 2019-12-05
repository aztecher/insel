{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- # 演算を考えるときに試行錯誤したやつ (いくつか消せるかも)
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Library.Shell.Declarative where

import GHC.TypeLits
import Data.Proxy

-- TODO: 現状では全く持って上長な書き方になる.
--       うまく抽象化していきたい
newtype Command (command :: [Symbol])
                a
  = Command { getCommand :: a }

-- 制約を書くために型クラスを作る。便利のためにいくつかインスタンスにする
class KnownSymbols (s :: [Symbol]) where
  symbolVals :: Proxy s -> [String]

instance KnownSymbols '[] where
  symbolVals _ = []

instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s ': ss) where
  symbolVals _ = symbolVal (Proxy :: Proxy s) : symbolVals (Proxy :: Proxy ss)

-- これとCommandの同型対応をとって, interpretationしたい気持ちがあるがどう書くんだろう?

newtype SubCommand (subcommand :: Symbol)
                   a
  = SubCommand { getSubCommand :: a }

-- CommandとSubCommandをつなぐ演算を考えてみる
class Interp f s where
  type Interpret f s :: *
  interp :: f -> s -> Interpret f s

-- TODO: 脳死で考えている
type family GenCmd c s where
  -- GenCmd (Command _c a) (SubCommand _cs a) = Command (_cs ': " " ': _c) a
  GenCmd (Command _c a) (SubCommand _cs a) = Command (_cs ': _c) a
  GenCmd (Command _c a) (Flag _f a)        = Command ("-" ': _f  ': _c) a
  GenCmd (Command _c a) (Argument _a a)    = Command (_a  ': _c) a

instance ( KnownSymbol _sc
         , KnownSymbols _c )
  => Interp (Command _c a) (SubCommand _sc a) where
  -- AppendSymbolをちょっと改変したい (間に空白文字入れたい。type familiesとかで書けないか?)
  -- type Interpret (Command _c a) (SubCommand _sc a) = Command (AppendSymbol _c _sc) a
  type Interpret (Command _c a) (SubCommand _sc a) = GenCmd (Command _c a) (SubCommand _sc a)
  interp (Command a) _ = Command a


-- Command Appendableとかいう型クラスを作って
-- 関数を書いてdefault実装として
-- CommandとSubCommand, Flag, Argなどを繋げる関数を簡単に書けないかな?

newtype Flag (flag :: Symbol)
             a
  = Flag { getFlag :: a }

instance ( KnownSymbol _f
         , KnownSymbols _c )
  => Interp (Command _c a) (Flag _f a) where
  type Interpret (Command _c a) (Flag _f a) = GenCmd (Command _c a) (Flag _f a)
  interp (Command a) _ = Command a

newtype Argument (arg :: Symbol)
                 a
  = Argument { getArgument :: a }

instance ( KnownSymbol _a
         , KnownSymbols _c )
  => Interp (Command _c a) (Argument _a a) where
  type Interpret (Command _c a) (Argument _a a) = GenCmd (Command _c a) (Argument _a a)
  interp (Command a) _ = Command a


class ShellCommand a where
  type Cmd a :: *
  cmd :: a -> Cmd a

class SubCmd a where
  type Sub a :: *
  sub :: a -> Sub a

class CmdFlag a where
  type CFlag a :: *
  flag :: a -> CFlag a

class Arg a where
  type Argu a :: *
  arg :: a -> Argu a

instance (KnownSymbols _c) => ShellCommand (Command _c a) where
  type Cmd (Command _c a) = [String]
  cmd _ = symbolVals (Proxy :: Proxy _c)

instance (KnownSymbol _sc) => SubCmd (SubCommand _sc a) where
  type Sub (SubCommand _sc a) = String
  sub _ = symbolVal (Proxy :: Proxy _sc)

instance (KnownSymbol _f) => CmdFlag (Flag _f a) where
  type CFlag (Flag _f a) = String
  flag _ = symbolVal (Proxy :: Proxy _f)

instance (KnownSymbol _a) => Arg (Argument _a a) where
  type Argu (Argument _a a) = String
  arg _  = symbolVal (Proxy :: Proxy _a)
