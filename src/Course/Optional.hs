{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a = Full a | Empty deriving (Eq, Show)

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional f (Full a) = Full $ f a
mapOptional _ Empty = Empty

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional k (Full v) = k v
bindOptional _ Empty = Empty 

-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
(??) :: Optional a -> a -> a
Full x ?? _ = x
Empty ?? v = v 

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) :: Optional a -> Optional a -> Optional a
Full x <+> _ = Full x
_ <+> l = l

applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f = bindOptional (\v' -> mapOptional (\f' -> f' v') f)

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional = (applyOptional . ) . mapOptional

contains :: Eq a => a -> Optional a -> Bool
contains v (Full x) = (v == x)
contains _ Empty = False 

instance P.Functor Optional where
  fmap = M.liftM

instance A.Applicative Optional where
  (<*>) = M.ap
  pure = Full

instance P.Monad Optional where
  (>>=) = flip bindOptional
  return = Full
