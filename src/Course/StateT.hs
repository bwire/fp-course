{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a = StateT { runStateT :: s -> f (a, s) }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) :: (a -> b) -> StateT s f a -> StateT s f b
  f <$> StateT st = StateT $ (<$>) (\(v, s') -> (f v, s')) . st
  -- suggested approach uses Arrow!!!
  --f <$> StateT st = StateT $ (<$>) (first f) . st

    
-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure :: a -> StateT s f a
  pure v = StateT $ \s -> pure (v, s) 
    
  (<*>) :: StateT s f (a -> b) -> StateT s f a -> StateT s f b
  StateT sf <*> StateT sv = StateT $ \s -> 
     (\(f, s') -> (\(v, s'') -> pure (f v, s'')) =<< sv s') =<< sf s
  
  -- suggested approach (with Arrow)
  --(<*>) :: StateT s f (a -> b) -> StateT s f a -> StateT s f b
  --StateT sf <*> StateT sv = StateT $ (\(f, s') -> first f <$> sv s') <=< sf
    

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) :: (a -> StateT s f b) -> StateT s f a -> StateT s f b
  k =<< StateT m = StateT $ (=<<) (\(v, s') -> runStateT (k v) s') . m
  
  -- with do-notation
  -- k =<< StateT m = StateT $ \s -> do
    -- (v, s') <- m s
    -- runStateT (k v) s'
   
-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a = StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' :: (s -> (a, s)) -> State' s a
state' k = StateT $ ExactlyOne . k

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: State' s a -> s -> (a, s)
runState' (StateT st) = runExactlyOne . st

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT :: Functor f => StateT s f a -> s -> f s
execT (StateT st) = (<$>) snd . st

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' :: State' s a -> s -> s
exec' st = runExactlyOne . execT st

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT :: Functor f => StateT s f a -> s -> f a
evalT (StateT st) = (<$>) fst . st

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' :: State' s a -> s -> a
eval' st = runExactlyOne . evalT st

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: Monad f => StateT s f s
getT = StateT $ pure . join (,)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: Monad f => s -> StateT s f ()
putT st = StateT $ \_ -> pure ((), st)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' :: (Ord a, Num a) => List a -> List a
distinct' xs = eval' (filtering (\a -> state' (S.notMember a &&& S.insert a)) xs) S.empty
-- initial variant was
--distinct' xs = eval' (filtering (\a -> state' $ lift2 (&&&) S.notMember S.insert a) xs) S.empty


-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF :: (Ord a, Num a) => List a -> Optional (List a)
-- this approach isn't optimal, couse it initially gets distinct list and then filters it and traverse to get the final result 
distinctF xs = 
  let 
    xs' = eval' (filtering (\a -> state' (S.notMember a &&& S.insert a)) xs) S.empty
    xs'' = map (\e -> if e > 100 then Empty else Full e) xs'
  in seqOptional xs''  

-- suggested variant is much better. H ere the row StateT monad is used
distinctF' :: (Ord a, Num a) => List a -> Optional (List a)
distinctF' xs = evalT (filtering (\a -> StateT (\s -> if a > 100 then Empty else Full (S.notMember a s, S.insert a s))) xs) S.empty
    

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a = OptionalT { runOptionalT :: f (Optional a)}

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  f <$> (OptionalT fr) = OptionalT ((<$>) f <$> fr)

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Applicative f => Applicative (OptionalT f) where
  pure = OptionalT . pure . Full
  (OptionalT frf) <*> (OptionalT frv) = OptionalT $ lift2 (<*>) frf frv 

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  k =<< OptionalT m = OptionalT $ do
    oa <- m
    case oa of
      Full a -> runOptionalT (k a)
      Empty -> return Empty 

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a = Logger (List l) a deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  f <$> (Logger l v) = Logger l (f v)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure v = Logger Nil v 
  Logger l1 f <*> Logger l2 v = Logger (l1 ++ l2) (f v) 

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  k =<< Logger l v = 
    let Logger l' v' = k v
    in Logger (l ++ l') v' 

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: l -> a -> Logger l a
log1 l = Logger (l :. Nil)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG :: (Integral a, Show a) => List a -> Logger Chars (Optional (List a))
distinctG xs = 
  let ff = filtering (\a -> StateT (\s -> OptionalT $ logger a s)) xs
      logger a s = 
        if a > 100 then
          Logger (listh ["aborting > 100: " ++ (show' a)]) Empty
        else Logger (
          if even a 
          then listh ["even number " ++ (show' a)] 
          else Nil) (Full (S.notMember a s, S.insert a s))
  in runOptionalT $ evalT ff S.empty
  