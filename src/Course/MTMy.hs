import Control.Category(Category((.)))
import Control.Monad(Monad(..), (=<<))
import Data.Eq(Eq)
import Data.Foldable(foldr)
import Data.Functor(Functor(fmap))
import Data.Int(Int)
import Data.String(IsString(fromString))
import Prelude(Show, undefined, ($))
import System.IO(IO)

-- Id
data Id a = Id a deriving (Eq, Show)

bindId :: (a -> Id b) -> Id a -> Id b
bindId k (Id v) = k v

pureId :: a -> Id a
pureId = Id

sequenceID :: [Id a] -> Id [a]
sequenceID = foldr (\x ini -> bindId (\a -> bindId (\xs -> pureId (a:xs)) ini) x) (pureId [])

-- Optional
data Optional a = Empty | Full a deriving (Eq, Show)

bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty = Empty
bindOptional k (Full v) = k v

pureOptional :: a -> Optional a
pureOptional = Full

sequenceOptional :: [Optional a] -> Optional [a]
sequenceOptional = foldr (\x ini -> bindOptional (\v -> bindOptional (\xs -> pureOptional (v:xs)) ini) x) (pureOptional [])

-- IntReader
data IntReader a = IntReader (Int -> a)

bindIntReader :: (a -> IntReader b) -> IntReader a -> IntReader b
bindIntReader k (IntReader v) = IntReader $ \x -> 
  let (IntReader r) = k (v x) 
  in r x
  
pureIntReader :: a -> IntReader a
pureIntReader v = IntReader (\_ -> v)

sequenceIntReader :: [IntReader a] -> IntReader [a]
sequenceIntReader = foldr (\x ini -> bindIntReader (\v -> bindIntReader (\xs -> pureIntReader (v:xs)) ini) x) (pureIntReader [])

-- DataReader
data Reader r a = Reader (r -> a)
  
bindReader :: (a -> Reader r b) -> Reader r a -> Reader r b
bindReader k (Reader v) = Reader $ \x ->
  let Reader r = k (v x)
  in r x
  
pureReader :: a -> Reader r a
pureReader = Reader . return

sequenceReader :: [Reader r a] -> Reader r [a]
sequenceReader = foldr (\x ini -> bindReader (\v -> bindReader (\xs -> pureReader (v:xs)) ini) x) (pureReader [])

-- IntState 
data IntState a = IntState (Int -> (a, Int))

bindIntState :: (a -> IntState b) -> IntState a -> IntState b
bindIntState k (IntState f) = IntState $ \st -> 
  let (v, st') = f st
      IntState f' = k v
  in f' st'
  
pureIntState :: a -> IntState a
pureIntState v = IntState $ \st -> (v, st)

sequenceIntState :: [IntState a] -> IntState [a]
sequenceIntState = foldr (\is ini -> bindIntState (\s -> bindIntState (\xs -> pureIntState (s:xs)) ini) is) (pureIntState [])
  
-- State
data State s a = State (s -> (a, s))

bindState :: (a -> State s b) -> State s a -> State s b
bindState k (State f) = State $ \s -> 
  let (v', s') = f s
      State f' = k v'
  in f' s'
  
pureState :: a -> State s a
pureState v = State $ \s -> (v, s)

sequenceState :: [State s a] -> State s [a]
sequenceState = foldr (\is ini -> bindState (\s -> bindState (\xs -> pureState (s:xs)) ini) is) (pureState [])

-- Or 
data Or t a = This t | That a deriving (Eq, Show)

bindOr :: (a -> Or t b) -> Or t a -> Or t b
bindOr _ (This v) = This v
bindOr k (That v) = k v

pureOr :: a -> Or t a
pureOr = That

sequenceOr :: [Or t a] -> Or t [a]
sequenceOr = foldr (\eo ini -> bindOr (\e -> bindOr (\xs -> pureOr (e:xs)) ini) eo) (pureOr [])

-- ListFree
data ListFree a = ListDone a | ListMore [ListFree a] deriving (Eq, Show)

bindListFree :: (a -> ListFree b) -> ListFree a -> ListFree b
bindListFree k (ListDone v) = k v
bindListFree k (ListMore xs) = ListMore (fmap (bindListFree k) xs)

pureListFree :: a -> ListFree a
pureListFree = ListDone

sequenceListFree :: [ListFree a] -> ListFree [a]
sequenceListFree = foldr (\lf ini -> bindListFree (\x -> bindListFree (\xs -> pureListFree (x:xs)) ini) lf) (pureListFree [])

-- IntReaderFree
data IntReaderFree a = IntReaderDone a | IntReaderMore [IntReaderFree a] deriving (Eq, Show)

bindIntReaderFree :: (a -> IntReaderFree b) -> IntReaderFree a -> IntReaderFree b
bindIntReaderFree k (IntReaderDone v) = k v
bindIntReaderFree k (IntReaderMore xs) = IntReaderMore (fmap (bindIntReaderFree k) xs)

pureIntReaderFree :: a -> IntReaderFree a
pureIntReaderFree = IntReaderDone 

sequenceIntReaderFree :: [IntReaderFree a] -> IntReaderFree [a]
sequenceIntReaderFree = foldr (\lf ini -> bindIntReaderFree (\x -> bindIntReaderFree (\xs -> pureIntReaderFree (x:xs)) ini) lf) (pureIntReaderFree [])

-- ReaderFree
data ReaderFree r a = ReaderDone a | ReaderMore (Reader r (ReaderFree r a))

bindReaderFree :: (a -> ReaderFree r b) -> ReaderFree r a -> ReaderFree r b
bindReaderFree k (ReaderDone v) = k v
bindReaderFree k (ReaderMore (Reader r)) =  ReaderMore (Reader (bindReaderFree k . r))

pureReaderFree = ReaderDone

sequenceReaderFree :: [ReaderFree r a] -> ReaderFree r [a]
sequenceReaderFree = foldr (\lf ini -> bindReaderFree (\x -> bindReaderFree (\xs -> pureReaderFree (x:xs)) ini) lf) (pureReaderFree [])

-- Free
data Free f a = Done a | More (f (Free f a))

bindFree :: Functor f => (a -> Free f b) -> Free f a -> Free f b
bindFree k (Done v) = k v
bindFree k (More (inner)) = More (fmap (bindFree k) inner)

pureFree = Done

sequenceFree :: Functor f => [Free f a] -> Free f [a]
sequenceFree = foldr (\lf ini -> bindFree (\x -> bindFree (\xs -> pureFree (x:xs)) ini) lf) (pureFree [])

-- IO
bindIO :: (a -> IO b) -> IO a -> IO b
bindIO = (=<<)

pureIO :: a -> IO a
pureIO = return

sequenceIO :: [IO a] -> IO [a]
sequenceIO = foldr (\lf ini -> bindIO (\x -> bindIO (\xs -> pureIO (x:xs)) ini) lf) (pureIO [])

 

