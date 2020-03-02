{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE RankNTypes             #-}

module AUC
  ( createAULib
  , AUSel(..)
  , AUX(..)
  , defaultPhsr
  , stutterPhsr
  , reversePhsr
  , pitchPhsr
  , timestretchPhsr
  , linFadeInEnv
  , linFadeOutEnv
  , cVol
  , runAUC
  , AUCState(..)
  , AUCEnv(..)
  , AUCLog(..)
  , AUDeck
  , renderDeckBS
  , nextFrameFromDeck
  ) where

--  atest ,
--  btest
import qualified Data.ByteString.Builder          as BS
import qualified Data.ByteString.Lazy             as BS
import qualified Data.Map                         as M
import qualified Data.Sequence                    as S
import qualified Data.Vector.Storable             as SV
import qualified Data.Vector.Storable.ByteString  as SVBS
import qualified Data.Vector.Unboxed              as V

import qualified Sound.File.Sndfile               as SF
import qualified Sound.File.Sndfile.Buffer.Vector as BV

import           Sound.ProteaAudio

import           Data.Fixed                       (mod')
import           Data.Maybe                       (fromMaybe)

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.RWS

import Debug.Trace

data AUCEnv =
  AUCEnv

instance Monoid AUCEnv where
  mempty = AUCEnv

instance Semigroup AUCEnv where
  a <> b = AUCEnv

data AUCState =
  AUCState
    { _svol   :: AUVol
    , _rndGen :: StdGen
    }

data AUCLog =
  AUCLog

instance Monoid AUCLog where
  mempty = AUCLog

instance Semigroup AUCLog where
  a <> b = AUCLog

--- ????
instance Monoid Float where
  mempty = 1.0

instance Semigroup Float where
  a <> b = a * b

newtype AUC r w s a =
  AUC
    { getAUC :: RWST r w s (Rand StdGen) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader r
           , MonadWriter w
           , MonadState s
           , MonadRandom
           )

--runAUC :: AUC r w s a -> r -> w -> s -> IO a
runAUC r s g auc = evalRand (runRWST (getAUC auc) r s) g

type AULen = Int

type AUId = String

type AUOff = Int

type AUTick = Int

type AURatio = Float

type AUVol = Float

type AUSample = Float

data AUIdx
  = Rel AURatio
  | Abs AUTick
  | Done
  deriving (Eq, Show)

data AU =
  AU
    { _data :: SV.Vector AUSample
    , _dur  :: AULen
    }

type AULib = (M.Map AUId AU)

data AUSel =
  AUSel
    { _lib :: AULib
    , _id  :: AUId
    , _off :: AUOff
    , _len :: AULen
    }

--  | Par AUSel AUSel
--  | Ser AUSel AUSel
--
--

data AUX
  = AUXS [AUX]
  | AUXP [AUX]
  | AUX
      { _sel   :: AUSel
      , _ticks :: AUTick
      , _phsr  :: AUPhsr 
      , _done  :: Bool
      , _vol   :: AURatio -> AUVol
      }


type AUDeck = [AUX]

class HasAUIdx a where
  getIdx :: a -> AUIdx
  putIdx :: AUIdx -> a -> a

instance HasAUIdx AUIdx where
  getIdx = id 
  putIdx i a = i

newtype AUPhsr =
  AUPhsr
    { _next :: forall i . (HasAUIdx i) => AUC AUX (AUVol -> AUVol) i AUIdx 
    }

instance Semigroup AUPhsr where
  pa <> pb =
    AUPhsr $ do
      _next pa
      _next pb

instance Monoid AUPhsr where
  mempty = AUPhsr $ gets getIdx

checkRel (Rel r) =
  if | r < 0.0 || r > 1.0 -> Done
     | True -> Rel r

defaultPhsr =
  AUPhsr $ do
    st <- get
    let idx = getIdx st
    sel <- asks _sel
    let idx' =
          case idx of
            Abs a -> checkRel $ Rel $ fromIntegral a / fromIntegral (_len sel)
            Rel r -> checkRel $ Rel r
            Done -> Done
    put $ putIdx idx' st
    return idx'

reversePhsr =
  AUPhsr $ do
    st <- get
    let idx = getIdx st
    sel <- asks _sel
    let idx' =
          case idx of
            Abs a ->
              checkRel $ Rel $ 1 - (fromIntegral a / fromIntegral (_len sel))
            Rel r -> checkRel $ Rel (1 - r)
            Done -> Done
    put $ putIdx idx' st
    return idx' 

pitchPhsr p =
  AUPhsr $ do
    st <- get
    let idx = getIdx st
    sel <- asks _sel
    let idx' =
          case idx of
            Abs a ->
              checkRel $ Rel (fromIntegral a / (p * fromIntegral (_len sel)))
            Rel r -> checkRel $ Rel $ r / p
            Done -> Done
    put $ putIdx idx' st
    return idx' 


idxAp :: (AUIdx -> AUIdx) -> AUIdx -> AUIdx
idxAp f Done = Done
idxAp f i    = f i

idxToRel :: AUSel -> AUIdx -> AUIdx
idxToRel _ Done    = Rel 1.0
idxToRel s (Abs a) = Rel $ fromIntegral a / fromIntegral (_len s)
idxToRel s (Rel r) = Rel r

fromRel :: AUSel -> AUIdx -> AURatio
fromRel s i =
  let (Rel r) = idxToRel s i
   in r

stutterPhsr p =
  AUPhsr $ do
    st <- get
    let idx = getIdx st
    sel <- asks _sel
    if idx == Done
      then return idx
      else do
        let r = fromRel sel idx
            r' = r `mod'` p
            vs =
              if | r' < (p / 100) -> linFadeInEnv 1 r'
                 | r' > (p - (p / 100)) -> linFadeOutEnv 1 r'
                 | True -> 1
        tell (* vs)
        let idx' = checkRel $ Rel r'
        put $ putIdx idx' st
        return idx' 



  {-


  0123456789
  abcdefghij  
  0123456789           <- r



  00112233445566778899
  a b c d e f g h i j 
  0123456789.........  <- r' = r / t

  slot = idx / gr





  r'(r) = t * (g(r) + gi(r))

  g(r) = r / (1/200) 
  gi(r) =  g(r) - trnc(g(r)) * 2 % 1

  -}
timestretchPhsr gc p' t' =
  AUPhsr $ do
    st <- get
    let idx = getIdx st
    sel <- asks _sel
    if idx == Done
      then return idx
      else do
        let p = p' 
        let t = t' 
        let gs = fromIntegral (_len sel) / fromIntegral gc
            r = fromRel sel idx
            g = (r) * (fromIntegral gc)
            gn =  fromIntegral $ truncate g
            gi = ((g - gn) * t) `mod'` 1
            r' = (p/t) * (gn + gi) / fromIntegral gc
            out =  " r: " ++ (show r)
                ++ " g: " ++ (show g)
                ++ " gn: " ++ (show gn)
                ++ " gi: " ++ (show gi)
                ++ " r': " ++ (show r')
            vs =
              if | (g-gn) < (1 / 100) -> linFadeInEnv 1 (g-gn) 
                 | (g-gn) > (1 - (1 / 100)) -> linFadeOutEnv 1 (g-gn)
                 | True -> 1
        tell (*vs)
        let idx' = checkRel $ Rel r'
        put $ putIdx idx' st
        return idx'
        --return $ traceShow out idx'


cVol :: AUVol -> AURatio -> AUVol
cVol = const

linFadeOutEnv :: AUVol -> AURatio -> AUVol
linFadeOutEnv v r = v * (1 - r)

linFadeInEnv :: AUVol -> AURatio -> AUVol
linFadeInEnv v r = v * r

  {-
newAUX sel =
  AUX sel (-4410) (pitchPhsr (1.0 - (0.5 / 12.0) * 5.0) <> defaultPhsr) False (cVol 1.0)


renderDeck :: AUDeck -> SV.Vector AUSample
renderDeck d = renderDeck' d SV.empty

renderDeck' :: AUDeck -> SV.Vector AUSample -> SV.Vector AUSample
renderDeck' d v =
  let d' = filter (not . _done) d
      etrip = map extract d'
      aux (a, _, _) = a
      smp (_, _, s) = s
      s = sum $ map smp etrip
      as = map aux etrip
   in if null d'
        then v
        else renderDeck' as (SV.snoc v s)


-}

auxDone :: AUX -> Bool
auxDone (AUXS _) = False
auxDone (AUXP _) = False
auxDone aux = _done aux

nextFrameFromDeck :: AUC AUDeck AUDeck AUCState AUSample
nextFrameFromDeck = do
  deck <- ask
  let d' = filter (not . auxDone) deck
      aux (a, _, _) = a
      smp (_, _, s) = s
  etrip <- mapM extract d'
  let s = foldl (\a b -> a + smp b) 0 etrip
      as = map aux etrip
  tell as
  return s

--renderDeckBS :: AUDeck -> BS.ByteString
renderDeckBS d = renderDeckBS' d mempty

--renderDeckBS' :: AUDeck -> BS.Builder -> BS.ByteString
renderDeckBS' d b = do
  let d' = filter (not . auxDone) d
      aux (a, _, _) = a
      smp (_, _, s) = s
  etrip <- mapM extract d'
  let s = foldl (\a b -> a + smp b) 0 etrip
      as = map aux etrip
  if null d'
    then return $ BS.toLazyByteString b
    else renderDeckBS' as (b <> BS.floatLE s)

extract :: MonadState AUCState m => AUX -> m (AUX, AUIdx, AUSample)
extract (AUXS [aux]) = extract aux 
extract (AUXS (aux:auxs)) = do
  (aux',idx,s) <- extract aux
  if idx == Done
    then return (AUXS auxs,Abs 0,s)
    else return (AUXS (aux':auxs),idx,s)

extract (AUXP [aux]) = extract aux 
extract (AUXP auxs) = do
  let d' = filter (not . auxDone) auxs
      aux (a, _, _) = a
      smp (_, _, s) = s
  etrip <- mapM extract d'
  let s = foldl (\a b -> a + smp b) 0 etrip
      as = map aux etrip
  return (AUXP as,Done,s)
      

extract aux = do
  let t' = _ticks aux + 1
  g <- gets _rndGen
  modify (\s -> s {_rndGen = (snd . split) $ _rndGen s})
  
  let (i, s, vf) = runAUC aux (Abs $ _ticks aux) g (_next $ _phsr aux)
  return $
    if | Done == i -> (aux {_done = True}, i, 0)
       | t' < 0 -> (aux {_ticks = t'}, Abs t', 0)
       | otherwise ->
         let ri = fromIntegral t' / fromIntegral (_len $ _sel aux)
             s = vf (_vol aux ri) * sampleAt (_sel aux) i
          in (aux {_ticks = t'}, i, s)

progress :: AUX -> AUIdx -> AURatio
progress _ (Rel r)   = r
progress aux (Abs a) = fromIntegral a / fromIntegral (_len $ _sel aux)
progress _ Done      = 1.0

sampleAt :: AUSel -> AUIdx -> AUSample
sampleAt sel i =
  let checkRel r =
        if | r < 0.0 || r > 1.0 -> Nothing
           | True -> Just r
      mb =
        case i of
          Done  -> Nothing
          Rel r -> checkRel r
          Abs t -> checkRel $ fromIntegral t / fromIntegral (_len sel)
      mc = do
        ri <- mb
        au <- M.lookup (_id sel) (_lib sel)
        let rat = ri * fromIntegral (_len sel)
            frct = rat - fromIntegral (floor rat)
            li = floor rat
            ui = ceiling rat
        lv <- _data au SV.!? (_off sel + li)
        uv <- _data au SV.!? (_off sel + ui)
        return $ lv + ((uv - lv) * frct)
   in fromMaybe 0 mc

toOneChannel []       = []
toOneChannel [s]      = []
toOneChannel (l:_:ss) = l : toOneChannel ss

loadSample :: String -> IO (SV.Vector AUSample)
loadSample fn = do
  (info, Just x) <- SF.readFile fn
  print info
  return $
    SV.map (* 0.33) $
    if SF.channels info == 2
      then SV.fromList $ toOneChannel $ SV.toList $ BV.fromBuffer x
      else BV.fromBuffer x

createAULib :: [String] -> IO AULib
createAULib fns = do
  smps <- mapM loadSample fns
  return $ M.fromList $ zip fns $ newAU <$> smps

newAU :: SV.Vector AUSample -> AU
newAU v = AU v $ SV.length v
{-
createLib ----> Lib
                |
                |
                V
               Sel
                |
                |
                v
               AUX <---- Phsr
                |          |
                |          |
                |          v
                |<------- FX
                |
                |
                v
               AUX
                |
                v
               AUDeck



-}
