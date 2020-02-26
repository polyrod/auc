{-# LANGUAGE MultiWayIf #-}

module AUC
  (
--  myplay,
--  atest ,
--  btest 
  ) where

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

import Control.Monad.RWS
import Control.Monad.Random

data AUCEnv = AUCEnv
data AUCState = AUCState
data AUCLog = AUCLog

type AUC a = RWST AUCEnv AUCLog AUCState (Rand StdGen) a

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
newtype AUPhsr =
  AUPhsr
    { _next :: AUSel -> AUIdx -> AUIdx
    }

instance Semigroup AUPhsr where
  pa <> pb =
    AUPhsr $ \as ai ->
      let ai_a = _next pa as ai
       in _next pb as ai_a

instance Monoid AUPhsr where
  mempty = AUPhsr $ \_ ai -> ai

checkRel (Rel r) =
  if | r < 0.0 || r > 1.0 -> Done
     | True -> Rel r

defaultPhsr =
  AUPhsr
    (\sel idx ->
       case idx of
         Abs a -> checkRel $ Rel $ fromIntegral a / fromIntegral (_len sel)
         Rel r ->
           checkRel $
           Rel r 
         Done -> Done)

reversePhsr =
  AUPhsr
    (\sel idx ->
       case idx of
         Abs a ->
           checkRel $ Rel $ 1 - (fromIntegral a / fromIntegral (_len sel))
         Rel r ->
           checkRel $
           Rel $ (1 - r) 
         Done -> Done)

pitchPhsr p =
  AUPhsr
    (\sel idx ->
       case idx of
         Abs a ->
           checkRel $ Rel $ (fromIntegral a / (p * fromIntegral (_len sel)))
         Rel r ->
           checkRel $
           Rel $ r / p
         Done -> Done)

stutterPhsr p =
  AUPhsr
    (\sel idx ->
       case idx of
         Abs a -> 
           let r = fromIntegral a / (fromIntegral (_len sel))
            in if r > 1.0
                then Done
                else checkRel $ Rel $ r `mod'` p
         Rel r ->
           checkRel $
           Rel $ r `mod'` p 
         Done -> Done)


data AUX =
  AUX
    { _sel   :: AUSel
    , _ticks :: AUTick
    , _phsr  :: AUPhsr
    , _done  :: Bool
    , _vol   :: AURatio -> AUVol
    }

cVol :: AUVol -> AURatio -> AUVol
cVol v = const v

linFadeOutEnv :: AUVol -> AURatio -> AUVol
linFadeOutEnv v r = v * (1-r) 

linFadeInEnv :: AUVol -> AURatio -> AUVol
linFadeInEnv v r = v * (r) 

newAUX sel =
  AUX sel (-4410) (pitchPhsr (1.0 - (0.5 / 12.0) * 5.0) <> defaultPhsr) False (cVol 1.0)

type AUDeck = [AUX]

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

renderDeckBS :: AUDeck -> BS.ByteString
renderDeckBS d = renderDeckBS' d mempty

renderDeckBS' :: AUDeck -> BS.Builder -> BS.ByteString
renderDeckBS' d b =
  let d' = filter (not . _done) d
      etrip = map extract d'
      aux (a, _, _) = a
      smp (_, _, s) = s
      s = foldl  (\a b -> a + smp b) 0 etrip
      as = map aux etrip
   in if null d'
        then BS.toLazyByteString b
        else renderDeckBS' as (b <> BS.floatLE s)

extract :: AUX -> (AUX, AUIdx, AUSample)
extract aux =
  let t' = _ticks aux + 1
      i = (_next $ _phsr aux) (_sel aux) (Abs $ _ticks aux)
   in if | Done == i -> (aux {_done = True}, i, 0)
         | t' < 0 -> (aux {_ticks = t'}, Abs t', 0)
         | otherwise ->
           let ri = (fromIntegral t') / fromIntegral (_len $ _sel aux)  
               s = (_vol aux $ ri) * sampleAt (_sel aux) i
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
        let rat = (ri * fromIntegral (_len sel))
            frct = rat - (fromIntegral $ floor rat)
            li = floor rat
            ui = ceiling rat
        lv <- _data au SV.!? (_off sel + li)
        uv <- _data au SV.!? (_off sel + ui)
        return $ lv + ((uv-lv) * frct)

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
myplay = do
  putStrLn "Creating Lib"
  lib <- createAULib ["dark.ogg","amen.wav"]
  let s0 = AUSel lib "dark.ogg" 0 96000
  putStrLn "Creating Selection"
  let s1 = AUSel lib "amen.wav" 30000 96000
  putStrLn "Creating Extractor"
  let aux1 = AUX s1 0 (defaultPhsr ) False (cVol 1)
  let d0 = AUX s0 0 (defaultPhsr <> pitchPhsr (1.1 * 44100.0/48000.0)) False (cVol 0.7)
  let aux2 = AUX s1 (-30) (pitchPhsr 0.99) False (cVol 1)
  let aux3 = AUX s1 0 (defaultPhsr <> reversePhsr ) False (cVol 1)
  let aux4 = AUX s1 0 (defaultPhsr) False (cVol 1)
  putStrLn "Rendering Deck"
  let s0 = renderDeckBS [aux1,aux2,aux3]
  let s1 = renderDeckBS [aux3,d0]
  let s2 = renderDeckBS [aux1,aux2,d0]
  let s3 = renderDeckBS [aux1,d0]
  let s4 = renderDeckBS [aux4]
  let s = s0 <> s1 <> s2 <> s3 <> s4 
    --let s = SVBS.vectorToByteString rd
  putStrLn $ "Length of rendered Deck : " ++ show (BS.length s)
  putStrLn "Convering rendered Deck to MemSample"
    --smp <- sampleFromMemoryWav s 1.0
  smp <- sampleFromMemoryPcm (BS.toStrict s) 1 22050 32 1.0
  putStrLn "Playing MemSample"
  soundLoop smp 1.0 1.0 0.0 1.0

atest = do
  putStrLn "Creating Lib"
  lib <- createAULib ["fatamen.wav"]
  putStrLn "Creating Selection"
  let s0 = AUSel lib "fatamen.wav" (0 * 74325 `div` 10) (1 * 74325 `div` 10)
  let s1 = AUSel lib "fatamen.wav" (1 * 74325 `div` 10) (1 * 74325 `div` 10)
  let s2 = AUSel lib "fatamen.wav" (2 * 74325 `div` 10) (1 * 74325 `div` 10)
  let s3 = AUSel lib "fatamen.wav" (3 * 74325 `div` 10) (6 * 74325 `div` 10)
  putStrLn "Creating Extractor"
  let aux1 = AUX s1 0 (pitchPhsr 2.0 <> defaultPhsr <> stutterPhsr 0.2 ) False (cVol 1)
  let aux2 = AUX s2 0 (defaultPhsr) False (cVol 1)
  let aux3 = AUX s0 0 (defaultPhsr) False (cVol 1)
  let aux4 = AUX s3 0 (defaultPhsr) False (cVol 1)
  putStrLn "Rendering Deck"
  let s0 = renderDeckBS [aux1]
  let s1 = renderDeckBS [aux2,aux1]
  let s2 = renderDeckBS [aux3]
  let s3 = renderDeckBS [aux4]
  let s = s0 <> s1 <> s2 <> s1 <> s0 <> s0 <> s2 <> s2 <> s1 <> s3 
  putStrLn $ "Length of rendered Deck : " ++ show (BS.length s)
  putStrLn "Convering rendered Deck to MemSample"
  smp <- sampleFromMemoryPcm (BS.toStrict s) 1 22050 32 1.0
  putStrLn "Playing MemSample"
  soundLoop smp 1.0 1.0 0.0 1.0

btest = do
  putStrLn "Creating Lib"
  lib <- createAULib ["fatamen.wav"]
  putStrLn "Creating Selection"
  let s0 = AUSel lib "fatamen.wav" (0 * 74325 `div` 10) (2 * 74325 `div` 10)
  let s1 = AUSel lib "fatamen.wav" (3 * 74325 `div` 10) (2 * 74325 `div` 10)
  let s2 = AUSel lib "fatamen.wav" (5 * 74325 `div` 10) (2 * 74325 `div` 10)
  let s3 = AUSel lib "fatamen.wav" (7 * 74325 `div` 10) (2 * 74325 `div` 10)
  let s4 = AUSel lib "fatamen.wav" (9 * 74325 `div` 10) (2 * 74325 `div` 10)
  let s5 = AUSel lib "fatamen.wav" (2 * 74325 `div` 10) (3 * 74325 `div` 10)
  putStrLn "Creating Extractor"
  let aux0 = AUX s0 0 (defaultPhsr) False (cVol 1)
  let aux1 = AUX s1 0 (stutterPhsr 0.125 <> pitchPhsr 1.0 ) False (linFadeOutEnv 1)
  let aux2 = AUX s2 0 (defaultPhsr) False (cVol 1)
  let aux3 = AUX s3 0 (defaultPhsr) False (cVol 1)
  let aux4 = AUX s4 0 (defaultPhsr) False (cVol 1)
  let aux5 = AUX s5 0 (defaultPhsr) False (cVol 1)
  putStrLn "Rendering Deck"
  let s0 = renderDeck [aux0]
  let s1 = renderDeck [aux1]
  let s2 = renderDeck [aux2]
  let s3 = renderDeck [aux3]
  let s4 = renderDeck [aux4]
  let s5 = renderDeck [aux5]
  let s = s0 <> s1 <> s2 <> s3 <> s1 <> s1 <> s4 
  putStrLn $ "Length of rendered Deck : " ++ show (SV.length s)

  let nau = newAU s
      lib' = M.insert "newbk" nau lib

  let s0 = AUSel lib' "newbk" 0 (_dur nau) 
  let aux0 = AUX s0 0 (defaultPhsr) False (cVol 1)
  let aux1 = AUX s0 0 (reversePhsr ) False (cVol 1)
  let aux2 = AUX s0 0 (pitchPhsr (1 + (6 * (0.5 / 12)))) False (cVol 1)
  let aux3 = AUX s0 0 (pitchPhsr 2.0 <> stutterPhsr (1/28) <> pitchPhsr 2.0 ) False (linFadeInEnv 1)

  let s0 = renderDeckBS [aux0]
  let s1 = renderDeckBS [aux0,aux1]
  let s2 = renderDeckBS [aux2]
  let s3 = renderDeckBS [aux0,aux2]
  let s4 = renderDeckBS [aux3]
  let s = s4 <> s0 <> s0 <> s0 <> s1 <> s2 <> s0 

  putStrLn "Convering rendered Deck to MemSample"
  smp <- sampleFromMemoryPcm (BS.toStrict s) 1 22050 32 1.0
  putStrLn "Playing MemSample"
  soundLoop smp 1.0 1.0 0.0 1.0

-}

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





State + Rnd + IORef + Reader



-}
