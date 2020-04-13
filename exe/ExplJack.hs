{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Sound.JACK                          as JACK
import qualified Sound.JACK.Audio                    as Audio

import qualified Sound.JACK.MIDI                     as Midi

import qualified Foreign.C.Error                     as E
import qualified Foreign.C.Types                     as CT

import           System.Environment                  (getProgName)
import           System.Random

import           Data.Array.Storable                 (writeArray)

import           Data.IORef                          (IORef, newIORef,
                                                      readIORef, writeIORef)

import qualified Data.EventList.Absolute.TimeBody    as EventList
import qualified Data.Map                            as M
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Storable                as V
import qualified Sound.MIDI.Message                  as MM
import qualified Sound.MIDI.Message.Channel          as MMC
import qualified Sound.MIDI.Message.Channel.Voice    as MMV

import           GHC.Float
import           GHC.Int
import           GHC.Word

import           Debug.Trace
import qualified Sound.JACK                          as Jack
import qualified Sound.JACK.Exception                as JackExc
import           System.Mem

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans

import           Control.Monad                       (foldM, forever,
                                                      replicateM)
import           Control.Monad.Random
import           System.IO

import           AUC

data PlayState =
  PlayState
    { _deck     :: AUDeck
    , _tsv      :: Double
    , _psv      :: Double
    , _gc       :: Int
    , _nslice   :: Int
    , _isample  :: Int
    , _islice   :: Int
    , _rndprcnt :: Double
    }

initState = pure $ PlayState [] 8.0 1.0 75 8 0 7 40

mainWait ::
     JackExc.ThrowsErrno e
  => Jack.Client
  -> String
  -> IORef PlayState
  -> Sync.ExceptionalT e IO ()
mainWait client name psr =
  Jack.withActivation client $
  Trans.lift $ do
    putStrLn $ "started " ++ name ++ "..."
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    lib <-
      createAULib
        [ "fatamen.wav"
        , "GM1.wav"
        , "Dancing.wav"
        , "Fat_On_Funk_1.wav"
        , "Fat_On_Funk_2.wav"
        , "Fools_Gold.wav"
        , "Football.wav"
        , "Funky_Mule_Intro.wav"
        , "Funky_Soul_Shake_2.wav"
        , "Fuzz_And_Da_Boog.wav"
        , "Getaway.wav"
        , "Get_Ready.wav"
        , "Getting_It_Out_Of_My_System_1.wav"
        , "Hard_Way_To_Go.wav"
        , "Hoppin_John.wav"
        , "Hot_Pants.wav"
        , "Mary_Mary.wav"
        , "New_Orleans.wav"
        , "Party_Time.wav"
        , "Sandy.wav"
        , "Shackup.wav"
        , "This_Is_My_Love.wav"
        ]
    forever $ do
      ps <- readIORef psr
      let sn = M.keys lib !! (_isample ps `mod` M.size lib)
      let ss = sliceTo (_nslice ps) lib sn
      putStrLn "Creating Extractor"
      let aux =
            [ \s -> AUX s 0 defaultPhsr False (cVol 1)
            , \s ->
                AUX s 0 (defaultPhsr <> stutterPhsr 0.1) False (linFadeInEnv 1)
            , \s ->
                AUX
                  s
                  0
                  (pitchPhsr 0.5 <> stutterPhsr 0.1)
                  False
                  (linFadeInEnv 1.3)
            , \s -> AUX s 0 (pitchPhsr 0.5) False (linFadeInEnv 1.3)
            , \s -> AUX s 0 (pitchPhsr (1 + (6 * (0.5 / 12)))) False (cVol 1)
            , \s ->
                AUX
                  s
                  0
                  (defaultPhsr <> stutterPhsr 0.25)
                  False
                  (linFadeOutEnv 1)
            , \s ->
                AUX
                  s
                  0
                  (defaultPhsr <> stutterPhsr 0.125)
                  False
                  (linFadeOutEnv 1)
            , \s ->
                AUX
                  s
                  0
                  (defaultPhsr <>
                   stutterPhsr 0.125 <>
                   timestretchPhsr (_gc ps) (_tsv ps) (_psv ps))
                  False
                  (linFadeOutEnv 1)
            , \s ->
                AUX
                  s
                  0
                  (reversePhsr <> timestretchPhsr (_gc ps) (_tsv ps) (_psv ps))
                  False
                  (linFadeOutEnv 1)
            , \s ->
                AUX
                  s
                  0
                  (defaultPhsr <> stutterPhsr 0.333)
                  False
                  (linFadeOutEnv 1)
            , \s ->
                AUX
                  s
                  0
                  (reversePhsr <> stutterPhsr 0.125)
                  False
                  (linFadeOutEnv 1)
            , \s ->
                AUX
                  s
                  0
                  (timestretchPhsr (_gc ps) (_tsv ps) (_psv ps))
                  False
                  (cVol 1)
            , \s -> AUX s 0 (defaultPhsr <> reversePhsr) False (cVol 1)
            ]
      putStrLn "Rendering Deck"
      g <- newStdGen
      let st = AUCState 1 g
      let (s, _, ()) =
            runAUC AUCEnv st g $ do
              let rndRepeat [] = pure []
                  rndRepeat (i:is) = do

                    probRep <- getRandomR (1, 100 :: Int)
                    numRep <- getRandomR (1, 4 :: Int)
                    ins <- replicateM numRep (getRandomR (0,(_nslice ps - 1)))

                    return $
                      if
                        | probRep > 20 -> (i : ins ++ is)
                        | True -> (i : is)
              let pat = [0 .. (_nslice ps - 1)]
              ac <- getRandomR (4, 8)
              patM <- concat <$> replicateM ac ((<> pat) <$> rndRepeat pat)
              d <-
                foldl1 (<>) <$>
                (mapM
                   (\i
                                  --ds <- getRandomR (1, 2)
                     -> do
                      let ds = 1
                      s <-
                        replicateM
                          ds
                          (do si <- getRandomR (0, length ss - 1)
                              ai <- getRandomR (0, length aux - 1)
                              prb <- getRandomR (0, 100 :: Double)
                              let xtr =
                                    if prb < _rndprcnt ps
                                      then (aux !! ai)
                                      else (\s ->
                                              AUX s 0 defaultPhsr False (cVol 1))
                                          --return $ (aux !! ai) (ss !! si))
                              return $ xtr (ss !! i))
                      return [AUXP s]) 
                   $ patM  )
              return $ AUXS d
      let auxD = s
      putStr ">"
      inp <- getChar
      case inp of
        'q' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_rndprcnt = _rndprcnt ps - 1}
        'w' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_rndprcnt = _rndprcnt ps + 1}
        'o' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_nslice = _nslice ps - 1}
        'p' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_nslice = _nslice ps + 1}
        'n' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_tsv = _tsv ps - 0.1}
        'm' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_tsv = _tsv ps + 0.1}
        'j' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_psv = _psv ps - 0.1}
        'k' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_psv = _psv ps + 0.1}
        'y' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_isample = (_isample ps - 1) `mod` M.size lib}
        'x' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_isample = (_isample ps + 1) `mod` M.size lib}
        'c' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_islice = (_islice ps - 1) `mod` _nslice ps}
        'v' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_islice = (_islice ps + 1) `mod` _nslice ps}
        'u' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_gc = _gc ps - 1}
        'i' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_gc = _gc ps + 1}
        'U' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_gc = _gc ps - 10}
        'I' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_gc = _gc ps + 10}
        'a' -> do
          ps <- readIORef psr
          writeIORef psr $ ps {_deck = (auxD : (_deck ps))}
        'b' -> do
          ps <- readIORef psr
          let x = ss !! (_islice ps)
              --s = AUX x 0 (defaultPhsr) False (cVol 1)
              s =
                AUX
                  x
                  0
                  (timestretchPhsr (_gc ps) (_tsv ps) (_psv ps))
                  False
                  (cVol 1)
          writeIORef psr $ ps {_deck = (s : (_deck ps))}
        _ -> pure ()
      ps <- readIORef psr
      putStrLn $
        "Sample : " ++
        sn ++
        " SC  : " ++
        (show $ _nslice ps) ++
        " SlIdx : " ++
        (show $ _islice ps) ++
        " TSV : " ++
        (show $ _tsv ps) ++
        " PSV : " ++
        (show $ _psv ps) ++
        " GC  : " ++ (show $ _gc ps) ++ " RndPrcnt  : " ++ (show $ _rndprcnt ps)
    Jack.waitForBreak

main :: IO ()
main = do
  name <- getProgName
  is <- initState
  stateRef <- newIORef is
  JACK.handleExceptions $
    JACK.withClientDefault name $ \client ->
      JACK.withPort client "input" $ \input ->
        JACK.withPort client "output" $ \output ->
          JACK.withProcess client (processAudioOut stateRef input output) $
          mainWait client name stateRef

processAudioOut ::
     IORef PlayState
  -> Midi.Port JACK.Input
  -> Audio.Port JACK.Output
  -> JACK.NFrames
  -> Sync.ExceptionalT E.Errno IO ()
processAudioOut psr input output nframes@(JACK.NFrames nframesInt) = do
  mel <- EventList.toPairList <$> Midi.readEventsFromPort input nframes
  Trans.lift $ do
    mapM_
      (\(t, b) -> do
         ps <- readIORef psr
         case b of
           MM.System _ -> return ()
           MM.Channel ct -> do
             let c = MMC.fromChannel $ MMC.messageChannel ct
             if c /= 1
               then return ()
               else case MMC.messageBody ct of
                      MMC.Mode _ -> return ()
                      MMC.Voice vt
                           {-| isSampSelectCntrl vt ->
                             do let (MMV.Control _ cv) = vt
                                setSampSelect psr cv
                           | isMasterVolCntrl vt ->
                             do let (MMV.Control _ cv) = vt
                                setMasterVol psr cv
                           | isMasterPitchCntrl vt ->
                             do let (MMV.Control _ cv) = vt
                                setMasterPitch psr cv
                           | isGateAmtCntrl vt ->
                             do let (MMV.Control _ cv) = vt
                                setGateAmt psr cv
                           | isDoWarpCntrl vt -> do toggleDoWarp psr
                           | isWarpCntrl vt ->
                             do let (MMV.Control _ cv) = vt
                                setWarp
                                  psr
                                  (0.1 + (2 * MMV.realFromControllerValue cv))
                           | isBankCntrl vt ->
                             do let (MMV.Control _ cv) = vt
                                setBank psr cv
                           | isRetrigCntrl vt ->
                             do let (MMV.Control _ cv) = vt
                                let dn = cv - 64
                                setRetrig psr dn
                           | isPresetCntrl vt ->
                             do let (MMV.Control _ cv) = vt
                                setPreset psr cv
                           | MMV.isNoteOn vt ->
                             do let (MMV.NoteOn p v') = vt
                                let v = MMV.fromVelocity v'
                                let (JACK.NFrames nf) = t
                                let banklen =
                                      Prelude.length $ ((smpmap ps) !! bank ps)
                                let ss = sselect ps `mod` banklen
                                let s = slu (smpmap ps) (bank ps) (ss)
                                let sl = V.length s
                                addHit psr $
                                  mkHit
                                    ((+) (mpitch ps) (MMV.fromPitch p))
                                    (fromVel v)
                                    (gateamt ps)
                                    (warpfact ps)
                                    (bank ps)
                                    (ss)
                                    (sl)
                                    t
                           | MMV.isNoteOff vt ->
                             do let (MMV.NoteOff p _) = MMV.explicitNoteOff vt
                                let banklen =
                                      Prelude.length $ ((smpmap ps) !! bank ps)
                                let ss = sselect ps `mod` banklen
                                delHit
                                  psr
                                  ((+) (mpitch ps) (MMV.fromPitch p))
                                  (bank ps)
                                  (ss) -}
                       -> if | otherwise -> return ())
      mel
    outArr <- Audio.getBufferArray output nframes
    case JACK.nframesIndices nframes of
      [] -> return ()
      idxs ->
        mapM_
          (\i@(JACK.NFrames ii) -> do
             f <- nextFrame psr i
             writeArray outArr i (CT.CFloat $ double2Float f))
          idxs

nextFrame :: IORef PlayState -> Jack.NFrames -> IO Double
nextFrame psr i = do
  ps <- readIORef psr
  g <- newStdGen
  let (s, _, d') = (runAUC (_deck ps) (AUCState 1.0 g) g nextFrameFromDeck)
  writeIORef psr $ ps {_deck = d'}
  return s
