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

import           GHC.Int
import           GHC.Word

import           Debug.Trace
import qualified Sound.JACK                          as Jack
import qualified Sound.JACK.Exception                as JackExc
import           System.Mem

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans

import           Control.Monad                       (forever)
import           System.IO

import AUC

data PlayState =  PlayState { _deck :: AUDeck }

initState = pure $ PlayState []

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


    lib <- createAULib ["fatamen.wav"]
    let s1 = AUSel lib "fatamen.wav" 0 74325
    let s2 = AUSel lib "fatamen.wav" 3000 71325
    let s3 = AUSel lib "fatamen.wav" 20000 54325
    let aux1 = AUX s1 0 (defaultPhsr) False (cVol 1.0)
    let aux2 = AUX s2 0 (reversePhsr) False (cVol 1.0)
    let aux3 = AUX s3 0 (defaultPhsr <> stutterPhsr 0.1) False (linFadeOutEnv 1.0)

    let auxA = AUXS [aux1,aux2,aux3]
    let auxB = AUXP [aux1,aux2,aux3]

    forever $ do
      putStr ">"
      inp <- getChar
      case inp of
        'a' -> do
          ps <- readIORef psr
          writeIORef psr $ ps { _deck = (auxA : (_deck ps)) }
        'b' -> do
          ps <- readIORef psr
          writeIORef psr $ ps { _deck = (auxB : (_deck ps)) }
        _   -> pure ()
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
                      MMC.Voice vt ->
                        if {-| isSampSelectCntrl vt ->
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
                           | otherwise -> return ())
      mel
    outArr <- Audio.getBufferArray output nframes
    case JACK.nframesIndices nframes of
      [] -> return ()
      idxs ->
        mapM_
          (\i@(JACK.NFrames ii) -> do
             f <- hits2nextFrame psr i
             writeArray outArr i (CT.CFloat f))
          idxs


hits2nextFrame :: IORef PlayState -> Jack.NFrames -> IO Float
hits2nextFrame psr i = do
  ps <- readIORef psr
  g <- newStdGen
  let (s,_,d') = (runAUC (_deck ps) (AUCState 1.0 g) g nextFrameFromDeck) 
  writeIORef psr $ ps { _deck = d' }
  return s






