module Main where

import           AUC
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.RWS
import qualified Data.ByteString.Lazy as BS
import           Sound.ProteaAudio
import           System.IO
import           System.Random

myplay lib = do
  putStrLn "Creating Selection"
  let ss =
        [ AUSel lib "fatamen.wav" (0 * 74325 `div` 10) (2 * 74325 `div` 10)
        , AUSel lib "fatamen.wav" (2 * 74325 `div` 10) (2 * 74325 `div` 10)
        , AUSel lib "fatamen.wav" (4 * 74325 `div` 10) (2 * 74325 `div` 10)
        , AUSel lib "fatamen.wav" (6 * 74325 `div` 10) (2 * 74325 `div` 10)
        , AUSel lib "fatamen.wav" (8 * 74325 `div` 10) (2 * 74325 `div` 10)
        , AUSel lib "mileenia.wav" (0 * 71656 `div` 10) (2 * 71656 `div` 10)
        , AUSel lib "mileenia.wav" (2 * 71656 `div` 10) (2 * 71656 `div` 10)
        , AUSel lib "mileenia.wav" (4 * 71656 `div` 10) (2 * 71656 `div` 10)
        , AUSel lib "mileenia.wav" (6 * 71656 `div` 10) (2 * 71656 `div` 10)
        , AUSel lib "mileenia.wav" (8 * 71656 `div` 10) (2 * 71656 `div` 10)
        , AUSel lib "mileenia.wav" 0 71656
        , AUSel lib "kleinekakanudel.wav" (0 * 66144 `div` 10) (4 * 66144 `div` 10)
        , AUSel lib "kleinekakanudel.wav" (3 * 66144 `div` 10) (4 * 66144 `div` 10)
        , AUSel lib "kleinekakanudel.wav" (5 * 66144 `div` 10) (4 * 66144 `div` 10)
        , AUSel lib "kleinekakanudel.wav" (7 * 66144 `div` 10) (3 * 66144 `div` 10)
        , AUSel lib "kleinekakanudel.wav" (9 * 66144 `div` 10) (1 * 66144 `div` 10)
        , AUSel lib "kleinekakanudel.wav" 0 66144 
        ]
  putStrLn "Creating Extractor"
  let aux =
        [ \s -> AUX s 0 defaultPhsr False (cVol 1)
        , \s -> AUX s 0 (defaultPhsr <> stutterPhsr 0.1) False (linFadeInEnv 1)
        , \s ->
            AUX s 0 (pitchPhsr 0.5 <> stutterPhsr 0.1) False (linFadeInEnv 1.3)
        , \s -> AUX s 0 (pitchPhsr 0.5) False (linFadeInEnv 1.3)
        , \s -> AUX s 0 (pitchPhsr (1 + (6 * (0.5 / 12)))) False (cVol 1)
        , \s ->
            AUX s 0 (defaultPhsr <> stutterPhsr 0.25) False (linFadeOutEnv 1)
        , \s ->
            AUX s 0 (defaultPhsr <> stutterPhsr 0.125) False (linFadeOutEnv 1)
        , \s ->
            AUX s 0 (defaultPhsr <> stutterPhsr 0.333) False (linFadeOutEnv 1)
        , \s -> AUX s 0 (defaultPhsr <> reversePhsr) False (cVol 1)
        ]
  putStrLn "Rendering Deck"
  g <- newStdGen
  let st = AUCState 1 g
  let (s, _, ()) =
        runAUC AUCEnv st g $ do
          ac <- getRandomR (4, 16)
          foldl1 (<>) <$>
            replicateM
              (ac)
              (do ds <- getRandomR (1, 3)
                  s <-
                    replicateM
                      ds
                      (do si <- getRandomR (0, length ss - 1)
                          ai <- getRandomR (0, length aux - 1)
                          return $ (aux !! ai) (ss !! si))
                  renderDeckBS s)
    --let s = SVBS.vectorToByteString rd
  putStrLn $ "Length of rendered Deck : " ++ show (BS.length s)
  putStrLn "Convering rendered Deck to MemSample"
    --smp <- sampleFromMemoryWav s 1.0
  return $ BS.toStrict s

genAu lib = do
  s <- myplay lib
  smp <- sampleFromMemoryPcm s 1 22050 32 1.0
  putStrLn "Playing MemSample"
  soundStopAll
  soundLoop smp 1.0 1.0 0.0 0.8
  c <- getChar
  unless (c == 'q') $ genAu lib

main = do
  hSetBuffering stdin NoBuffering
  initAudio 1 44100 512
  putStrLn "Creating Lib"
  lib <- createAULib ["fatamen.wav", "kleinekakanudel.wav", "mileenia.wav"]
  genAu lib
  putStrLn "Bye!"
  finishAudio
