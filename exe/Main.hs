module Main where
  
import AUC
import Sound.ProteaAudio
import Control.Monad
import Control.Concurrent
import System.IO

main = do
  hSetBuffering stdin NoBuffering
  initAudio 1 44100 512
  myplay
  --threadDelay $ 1000000 * 30
  c <- getChar
  putStrLn "Bye!"
  finishAudio
