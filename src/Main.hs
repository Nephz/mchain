{-# LANGUAGE OverloadedStrings #-}

module Main where

import Mchain
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  file <- prompt "File to Read: "
  ls   <- fmap T.lines (T.readFile file)
  putStrLn "It will take longer time to generate the first chain"
  let wm = walkMap $ walks ls
  loop ls wm

loop :: [T.Text] -> WalkMap -> IO ()
loop ls wm = do
  n  <- prompt "Iterations of chain: "
  s  <- prompt "String to start with: "
  putStrLn "Generating chain..."
  ss <- genChain wm (T.pack s) (1, (read n :: Integer))
  let t = replacing filt ss 
  print t
  ans <- prompt "\n> run again? y/n: "
  if ans == "y" then loop ls wm else return ()

replacing :: [(T.Text, T.Text)] -> T.Text -> T.Text
replacing ((n,r):nrs) txt = 
  replacing nrs $ T.replace n r txt
replacing [] txt = txt

filt :: [(T.Text, T.Text)]
filt =  [ (" .", ".")
        , (" ,", ",")
        , (" ' ","'")
        , ("( ", "(")
        , (" )", ")")
        , (" :", ":")
        ]

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine