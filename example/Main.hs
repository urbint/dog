{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Data.ByteString as B

import           Datalog.Lexer
import           Datalog.QQ
import           Datalog.Parser
import           Datalog.Pretty
import           Datalog.AST

import           Data.String.Conversions

main :: IO ()
main = do
  file <- B.readFile "datalog.txt"
  mapM_ print (getTokens file)
  either putStrLn (print . printProgram) (parseDatalog file)
  putStrLn "show me some QQ"
  print $ parseDatalog $ cs $ show $ printProgram someDataLog

someDataLog :: Program
someDataLog = [datalog| edge(a,b). |]
