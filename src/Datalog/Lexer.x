{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Datalog.Lexer
-- Description : Lexer / Tokenizer of Datalog document per Datalog specification
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module Datalog.Lexer
  ( getTokens
  , Token (..)
  , Error (..)
  ) where
--------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Control.Monad.State
--------------------------------------------------------------------------------
import           Datalog.LexerUtils
--------------------------------------------------------------------------------
}

$upper = [A-Z]
$lower = [a-z]
$digit = [0-9]
$identifier = . # [\(\`\'\)\=\:\.\~\?\"\%\,]
@comment = \% .+
@ignore = @comment | \n | \ 
@variable = $upper ($upper | $lower | $digit | '_')+
@punctuator = [\.\~\?\,\(\)\=] | ":-" | "!="

tokens :-
 <0> {
  @ignore ;
  @punctuator { token lexPunctuator }
  @variable { token lexVariable }
  $identifier+ { token lexIdentifier }
  \" { startString }
 }
 <string> {
   . # \" { appendMode }
   \" { endMode }
 }
{
getTokens :: ByteString -> [Token]
getTokens = alexScanTokens

alexScanTokens :: ByteString -> [Token]
alexScanTokens input = flip evalState (LexerState (AlexInput '\n' input 0) InNormal mempty) go
  where
    go :: State LexerState [Token]
    go = do
      LexerState {..} <- get
      case alexScan matchedInput (stateToInt lexerMode) of
        AlexEOF -> eofAction
        AlexError alexInput -> errorAction alexInput
        AlexSkip alexInput _ -> do
          modify $ \s -> s { matchedInput = alexInput }
          go
        AlexToken alexInput _ act -> do
          let len = alexBytePos alexInput - alexBytePos matchedInput
          r <- act len matchedInput
          modify $ \s -> s { matchedInput = alexInput  }
          case r of
            Nothing -> go
            Just t -> do
              ts <- go
              pure (t:ts)

stateToInt :: LexerMode -> Int
stateToInt InNormal{}      = 0
stateToInt InString{}      = string

}
