{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Datalog.LexerUtils
-- Description : Lexer / Tokenizer of Datalog document per Datalog specification
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module Datalog.LexerUtils where

import           Control.DeepSeq
import           Control.Monad.State
import           Data.Data
import           Data.Text                (Text, strip)

import qualified Data.Text.Encoding       as T
import           Data.Word
import           GHC.Generics


import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B

import qualified Data.ByteString.Internal as B (w2c)

-- | Token unit for lexing the Datalog specification
data Token
  = TokenPunctuator Text
  | TokenVariable Text
  | TokenIdentifier Text
  | TokenString Text
  | TokenError Error
  deriving (Show, Eq, Data, Read, Generic, Typeable)

data Error
  = ConversionError Text Text
  | LexerError Text
  | NoMatch Text
  | UntermString
  deriving (Show, Eq, Data, Read, Generic, Typeable)

instance NFData Error

data AlexInput = AlexInput
  { alexChar    :: {-# UNPACK #-} !Char
  , alexStr     :: {-# UNPACK #-} !B.ByteString
  , alexBytePos :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

data LexerState
  = LexerState
  { matchedInput :: !AlexInput
  , lexerMode    :: !LexerMode
  , stringBuffer :: !ByteString
  } deriving (Show, Eq)

type Action = Int -> AlexInput -> State LexerState (Maybe Token)

token :: (ByteString -> Token) -> Action
token f inputLength _ = do
  LexerState {..} <- get
  pure . pure $ f (B.take inputLength (alexStr matchedInput))

token_ :: Token -> Action
token_ = token . const

data LexerMode
  = InNormal
  | InString
  deriving (Show, Eq)

appendMode :: Action
appendMode = action

action :: Action
action len bs = do
  s@LexerState {..} <- get
  put s { stringBuffer = stringBuffer `B.append` B.take len (alexStr bs) }
  pure Nothing

endMode :: Action
endMode _ _ = do
  mode <- gets lexerMode
  case mode of
    InNormal -> pure Nothing
    InString -> apply
  where
    apply = do
      buf <- gets stringBuffer
      modify $ \s -> s { lexerMode = InNormal, stringBuffer = mempty }
      pure $ Just $ TokenString (T.decodeUtf8 buf)

eofAction :: State LexerState [Token]
eofAction = do
  mode <- gets lexerMode
  pure $ case mode of
    InString      -> [TokenError UntermString]
    InNormal      -> []

errorAction :: AlexInput -> State LexerState [Token]
errorAction AlexInput {..} =
  pure [TokenError (NoMatch (T.decodeUtf8 alexStr))]

startString :: Action
startString _ _ =
  Nothing <$ do
    modify $ \s -> s { lexerMode = InString }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput {..} =
  case B.uncons alexStr of
    Nothing -> Nothing
    Just (c, rest) ->
      Just (c, AlexInput {
        alexChar = B.w2c c,
        alexStr = rest,
        alexBytePos = alexBytePos+1
      })

lexPunctuator :: ByteString -> Token
lexPunctuator = TokenPunctuator . T.decodeUtf8

lexVariable :: ByteString -> Token
lexVariable = TokenVariable . T.decodeUtf8

lexIdentifier :: ByteString -> Token
lexIdentifier = TokenIdentifier . strip . T.decodeUtf8
