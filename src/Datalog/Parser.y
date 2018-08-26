{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Datalog.Parser
-- Description : Parser for Datalog specification
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
-- <https://docs.racket-lang.org/datalog/datalog.html>
--
--------------------------------------------------------------------------------
module Datalog.Parser
  ( parseDatalog
  ) where
--------------------------------------------------------------------------------
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.ByteString (ByteString)
--------------------------------------------------------------------------------
import           Datalog.Lexer
import           Datalog.AST
--------------------------------------------------------------------------------
}

%name datalog Program

%monad { Either String } { (>>=) } { return }

%tokentype { Token }
%error { parseError }

%token
  IDENTIFIER    { TokenIdentifier $$ }
  VARIABLE      { TokenVariable $$ }
  STRING	{ TokenString $$ }
  '.'           { TokenPunctuator "." }
  '~'           { TokenPunctuator "~" }
  '?'           { TokenPunctuator "?" }
  ':-'          { TokenPunctuator ":-" }
  ','           { TokenPunctuator "," }
  '('           { TokenPunctuator "(" }
  ')'           { TokenPunctuator ")" }
  '='           { TokenPunctuator "=" }
  '!='          { TokenPunctuator "!=" }

%%

Program :: { Program }
  : Statements { Program (reverse $1) }

Statements :: { [Statement] }
  : { [] }
  | Statements Statement { $2 : $1 }

Statement :: { Statement }
  : Clause '.' { Assertion $1 }
  | Clause '~' { Retraction $1 }
  | Literal '?'  { Query $1 }

Clause :: { Clause }
  : Literal ':-' Body { Clause $1 $ Just (Body (reverse $3)) }
  | Literal { Clause $1 Nothing }

Body :: { [Literal] }
  : Literal { [$1] }
  | Literal ',' Body { $1 : $3 }

Literal :: { Literal }
  : PredicateSym '(' ')' { LPred $1 [] }
  | PredicateSym '(' Terms ')' { LPred $1 (reverse $3) }
  | PredicateSym { LPred $1 [] }
  | Term '=' Term { LTerm Eq $1 $3 }
  | Term '!=' Term { LTerm NEq $1 $3 }

PredicateSym :: { PredicateSym }
  : IDENTIFIER { PIdent $1 }
  | STRING { PString $1 }

Terms :: { [Term] }
  : Term { [$1] }
  | Terms ',' Term { $3 : $1 }

Term :: { Term }
  : VARIABLE { TermVar $1 }
  | Constant { TermConst $1 }

Constant :: { Constant }
  : IDENTIFIER { ConstIdent $1 }
  | STRING { ConstString $1 }

{

-- | Parses a Datalog 'Program'
parseDatalog :: ByteString -> Either String Program
parseDatalog = datalog . getTokens

parseError :: [Token] -> Either String a
parseError [] = Left "Parse error"
parseError tks =
  Left $ "Parse error: " ++ explainToken (head tks)
    where
      explainToken (TokenError err) = explainError err
      explainToken t = show t
      explainError (ConversionError errMsg s)
        = T.unpack errMsg ++ " at " ++ T.unpack s
      explainError (LexerError errMsg)
        = T.unpack errMsg
      explainError (NoMatch c)
        = "Couldn't match on " ++ T.unpack c
      explainError UntermString
        = "Unterminated string"
}
