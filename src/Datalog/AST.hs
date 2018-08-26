{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Datalog.AST
-- Description : Datalog abstract syntax tree representation
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module Datalog.AST where
--------------------------------------------------------------------------------
import GHC.Generics    (Generic)
import Data.Typeable   (Typeable)
import Data.Data       (Data)
import Data.Text       (Text)
import Control.DeepSeq (NFData)
--------------------------------------------------------------------------------

-- | A Datalog 'Program'
newtype Program
  = Program { statements :: [ Statement ] }
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | A Datalog 'Statement'
data Statement
  = Assertion Clause
  | Retraction Clause
  | Query Literal
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Statement

-- | A Datalog 'Clause'
data Clause
  = Clause Literal (Maybe Body)
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Clause

-- | A Datalog 'Body'
newtype Body = Body [Literal]
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Body

-- | A Datalog 'Literal'
data Literal
  = LPred PredicateSym [Term]
  | LTerm Op Term Term
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Literal

-- | A Datalog 'Op'
data Op = Eq | NEq
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Op

-- | A Datalog 'PredicateSym'
data PredicateSym
  = PString Text
  | PIdent Text
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData PredicateSym

-- | A Datalog 'Term'
data Term
  = TermVar Text
  | TermConst Constant
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Term

-- | A Datalog 'Constant'
data Constant
  = ConstIdent Text
  | ConstString Text
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Constant



