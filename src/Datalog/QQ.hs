{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Datalog.QQ
-- Description : Compile-time facilities for dealing with Datalog schema / queries
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module Datalog.QQ
  ( -- * QuasiQuoters
    datalog
  ) where
--------------------------------------------------------------------------------
import           Data.Data
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax hiding (Name, Clause)
--------------------------------------------------------------------------------
import           Datalog.AST
import           Datalog.Parser
--------------------------------------------------------------------------------

-- | QuasiQuoter for Datalog 'Schema' definitions
datalog :: QuasiQuoter
datalog = def { quoteExp = qqDatalog }

-- | Parsing the Datalog Schema
def :: QuasiQuoter
def = QuasiQuoter
    { quoteExp  = fail "quotExp: not implemented"
    , quotePat  = fail "quotPat: not implemented"
    , quoteDec  = fail "quotDec: not implemented"
    , quoteType = fail "quotType: not implemented"
    }

qqDatalog
  :: String
  -> Q Exp
qqDatalog
  = either fail liftDataWithText
  . parseDatalog
  . T.encodeUtf8
  . T.pack
      where
        liftDataWithText :: Program -> Q Exp
        liftDataWithText program = do
          dataToExpQ (withText `extQ` goProgram) program

        extQ f g a = maybe (f a) g (cast a)
        withText a = liftText <$> cast a

        goProgram (Program stmts) = do
          newStmts <- mapM goStmt stmts
          Just [| Program $(listE newStmts) |]

        goStmt (Assertion c) = do
          newClause <- goClause c
          Just [| Assertion $newClause |]

        goStmt (Retraction c) = do
          newClause <- goClause c
          Just [| Retraction $newClause |]

        goStmt (Query literal) = do
          newLit <- goLiteral literal
          Just [| Query $newLit |]

        goClause (Clause lit Nothing) = do
          newLit <- goLiteral lit
          Just [| Clause $newLit Nothing |]

        goClause (Clause lit (Just body)) = do
          newBody <- goBody body
          newLit <- goLiteral lit
          Just [| Clause $newLit $newBody |]

        goBody (Body lits)= do
          newLits <- mapM goLiteral lits
          Just [| Body $(listE newLits) |]

        goLiteral (LPred predicateSym terms) = do
          newTerms <- mapM goTerm terms
          newPred <- goPred predicateSym
          Just [| LPred $newPred $(listE newTerms) |]

        goLiteral (LTerm op termA termB) = do
          newOp <- goOp op
          newTermA <- goTerm termA
          newTermB <- goTerm termB
          Just [| LPred $newOp $newTermA $newTermB |]

        goTerm (TermVar txt) =
          Just [| TermVar $(litE $ stringL $ T.unpack txt) |]

        goTerm (TermConst constant) = do
          newConstant <- goConstant constant
          Just [| TermConst $newConstant |]

        goPred (PString txt) =
          Just [| PString $(litE $ stringL $ T.unpack txt) |]

        goPred (PIdent txt) =
          Just [| PIdent $(litE $ stringL $ T.unpack txt) |]

        goOp Eq = Just [| Eq |]
        goOp NEq = Just [| NEq |]

        goConstant (ConstIdent txt) =
          Just [| ConstIdent $(litE $ stringL $ T.unpack txt) |]
        goConstant (ConstString txt) =
          Just [| ConstString $(litE $ stringL $ T.unpack txt) |]

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)
