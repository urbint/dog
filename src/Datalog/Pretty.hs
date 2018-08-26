{-# LANGUAGE FlexibleInstances   #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Datalog.Pretty
-- Description : Pretty-printing for the Datalog AST
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module Datalog.Pretty where
--------------------------------------------------------------------------------
import Data.Text.Prettyprint.Doc
--------------------------------------------------------------------------------
import Datalog.AST
--------------------------------------------------------------------------------
-- | Pretty prints a 'Document'
printProgram
  :: Program
  -> Doc ann
printProgram (Program ss)
  = vsep (printStatement <$> ss)

-- | Pretty prints a 'Statement'
printStatement
  :: Statement
  -> Doc ann
printStatement (Assertion clause)
  = printClause clause <> pretty '.'
printStatement (Retraction clause)
  = printClause clause <> pretty '~'
printStatement (Query literal)
  = printLiteral literal <> pretty '?'

-- | Pretty prints a 'Clause'
printClause
  :: Clause
  -> Doc ann
printClause (Clause literal Nothing)
  = printLiteral literal
printClause (Clause literal (Just body))
  = hsep [ printLiteral literal
         , pretty ":-"
         , printBody body
         ]

-- | Pretty prints a 'Body'
printBody
  :: Body
  -> Doc ann
printBody (Body terms)
  = hsep
  $ punctuate (pretty ",") (printLiteral <$> terms)

-- | Pretty prints a 'Literal'
printLiteral
  :: Literal
  -> Doc ann
printLiteral (LPred predicateSym terms)
  = hsep [ printPredicateSym predicateSym
         , pretty '('
         , hsep $ punctuate (pretty ",") (printTerm <$> terms)
         , pretty ')'
         ]
printLiteral (LTerm op termA termB)
  = hsep [ printTerm termA
         , printOp op
         , printTerm termB
         ]

-- | Pretty prints a 'PredicateSym'
printPredicateSym
  :: PredicateSym
  -> Doc ann
printPredicateSym (PString txt)
  = pretty txt
printPredicateSym (PIdent txt)
  = pretty txt

-- | Pretty prints a 'Op'
printOp
  :: Op
  -> Doc ann
printOp Eq
  = pretty '='
printOp NEq
  = pretty "!="

-- | Pretty prints a 'Term'
printTerm
  :: Term
  -> Doc ann
printTerm (TermVar txt)
  = pretty txt
printTerm (TermConst constant)
  = printConstant constant

-- | Pretty prints a 'Constant'
printConstant
  :: Constant
  -> Doc ann
printConstant (ConstString txt)
  = pretty txt
printConstant (ConstIdent txt)
  = pretty txt


