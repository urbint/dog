{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Debug.Trace (traceShow)
import           Control.Monad
import           Data.String.Conversions
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Datalog.AST
import           Datalog.Pretty
import           Datalog.Parser

main :: IO ()
main =
 quickCheckWith stdArgs $
   forAll genProgram $ \program -> do
     let bytes = cs $ show (printProgram program)
     traceShow bytes $
       parseDatalog bytes === Right program

genProgram :: Gen Program
genProgram = Program <$> genStatements

genStatements :: Gen [Statement]
genStatements = replicateM 2 genStatement

genStatement :: Gen Statement
genStatement =
  oneof [ Assertion <$> genClause
        , Retraction <$> genClause
        , Query <$> genLiteral
        ]

genClause :: Gen Clause
genClause =
  Clause <$> genLiteral
         <*> genMaybe genBody

genBody :: Gen Body
genBody =
  Body <$> replicateM 1 genLiteral

genPredicateSym :: Gen PredicateSym
genPredicateSym =
  oneof [ PString <$> genString
        , PIdent <$> genIdentifier
        ]

genOp :: Gen Op
genOp = oneof [ pure NEq
              , pure Eq
              ]

genTerms :: Gen [Term]
genTerms = replicateM 1 genTerm

genTerm :: Gen Term
genTerm =
  oneof [ TermVar <$> genVar
        , TermConst <$> genConstant
        ]

genConstant :: Gen Constant
genConstant =
  oneof [ ConstIdent <$> genIdentifier
        , ConstString <$> genString
        ]

genLiteral :: Gen Literal
genLiteral = oneof [ lpred, lterm ]
  where
    lpred = LPred <$> genPredicateSym <*> genTerms
    lterm = LTerm <$> genOp <*> genTerm <*> genTerm

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe f = do
  oneof [ pure Nothing
        , Just <$> f
        ]

genIdentifier :: Gen Text
genIdentifier = do
  pure "ident"

genVar :: Gen Text
genVar = do
  pure "var"

genString :: Gen Text
genString = do
--  txt <- arbitrary
  pure (T.pack "\"" <> "foo" <> T.pack "\"")


-- Right (Program {statements = [
--   Assertion (Clause (LTerm Eq (TermConst (ConstIdent "ident !")) (TermConst (ConstIdent "var"))) (Just (Body [LPred (PIdent "ident") [TermConst (ConstIdent "ident")]]))),Assertion (Clause (LPred (PIdent "ident") [TermConst (ConstIdent "var")]) Nothing)]})


-- Right (Program {statements = [
--   Assertion (Clause (LTerm NEq (TermConst (ConstIdent "ident")) (TermVar "var")) (Just (Body [LPred (PIdent "ident") [TermConst (ConstIdent "ident")]]))),Assertion (Clause (LPred (PIdent "ident") [TermVar "var"]) Nothing)]})
