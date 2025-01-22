{-# LANGUAGE TemplateHaskell #-}

module Syntax where

import Data.Bifunctor.Fix
import Data.Bifunctor.TH
import Data.Text (Text)

type Identifier = Text

newtype Program a = Program [TopLevel a]
  deriving (Eq, Ord, Show)

data TopLevel a
  = Function Identifier [Identifier] (Expr a)
  | Extern Identifier [Identifier]
  deriving (Eq, Ord, Show)

newtype Expr a = Expr {unExpr :: Fix ExprF a}
  deriving (Eq, Ord, Show)

data BinaryOp
  = Plus
  | Minus
  | Times
  | Division
  deriving (Eq, Ord, Show)

data ExprF r a
  = FloatF a Double
  | BinaryF a BinaryOp r r
  | VariableF a Identifier
  | CallF a r [r]
  deriving stock (Eq, Ord, Show)

deriveBifunctor ''ExprF
deriveBifoldable ''ExprF
deriveBitraversable ''ExprF
