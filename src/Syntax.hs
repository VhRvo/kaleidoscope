module Syntax where

import Data.Text (Text)

type Identifier = Text

newtype Program = Program [TopLevel]
  deriving (Eq, Ord, Show)

data TopLevel
  = Function Identifier [Identifier] Expr
  | Extern Identifier [Identifier]
  deriving (Eq, Ord, Show)

data Expr
  = Float Double
  | Binary BinaryOp Expr Expr
  | Variable Identifier
  | Call Expr [Expr]
  deriving (Eq, Ord, Show)

data BinaryOp
  = Plus
  | Minus
  | Times
  | Division
  deriving (Eq, Ord, Show)
