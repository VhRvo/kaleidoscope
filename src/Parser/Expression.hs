module Parser.Expression where

import Control.Applicative hiding (many, some)
import Control.Monad (MonadPlus)
import Control.Monad.Combinators.Expr
import Data.Char (isAlphaNum, isLetter)
import Data.Functor (void)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Parser.Common
-- hiding (runParser)

import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (head, tail)

operators :: [[Operator Parser Expr]]
operators =
  [ [binaryL Times "*", binaryL Division "/"],
    [binaryL Plus "+", binaryL Minus "-"]
  ]
  where
    -- binaryL op opSymbol = InfixL $ Binary op <$ symbol opSymbol
    -- unary op opSymbol = Prefix $ foldr1 (.) <$> some ((UnaryE . Unary op) <$ operator opSymbol)
    -- binaryR op opSymbol = InfixR $ Binary op <$ operator opSymbol
    binaryL op opSymbol = InfixL $ Binary op <$ operator opSymbol

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operators

pTerm :: Parser Expr
pTerm =
  nonLeftRecursive >>= call
  where
    nonLeftRecursive =
      choice
        [ Float <$> try floatLiteral,
          Float . fromIntegral <$> integerLiteral,
          Variable <$> pIdentifier,
          parentheses pExpr
        ]
    call expr =
      choice
        [ (parentheses (pExpr `sepBy` comma) <?> "function application") >>= (call . Call expr),
          pure expr
        ]
