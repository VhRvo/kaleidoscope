module Parser.Expression where

import Control.Applicative hiding (many, some)
import Control.Monad (MonadPlus)
import Control.Monad.Combinators.Expr
-- hiding (runParser)

import Data.Bifunctor
import Data.Bifunctor.Fix
import Data.Char (isAlphaNum, isLetter)
import Data.Functor (void)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Parser.Common
import Syntax hiding (Expr)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (head, tail)

type Expr = Fix ExprF

operators :: [[Operator Parser (Expr ())]]
operators =
  [ [binaryL Times "*", binaryL Division "/"],
    [binaryL Plus "+", binaryL Minus "-"]
  ]
  where
    -- binaryL op opSymbol = InfixL $ Binary op <$ symbol opSymbol
    -- unary op opSymbol = Prefix $ foldr1 (.) <$> some ((UnaryE . Unary op) <$ operator opSymbol)
    -- binaryR op opSymbol = InfixR $ Binary op <$ operator opSymbol
    binaryL op opSymbol = InfixL ((\left right -> In (BinaryF () op left right)) <$ operator opSymbol)

pExpr :: Parser (Expr ())
pExpr = makeExprParser pTerm operators

pTerm :: Parser (Expr ())
pTerm = nonLeftRecursive >>= call
  where
    nonLeftRecursive :: Parser (Expr ())
    nonLeftRecursive =
      ( In
          <$> choice
            [ FloatF () <$> try floatLiteral,
              FloatF () . fromIntegral <$> integerLiteral,
              VariableF () <$> pIdentifier
            ]
      )
        <|> parentheses pExpr
    call :: Expr () -> Parser (Expr ())
    call expr =
      choice
        [ (parentheses (pExpr `sepBy` comma) <?> "function application") >>= (call . In . CallF () expr),
          pure expr
        ]
