module Parser.TopLevel where

import Parser.Common
import Parser.Expression hiding (Expr)
import Syntax
import Text.Megaparsec

pTopLevel :: Parser (TopLevel ())
pTopLevel = pExtern <|> pFunction

pExtern :: Parser (TopLevel ())
pExtern = label "extern function declaration" $ do
  reserved "extern"
  name <- pIdentifier
  parameters <- parentheses (pIdentifier `sepBy` comma)
  pure $ Extern name parameters

pFunction :: Parser (TopLevel ())
pFunction = label "function declaration" $ do
  reserved "def"
  name <- pIdentifier
  parameters <- parentheses (pIdentifier `sepBy` comma)
  Function name parameters . Expr <$> pExpr
