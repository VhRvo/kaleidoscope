module Parser.Program where

import Data.Text (Text)
import Data.Void (Void)
import Parser.Common
import Parser.Expression
import Parser.TopLevel
import Syntax
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char

pProgram :: Parser Program
pProgram = Program <$> pTopLevel `sepBy` semi

-- parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
-- parseProgram = MP.parse (runParser (contents pProgram)) stdin

parseTopLevel :: Text -> Either (ParseErrorBundle Text Void) TopLevel
parseTopLevel = parse (runParser (contents pTopLevel)) stdin

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse (runParser (contents pExpr)) stdin
