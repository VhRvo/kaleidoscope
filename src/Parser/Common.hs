{-# LANGUAGE StrictData #-}

module Parser.Common where

import Control.Applicative hiding (many, some)
-- import Control.Applicative.Combinators
import Control.Monad (MonadPlus)
import Data.Char (isAlphaNum, isLetter)
import Data.Functor (void)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec -- hiding (runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (head, tail)

newtype Parser a
  = Parser {runParser :: Parsec Void Text a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadFail,
      MonadPlus,
      MonadParsec Void Text
    )

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

semi, comma, colon :: Parser ()
semi = void $ symbol ";"
comma = void $ symbol ","
colon = void $ symbol ":"

braces, squares, parentheses :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
squares = between (symbol "[") (symbol "]")
parentheses = between (symbol "(") (symbol ")")

reserved :: Text -> Parser ()
reserved word =
  lexeme . try $
    string word *> notFollowedBy alphaNumChar

operator :: Text -> Parser ()
operator opSymbol =
  lexeme . try $
    string opSymbol *> notFollowedBy (satisfy (`Set.member` operatorChars))

reservedWords :: Set Text
reservedWords =
  Set.fromList
    [ "def",
      "extern"
    ]

operatorChars :: Set Char
operatorChars =
  Set.fromList "~!@#$%^&*()-+\\|<>?/"

boolLiteral :: Parser Bool
boolLiteral = True <$ reserved "True" <|> False <$ reserved "False"

floatLiteral :: Parser Double
floatLiteral = lexeme $ L.signed sc L.float

integerLiteral :: Parser Int
integerLiteral = lexeme $ L.signed sc L.decimal

stringLiteral :: Parser Text
stringLiteral =
  between (string "\"") (string "\"") $
    takeWhileP (Just "string literal") (/= '"')

pIdentifier :: Parser Text
pIdentifier = label "identifier" . lexeme $ do
  head <- satisfy (\ch -> isLetter ch || ch == '_')
  tail <- many (satisfy (\ch -> isAlphaNum ch || ch == '_'))
  let identifier' = head : tail
      identifier = T.pack identifier'
  if identifier `Set.member` reservedWords
    then fail $ "reversed word cannot be seem as identifier: " <> identifier'
    else pure identifier

contents :: Parser a -> Parser a
contents parser = space *> parser <* eof

stdin :: String
stdin = "<stdin>"
