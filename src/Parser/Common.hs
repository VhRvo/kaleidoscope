module Parser.Common where

import Control.Monad (MonadPlus)
import Control.Applicative hiding (many, some)
import Control.Applicative.Combinators

import Data.Text (Text)
import Data.Void (Void)

import Data.Set (Set)
import Data.Set qualified as Set

import Text.Megaparsec -- hiding (runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

newtype Parser a
  = Parser {runParser :: Parsec Void Text a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      Alternative,
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

reserved :: Text -> Parser ()
reserved word = lexeme . try $ string word *> notFollowedBy alphaNumChar

reservedWords :: Set Text
reservedWords =
    Set.fromList
      [ "def" ]

operatorChars :: Set Char
operatorChars =
    Set.fromList "~!@#$%^&*()-+\\|<>?/"

