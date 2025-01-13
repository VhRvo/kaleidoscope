module Pretty where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import Error.Diagnose qualified as Diagnose
import Error.Diagnose.Compat.Megaparsec
import Text.Megaparsec
import Text.Pretty.Simple

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint =
  pPrintOpt CheckColorTty $
    defaultOutputOptionsLightBg
      { outputOptionsIndentAmount = 2,
        outputOptionsCompact = True,
        outputOptionsCompactParens = True
      }

fromParseErrorBundle :: String -> ParseErrorBundle Text Void -> String -> Diagnose.Diagnostic Text
fromParseErrorBundle fileName bundle =
  Diagnose.addFile diagnose fileName
  where
    diagnose =
      errorDiagnosticFromBundle
        Nothing
        "Parse error on input"
        Nothing
        bundle

instance HasHints Void msg where
  -- hits :: Void -> [Note msg]
  hints _ = mempty
