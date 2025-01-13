module Repl where

import Control.Monad.IO.Class
import Data.Text qualified as T
import Error.Diagnose as Diagnose
import Parser
import Pretty
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  case parseExpr (T.pack line) of
    Left errorBundle -> do
      liftIO $ print errorBundle
      Diagnose.printDiagnostic
        Diagnose.stderr
        Diagnose.WithUnicode
        (Diagnose.TabSize 4)
        Diagnose.defaultStyle
        (fromParseErrorBundle stdin errorBundle line)
    Right expr -> pPrint expr

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      mInput <- getInputLine "ready> "
      case mInput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          liftIO $ putStrLn input
          liftIO (process input) >> loop
