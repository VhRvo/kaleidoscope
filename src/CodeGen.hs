module CodeGen where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type (double)
import Syntax

type SymbolTable = [(Text, Operand)]

type Name = Text

-- data CodeGenState
--   = CodeGenState
--   { -- Name of the active block to append to
--     currentBlock :: Name,
--     -- Blocks for function
--     blocks :: Map Name BlockState,
--     -- Function scope symbol table
--     symbolTable :: SymbolTable,
--     -- Count of basic blocks
--     blockCount :: Int

--   }
