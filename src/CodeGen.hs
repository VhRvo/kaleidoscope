{-# LANGUAGE StrictData #-}

module CodeGen where

import Control.Monad.State.Strict hiding (state)
import Data.Bifunctor
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Word
import LLVM.AST hiding (index)
import LLVM.AST qualified as AST
import LLVM.AST.Global
import LLVM.AST.Constant qualified as C
import LLVM.AST.Linkage qualified as L
import LLVM.AST.Type (double)

-- import Data.ByteString qualified as BS

type SymbolTable = [(Text, Operand)]

data CodeGenState
  = CodeGenState
  { -- Name of the active block to append to
    currentBlock :: Name,
    -- Blocks for function
    blocks :: Map Name BlockState,
    -- Function scope symbol table
    symbolTable :: SymbolTable,
    -- Count of basic blocks
    blockCount :: Int,
    -- Count of unnamed instructions
    count :: Word,
    -- Name Supply
    names :: Names
  }
  deriving (Show)

data BlockState
  = BlockState
  { -- Block index
    index :: Int,
    -- Stack of instructions
    stack :: [Named Instruction],
    -- Block terminator
    term :: Maybe (Named Terminator)
  }
  deriving (Show)

newtype CodeGen a = CodeGen
  {runCodeGen :: State CodeGenState a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState CodeGenState
    )

-- Names

type Names = Map ShortByteString Word

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName name names =
  case Map.lookup name names of
    Nothing -> (name, Map.insert name 1 names)
    Just index ->
      ( name <> BSS.pack (integralToWord8sBase10 index),
        Map.insert name (index + 1) names
      )

integralToWord8sBase10 :: forall a. (Integral a) => a -> [Word8]
integralToWord8sBase10 = (`go` [])
  where
    go :: a -> [Word8] -> [Word8]
    go integral rest
      | integral < 10 = fromIntegral integral : rest
      | otherwise =
          let (quotient, remainder) = divMod integral 10
           in go quotient (fromIntegral remainder : rest)

newtype LLVM a = LLVM (State AST.Module a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState AST.Module
    )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM module' (LLVM m) = execState m module'

emptyModule :: ShortByteString -> AST.Module
emptyModule label = defaultModule {moduleName = label}

addDefinition :: Definition -> LLVM ()
addDefinition definition = do
  definitions <- gets moduleDefinitions
  modify $ \state -> state {moduleDefinitions = definitions ++ [definition]}

define :: Type -> ShortByteString -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define returnType label parameterTypes body =
  addDefinition $
    GlobalDefinition $
      functionDefaults
        { name = Name label,
          parameters = ((\(tpe, name) -> Parameter tpe name []) <$> parameterTypes, False),
          returnType = returnType,
          basicBlocks = body
        }

external :: Type -> ShortByteString -> [(Type, Name)] -> LLVM ()
external returnType label parameterTypes =
  addDefinition $
    GlobalDefinition $
      functionDefaults
        { name = Name label,
          linkage = L.External,
          parameters = ((\(tpe, name) -> Parameter tpe name []) <$> parameterTypes, False),
          returnType = returnType,
          basicBlocks = []
        }

entry :: CodeGen Name
entry = gets currentBlock

addBlock :: ShortByteString -> CodeGen Name
addBlock blockName = do
  blocks' <- gets blocks
  index <- gets blockCount
  names' <- gets names
  let new = emptyBlock index
      (qualifiedName, supply) = first Name $ uniqueName blockName names'
  modify $ \state ->
    state
      { blocks = Map.insert qualifiedName new blocks',
        blockCount = index + 1,
        names = supply
      }
  pure qualifiedName

setBlock :: Name -> CodeGen Name
setBlock blockName = do
  modify $ \state -> state {currentBlock = blockName}
  pure blockName

getBlock :: CodeGen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> CodeGen ()
modifyBlock newState = do
  active <- getBlock
  modify $ \state -> state {blocks = Map.insert active newState (blocks state)}

current :: CodeGen BlockState
current = do
  active <- getBlock
  blocks' <- gets blocks
  case Map.lookup active blocks' of
    Just x -> pure x
    Nothing -> error $ "No such block: " <> show active

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (index . snd))

createBlocks :: CodeGenState -> [BasicBlock]
createBlocks = fmap makeBlock . sortBlocks . Map.toList . blocks

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (label, BlockState _ stack term) =
  BasicBlock
    label
    (reverse stack)
    (fromJust (error $ "Block has no terminator: " <> show label) id term)

entryBlockName :: ShortByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock index = BlockState index [] Nothing

emptyCodeGen :: CodeGenState
emptyCodeGen =
  CodeGenState
    (Name entryBlockName)
    Map.empty
    []
    1
    0
    Map.empty

fresh :: CodeGen Word
fresh = do
  count' <- (+1) <$> gets count
  modify $ \state -> state { count = count'}
  pure count'

local :: Name -> Operand
local = LocalReference double

externFunction :: Name -> Operand
externFunction = ConstantOperand . C.GlobalReference double

