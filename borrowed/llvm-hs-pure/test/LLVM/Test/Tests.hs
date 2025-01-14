module LLVM.Test.Tests where

import qualified LLVM.Test.DataLayout as DataLayout
import qualified LLVM.Test.IRBuilder as IRBuilder
import Test.Tasty

tests =
  testGroup
    "llvm-hs"
    [ DataLayout.tests,
      IRBuilder.tests
    ]
