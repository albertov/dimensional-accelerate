{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Units.Dimensional.AccelerateSpec ( spec, main ) where

import           Numeric.Units.Dimensional.Accelerate
import           Numeric.Units.Dimensional hiding ((*~), (/~))
import           Numeric.Units.Dimensional.SIUnits
import           Numeric.Units.Dimensional.NonSI

import           Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native      as Native
import qualified Data.Array.Accelerate.Interpreter      as Interpreter
import           Data.List

import           Test.Hspec
import           Prelude as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Native.run" $ do
    it "works with scalar" $ do
      let asMeters = 2 *~ weaken meter :: Exp (Length Double)
          asFeet   = asMeters /~ foot
      show (Native.run (unit asFeet)) `shouldSatisfy` isInfixOf "6.5616"

    it "works with array" $ do
      let asMeters = A.map (*~ weaken meter) arr :: Acc (Array DIM1 (Length Double))
          asFeet   = A.map (/~ foot) asMeters
          arr = fill (constant (Z :. 3)) 2
      show (Native.run (asFeet)) `shouldSatisfy` isInfixOf "6.5616"

  describe "Interpreter.run" $ do
    it "works with scalar" $ do
      let asMeters = 2 *~ weaken meter :: Exp (Length Double)
          asFeet   = asMeters /~ foot
      show (Interpreter.run (unit asFeet)) `shouldSatisfy` isInfixOf "6.5616"

    it "works with array" $ do
      let asMeters = A.map (*~ weaken meter) arr :: Acc (Array DIM1 (Length Double))
          asFeet   = A.map (/~ foot) asMeters
          arr = fill (constant (Z :. 3)) 2
      show (Interpreter.run (asFeet)) `shouldSatisfy` isInfixOf "6.5616"
