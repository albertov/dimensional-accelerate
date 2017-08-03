{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}
module Numeric.Units.Dimensional.Accelerate ( EltQuantity, (*~), (/~) ) where

import           Data.Array.Accelerate as A
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Product
import           Data.Array.Accelerate.Array.Sugar as A
import           Data.Typeable
import           Numeric.Units.Dimensional (HasDimension, Quantity, Unit)
import qualified Numeric.Units.Dimensional as DP
import           Numeric.Units.Dimensional.Coercion as DP
import           Prelude as P

infixl 7 *~
(*~) :: forall d k a. (P.Num (Exp a), EltQuantity d a) => Exp a -> Unit k d (Exp a) -> Exp (Quantity d a)
a *~ u = A.lift1 ((DP.*~ u) :: Exp a -> Quantity d (Exp a)) a

infixl 7 /~
(/~) :: forall d k a. (P.Fractional (Exp a), EltQuantity d a) => Exp (Quantity d a) -> Unit k d (Exp a) -> Exp a
a /~ u = A.lift1 ((DP./~ u) :: Quantity d (Exp a) -> Exp a) a



type instance EltRepr (DP.Quantity u a) = EltRepr a

type EltQuantity d a = (Elt a, Real a, Typeable d, HasDimension (Proxy d))

instance EltQuantity d a => Elt (Quantity d a)
  where
  eltType _ = eltType (undefined :: a)
  toElt     = Quantity . toElt
  fromElt (Quantity x) = fromElt x

instance cst a => IsProduct cst (Quantity d a) where
  type ProdRepr (Quantity d a) = ((), a)
  fromProd _ (Quantity x) = ((), x)
  toProd _ ((), x) = Quantity x
  prod _ _ = ProdRsnoc ProdRunit

instance (Lift Exp a, EltQuantity d (Plain a)) => Lift Exp (Quantity d a) where
  type Plain (DP.Quantity d a) = Quantity d (Plain a)
  lift (Quantity x) = Exp . Tuple $ NilTup `SnocTup` lift x


instance EltQuantity d a => Unlift Exp (Quantity d (Exp a)) where
  unlift t = Quantity $ Exp $ ZeroTupIdx `Prj` t
