{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
module Numeric.Units.Dimensional.Accelerate.Prelude (
  recip
, (/)
, (*)
, negate
, (+)
, (-)
, abs
, signum
, sqrt
, (**)
, exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
, logBase
, atan2
) where

import           Numeric.Units.Dimensional.Accelerate( EltQuantity )
import           Numeric.Units.Dimensional.Prelude (
                 Quantity, Floating, Fractional, Real, RealFloat, Num, HasDimension,
                 Dimensionless, DOne, Sqrt, Recip)
import qualified Numeric.Units.Dimensional.Prelude as DP
import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate (Exp, Elt)
import           Data.Proxy
import           Data.Typeable

infixr 8  **
infixl 7  *, /
infixl 6  +, -

(*) :: forall a d1 d2.
         ( Num (Exp a)
         , EltQuantity d1 a
         , EltQuantity d2 a
         , EltQuantity (d1 DP.* d2) a
         )
    => Exp ((Quantity d1 a))
    -> Exp ((Quantity d2 a))
    -> Exp ((Quantity (d1 DP.* d2) a))
(*) = A.lift2 ((DP.*) :: Quantity d1 (Exp a) -> Quantity d2 (Exp a) -> (Quantity (d1 DP.* d2) (Exp a)))

(/) :: forall a d1 d2.
         ( Fractional (Exp a)
         , EltQuantity d1 a
         , EltQuantity d2 a
         , EltQuantity (d1 DP./ d2) a
         )
    => Exp ((Quantity d1 a))
    -> Exp ((Quantity d2 a))
    -> Exp ((Quantity (d1 DP./ d2) a))
(/) = A.lift2 ((DP./) :: Quantity d1 (Exp a) -> Quantity d2 (Exp a) -> (Quantity (d1 DP./ d2) (Exp a)))

recip :: forall u a.  (EltQuantity u a, EltQuantity (Recip u) a, Fractional (Exp a))
      => Exp (Quantity u a)
      -> Exp (Quantity (Recip u) a)
recip = A.lift1 (DP.recip :: Quantity u (Exp a) -> Quantity (Recip u) (Exp a))

negate :: forall u a. (EltQuantity u a, Num (Exp a))
       => Exp (Quantity u a)
       -> Exp (Quantity u a)
negate = A.lift1 (DP.negate :: Quantity u (Exp a) -> Quantity u (Exp a))

(+) :: forall u a. (Real a, Elt a, A.IsScalar a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
    -> Exp (Quantity u a)
(+) = A.lift2 ((DP.+) :: Quantity u (Exp a) -> Quantity u (Exp a) -> Quantity u (Exp a))

(-) :: forall u a. (Real a, Elt a, A.IsScalar a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
    -> Exp (Quantity u a)
(-) = A.lift2 ((DP.-) :: Quantity u (Exp a) -> Quantity u (Exp a) -> Quantity u (Exp a))

abs :: forall u a. (Real a, Elt a, A.IsScalar a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
abs = A.lift1 (DP.abs :: Quantity u (Exp a) -> Quantity u (Exp a))

signum :: forall u a. (Real a, Elt a, A.IsScalar a, Num (Exp a), Typeable u, HasDimension (Proxy u))
       => Exp (Quantity u a) -> Exp (Dimensionless a)
signum = A.lift1 (DP.signum :: Quantity u (Exp a) -> Quantity DOne (Exp a))

sqrt :: forall u a.
     ( Elt a
     , A.Floating a
     , A.IsScalar a
     , Real a
     , Typeable u
     , HasDimension (Proxy u)
     , Typeable (Sqrt u)
     , HasDimension (Proxy (Sqrt u))
     )
     => Exp (Quantity u a) -> Exp (Quantity (Sqrt u) a)
sqrt = A.lift1 (DP.sqrt :: Quantity u (Exp a) -> Quantity (Sqrt u) (Exp a))


exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
  :: forall a. (Floating (Exp a), A.IsScalar a, Real a, Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a)
exp   = A.lift1 (DP.exp :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
log   = A.lift1 (DP.log :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
sin   = A.lift1 (DP.sin :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
cos   = A.lift1 (DP.cos :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
tan   = A.lift1 (DP.tan :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
asin  = A.lift1 (DP.asin :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
acos  = A.lift1 (DP.acos :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
atan  = A.lift1 (DP.atan :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
sinh  = A.lift1 (DP.sinh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
cosh  = A.lift1 (DP.cosh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
tanh  = A.lift1 (DP.tanh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
asinh = A.lift1 (DP.asinh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
acosh = A.lift1 (DP.acosh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
atanh = A.lift1 (DP.atanh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))

(**)
  :: forall a. (Floating (Exp a), EltQuantity DOne a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a) -> Exp (Dimensionless a)
(**) = A.lift2 ((DP.**) :: Quantity DOne (Exp a) -> Quantity DOne (Exp a) -> Quantity DOne (Exp a))

logBase
  :: forall a. (Floating (Exp a), EltQuantity DOne a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a) -> Exp (Dimensionless a)
logBase = A.lift2 (DP.logBase :: Quantity DOne (Exp a) -> Quantity DOne (Exp a) -> Quantity DOne (Exp a))

atan2
  :: forall u a. (RealFloat (Exp a), EltQuantity u a)
  => Exp (Quantity u a) -> Exp (Quantity u a) -> Exp (Dimensionless a)
atan2 = A.lift2 (DP.atan2 :: Quantity u (Exp a) -> Quantity u (Exp a) -> Quantity DOne (Exp a))
