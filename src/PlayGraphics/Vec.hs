{-# OPTIONS -funbox-strict-fields #-}

module PlayGraphics.Vec where

import Control.Applicative
import Data.Functor.Compose

data Vec a = Vec !a !a !a
    deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Mat a = Mat (Vec (Vec a))
    deriving (Show, Eq, Functor)
    deriving Applicative via (Compose Vec Vec)

instance Applicative Vec where
    pure x = Vec x x x
    Vec fx fy fz <*> Vec ax ay az = Vec (fx ax) (fy ay) (fz az)

instance Num a => Num (Vec a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Num a => Num (Mat a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

(.*) :: _ => a -> f a -> f a
(.*) = fmap . (*)

(.*.) :: _ => f a -> f a -> a
a .*. b = sum (liftA2 (*) a b)

(><) :: _ => Vec a -> Vec a -> Vec a
Vec ax ay az >< Vec bx by bz =
    Vec (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

(-*) :: _ => f (g a) -> g a -> f a
a -* x = fmap (.*. x) a

(-*-) :: _ => f (g a) -> g (h a) -> f (h a)
a -*- b = fmap (sequenceA b -*) a

normSq :: _ => f a -> a
normSq a = a .*. a

norm :: _ => f a -> a
norm = sqrt . normSq

cosang :: _ => Vec a -> Vec a -> a
a `cosang` b = ((/ norm a) <$> a) .*. ((/ norm b) <$> b)

householder :: _ => f a -> f a -> f a
householder v x = x - (2 * (x .*. nv)) .* nv
    where nv = recip (norm v) .* v
