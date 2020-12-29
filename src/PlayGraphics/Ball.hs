module PlayGraphics.Ball where

import PlayGraphics.Ray
import PlayGraphics.Vec

data Ball a = Ball
    { ballCenter :: Vec a
    , ballRadius :: a
    }
    deriving (Show, Eq)

rayBall :: _ => Ray a -> Ball a -> Maybe a
rayBall (Ray a d) (Ball c r) =
        if det >= 0 && t >= 0 then Just t
        else Nothing
    where
        p = (c - a) .*. d
        q = normSq (c - a) - r * r
        z = normSq d
        det = p * p - z * q
        t = (p - sqrt det) / z
