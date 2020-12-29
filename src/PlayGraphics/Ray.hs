module PlayGraphics.Ray where

import PlayGraphics.Vec

data Ray a = Ray
    { rayStart :: Vec a
    , rayDir :: Vec a
    }
    deriving (Show, Eq)

rayPoint :: _ => Vec a -> Vec a -> Ray a
rayPoint a b = Ray a (b - a)

rayPar :: _ => Ray a -> a -> Vec a
rayPar (Ray a d) t = a + (t .* d)
