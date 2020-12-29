module PlayGraphics.Playing where

import Codec.Picture
import Codec.Picture.Types

import Data.List
import Data.Maybe
import Data.Ord

import PlayGraphics.Ball
import PlayGraphics.Ray
import PlayGraphics.Vec

-- >>> savePngImage "/home/dram/out.png" (ImageRGBF image)
--

balls :: _ => [Ball a]
balls =
    [ Ball (Vec (-0.1) (-0.1)  0.3) 0.08
    , Ball (Vec 0.1 (-0.1)  0.4) 0.08
    , Ball (Vec 0 0.1     0.3) 0.08
    ]

reflect :: _ => Ray a -> Maybe (Ray a)
reflect ray = case catMaybes $ (\b -> (,b) <$> rayBall ray b) <$> balls of
    [] -> Nothing
    xs ->
        let
            (t, b) = minimumBy (comparing fst) xs
            n = rayPar ray t - ballCenter b
        in Just $ Ray (rayPar ray t) (householder n $ rayDir ray)

genLight :: _ => Ray a -> a
genLight ray =
    let light = 0.4 * cosang (rayDir ray) (Vec 1 1 1) + 0.6
    in light * 0.5 + 0.1

gen :: _ => Int -> Bool -> Ray a -> a
gen n fl ray
    | n <= 0 = genLight ray
    | otherwise =
        case reflect ray of
            Nothing -> if fl then genLight ray else 0
            Just nextRay -> genLight nextRay + 0.6 * gen (n - 1) True nextRay

image :: Image PixelRGBF
image = gammaCorrection 2.2 . promoteImage $ (generateImage go w h :: Image PixelF)
    where
        fi = fromIntegral
        w = 512
        h = 512
        toRay x y = Ray (Vec 0 0 0) (Vec (fi x / fi w - 0.5) (fi y / fi h - 0.5) 1)
        go x y = gen 3 False (toRay x y)
