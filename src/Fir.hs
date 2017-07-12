module Fir where

fir :: Num a => [a] -> [a] -> [a]
fir bs xs = map (y bs xs) [1..]
y bs xs n = sum (zipWith (*) (reverse bs) (window n xs))
window n = reverse . take n

type Image = [[Double]]

mapIndex f = mapIndex' f 0

mapIndex' f _ [] = []
mapIndex' f i (x:xs) = f i x : mapIndex' f (i+1) xs

mapIndexImg :: ((Int,Int) -> Double) -> Image -> Image
mapIndexImg f = mapIndex (\i -> mapIndex (\j _ -> f (i,j)))

mapImg f = map (map f)

zipImg :: (Double -> Double -> Double) -> Image -> Image -> Image
zipImg f
  = zipWith (zipWith f)

sumImg :: Image -> Double
sumImg img = sum (map sum img)

mask = [[2,2,2]
       ,[2,2,2]
       ,[2,2,2]]

conv img = mapIndexImg (f img) img

f img (i,j) = (/(sumImg mask)) $ sumImg $ zipImg (*) mask $ getMask img i j

width  img = length (head img)
height img = length img

index img i j
  | i >= width img || i < 0 || j >= height img || j < 0 = 0
  | otherwise = (img !! i) !! j

getMask :: Image -> Int -> Int -> Image
getMask img i j
  = [[off (-1) (-1), off 0 (-1), off 1 (-1)]
    ,[off (-1)   0 , off 0   0 , off 1   0 ]
    ,[off (-1)   1 , off 0   1 , off 1   1 ]]
  where off oi oj = index img (i+oi) (j+oj)
