module HOD.Random where 

import System.Random

import System.Random.Shuffle

import System.IO.Unsafe


type Seed = Int
type SampleSize = Int
type NumSamples = Int
type NumRow = Int 
type NumCol = Int


getRandomInRange :: Random a => a -> a -> a
getRandomInRange = curry $ unsafePerformIO . randomRIO


reshapeHelper :: NumRow -> NumCol -> [a] -> [[a]]
reshapeHelper r c xs | r <= 0 = []
                     | otherwise = (take c xs) : reshapeHelper  (r-1) c (drop c xs)


reshape :: NumRow -> NumCol -> [a] -> Maybe [[a]]
reshape r c xs  | 0 < r * c && r * c <= length xs = Just (reshapeHelper r c xs)
                | otherwise = Nothing


shuffleList :: [a] -> Seed -> [a]
shuffleList xs sd = shuffle' xs (length xs) (mkStdGen sd)


genSubsampleHelper :: [a] -> NumSamples -> SampleSize -> Seed -> Maybe [[a]]
genSubsampleHelper xs n s sd = f . g . h $ sd where   
    f = reshape n s
    g = take $ n*s
    h = shuffleList xs


genSubsample :: [a] -> NumSamples -> SampleSize -> Seed -> Maybe [[a]]
genSubsample xs n s sd  | 0 < n * s && n * s <= length xs = genSubsampleHelper xs n s sd
                        | otherwise = Nothing