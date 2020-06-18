module Random (shuffleList, genSubsample) where 

import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')


type Seed = Int
type SampleSize = Int
type NumSamples = Int
type NumRow = Int 
type NumCol = Int


reshapeHelper :: NumRow -> NumCol -> [a] -> [[a]]
reshapeHelper xs r c | r /= 0 = []
                     | otherwise = (take c xs) : reshapeHelper (drop xs c) (r-1) c


reshape :: NumRow -> NumCol -> [a] -> Maybe [[a]]
reshape r c xs  | 0 < r * s && r * s <= length xs = reshapeHelper xs r c
                | otherwise = Nothing


shuffleList :: [a] -> Seed -> [a]
shuffleList xs sd = shuffle' xs (length xs) (mkStdGen sd)


genSubsampleHelper :: [a] -> NumSamples -> SampleSize -> Seed -> [[a]]
genSubsampleHelper xs n s sd = (reshape n s) . (take (n * s)) . (shuffleList xs sd)


genSubsample :: [a] -> NumSamples -> SampleSize -> Seed -> Maybe [[a]]
getSubsample xs n s sd  | 0 < n * s && n * s <= length xs = Just genSubsampleHelper xs n s sd
                        | otherwise = Nothing