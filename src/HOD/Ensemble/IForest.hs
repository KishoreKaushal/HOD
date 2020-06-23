module HOD.Ensemble.IForest (getIForest) where

import Data.List.Unique (uniq)

import HOD.CSV.DataFrame

import HOD.Random

-- TODO : Complete this module
eps :: Fractional p => p
eps = 1e-5


data ITree  = EmptyNode 
            | InternalNode {
                right :: ITree,
                left :: ITree,
                splitAttr :: Int, 
                splitVal :: Double, 
                size :: Int
            } deriving (Show)


data IForest = IForest {
        numTrees :: NumSamples,
        subsamplingSize :: SampleSize, 
        df :: DataFrame,
        itrees :: Maybe [ITree]
    } deriving (Show)


getIForest :: Int -> Int -> DataFrame -> Seed -> IForest
getIForest n s x sd = IForest {
                            numTrees = n, 
                            subsamplingSize = s, 
                            df = x,
                            itrees = genITrees n s (dat x) sd
                        }


-- generate samples from : 
-- [MAT] --> List of subsamples 
getITree :: MAT -> ITree
getITree mat = let  numAttr = length $ head mat
                    p = getRandomInRange 0 (numAttr-1)       -- splitting attribute
                    (minVal, maxVal) = getMinMaxValOfAttr mat p
                    q = getRandomInRange (minVal + eps) (maxVal - eps) -- splitting value
                    sz = length mat
                    numUniqueVals = length $ getUniqueValOfAttr mat p
                    filterCondn v = q <= v 
                    (ltMat, rtMat) = splitMatUsingColVal filterCondn mat p
                in 
                    if sz > 1 && numUniqueVals > 1 
                    then InternalNode {
                                splitAttr = p,
                                splitVal = q,
                                size = sz,
                                right = getITree rtMat,
                                left = getITree ltMat
                            }
                    else EmptyNode


genITrees :: NumSamples -> SampleSize -> MAT -> Seed -> Maybe [ITree]
genITrees n s x sd | n * s <= 0 || length x <= n * s = Nothing
                   | otherwise = let maybeSubsamples = genSubsample x n s sd
                                 in case maybeSubsamples of 
                                        Just subsamples -> Just (map getITree subsamples)
                                        _ -> Nothing


pathLength :: Int -> [Double] -> ITree -> Double
pathLength _ _ EmptyNode = 0.0
pathLength 0 _ _ = 0.0
pathLength hlim inp itree = let p = splitAttr itree 
                                q = splitVal itree
                            in 
                                if (inp !! p) <= q 
                                then 1.0 + pathLength  (hlim-1) inp (left itree)
                                else 1.0 + pathLength (hlim-1) inp (right itree)


average :: [Double] -> Double
average l = sum l / fromIntegral (length l)


getAnomalyScoreHelper :: Int -> [ITree] -> [Double] -> [Double]
getAnomalyScoreHelper _ _ [] = []
getAnomalyScoreHelper hlim (t:lstItree) r = pathLength hlim r t : getAnomalyScoreHelper hlim lstItree r


getAnomalyScore :: MAT -> IForest -> Int -> Maybe [Double]
getAnomalyScore mat iforest hlim = case itrees iforest of 
                                    Just lstItree -> Just scores 
                                                    where lstPathLength = getAnomalyScoreHelper hlim lstItree <$> mat
                                                          scores = average <$> lstPathLength
                                    _ -> Nothing


sayHello :: IO ()
sayHello = do
    putStrLn "hello from IForest"
