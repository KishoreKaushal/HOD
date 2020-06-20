module HOD.Ensemble.IForest (getIForest) where

import Data.List.Unique (uniq)

import HOD.CSV.DataFrame (DataFrame, DataFrame(..), MAT)

import HOD.Random


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
getITree mat = EmptyNode


genITrees :: NumSamples -> SampleSize -> MAT -> Seed -> Maybe [ITree]
genITrees n s x sd | n * s <= 0 || (length x) <= n * s = Nothing
                   | otherwise = let maybeSubsamples = genSubsample x n s sd
                                 in case maybeSubsamples of 
                                        Just subsamples -> Just (map getITree subsamples)
                                        _ -> Nothing
                            

sayHello :: IO ()
sayHello = do
    putStrLn "hello from IForest"
