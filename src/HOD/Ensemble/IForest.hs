module HOD.Ensemble.IForest (getIForest) where

import Data.List.Unique (uniq)

import HOD.CSV.DataFrame (DataFrame)

data ITree  = None 
            | Node {
                right :: ITree,
                left :: ITree,
                splitAttr :: Int, 
                splitVal :: Double, 
                size :: Int
            } deriving (Show)


data IForest = IForest {
        numTrees :: Int,
        subsamplingSize :: Int, 
        itrees :: [ITree],
        df :: DataFrame
    } deriving (Show)


getIForest :: Int -> Int -> DataFrame -> IForest
getIForest n s x = IForest {
                            numTrees = n, 
                            subsamplingSize = s, 
                            itrees = [], 
                            df = x
                        }




sayHello :: IO ()
sayHello = do
    putStrLn "hello from IForest"
