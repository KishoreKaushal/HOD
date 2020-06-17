module HOD.Ensemble.IForest (getIForest) where

import Data.List.Unique (uniq)

data ITree  = None 
            | Node {
                right :: ITree,
                left :: ITree,
                splitAttr :: Int, 
                splitVal :: Double, 
                size :: Int
            } deriving (Eq, Show)

data IForest = IForest {
        numTrees :: Int,
        subsamplingSize :: Int, 
        itrees :: [ITree],
        df :: [[Double]]
    } deriving (Eq, Show)


getIForest :: Int -> Int -> [[Double]] -> IForest
getIForest n s x = IForest {
                            numTrees = n, 
                            subsamplingSize = s, 
                            itrees = [], 
                            df = x
                        }


sayHello :: IO ()
sayHello = do
    putStrLn "hello from IForest"
