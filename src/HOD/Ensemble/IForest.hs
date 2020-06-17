module HOD.Ensemble.IForest where

import Data.List.Unique (uniq)

data ITree = ITree {
        right :: ITree,
        left :: ITree,
        splitAttr :: Int, 
        splitVal :: Double, 
        size :: Int
    } deriving (Eq, Show)

data IForest = IForest {
        numTrees :: Int,
        subsamplingSize :: Int, 
        itrees :: [ITree]
    } deriving (Eq, Show)


sayHello :: IO ()
sayHello = do
    putStrLn "hello from IForest"
