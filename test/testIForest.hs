import HOD.Random
import HOD.CSV.DataFrame
import HOD.Ensemble.IForest
import Control.Monad

numSamples = 12 :: Int 
sampleSize = 64 :: Int
seed = 0 :: Int 
hlim = 4 :: Int 

main :: IO ()
main = do 
    df <- fromCSV "/home/travis/build/KishoreKaushal/HOD/test/pima.csv"
    let iforest =  getIForest numSamples sampleSize seed df
    print $ getAnomalyScore iforest hlim (dat df)

