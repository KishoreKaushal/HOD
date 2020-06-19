import HOD.Random
import HOD.CSV.DataFrame

numSamples :: Int
numSamples = 5

sampleSize :: Int 
sampleSize = 10

seed :: Int 
seed = 0

main :: IO ()
main = do 
    df <- fromCSV "/home/kaushal/Documents/git/haskell-projects/HOD/data/pima.csv"
    putStrLn . show $ genSubsample (dat df) numSamples sampleSize seed
