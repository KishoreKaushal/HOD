import HOD.CSV.DataFrame

main :: IO ()
main = do 
    df <- fromCSV "/home/kaushal/Documents/git/haskell-projects/HOD/data/pima.csv"
    putStrLn . show $ df
