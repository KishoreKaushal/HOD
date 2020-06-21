import HOD.CSV.DataFrame

main :: IO ()
main = do 
    df <- fromCSV "/home/travis/build/KishoreKaushal/HOD/test/pima.csv"
    putStrLn . show $ df
