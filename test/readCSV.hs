import HOD.CSV.DataFrame

main :: IO ()
main = do 
    df <- fromCSV "pima.csv"
    putStrLn . show $ df
