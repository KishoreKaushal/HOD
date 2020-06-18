module HOD.CSV.DataFrame (fromCSV) where

import Text.CSV (parseCSVFromFile, CSV)
import Text.Parsec.Error (ParseError)

data DataFrame  = EmptyDataFrame
                | DataFrame {
                    header :: [String],
                    colIdx :: [Int],
                    rowIdx :: [Int],
                    dat :: [[Double]]
                } deriving(Show)


type MAT = [[Double]]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

fromStringToInt :: String -> Double
fromStringToInt = read

getMATFromCSV :: CSV -> MAT
getMATFromCSV [] = []
getMATFromCSV r::csv = (fromStringToInt <$> r) : (getMATFromCSV csv)

-- type CSV = [[String]]
-- first row is header, rest of the rows is data
getDataFrameFromCSV :: CSV -> DataFrame
getDataFrameFromCSV csv = getMATFromCSV 

fromCSV :: FilePath -> IO (DataFrame)
fromCSV fp = do 
    parsedCSV <- parseCSVFromFile fp
    case parsedCSV of 
        Left err -> error "Error in parsing."
        Right csv -> return $ getDataFrameFromCSV csv
