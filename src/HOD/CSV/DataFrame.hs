module HOD.CSV.DataFrame (fromCSV, DataFrame) where

import Text.CSV (parseCSVFromFile, CSV, Record, Field)
import Text.Parsec.Error (ParseError)

type MAT = [[Double]]


data DataFrame  = EmptyDataFrame
                | DataFrame {
                    header :: Record,
                    colIdx :: [Int],
                    rowIdx :: [Int],
                    dat :: MAT
                } deriving(Show)


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)


fieldToInt :: Field -> Double
fieldToInt = read


getMATFromCSV :: CSV -> MAT
getMATFromCSV [[""]] = []
getMATFromCSV (r:csv) = (fieldToInt <$> r) : (getMATFromCSV csv)


createDataFrame :: Record -> CSV -> DataFrame
createDataFrame hd csv = DataFrame {
        header = hd,
        colIdx = [1..(length hd)],
        rowIdx = [1..(length csv)],
        dat = getMATFromCSV csv
    }


-- type CSV = [[String]]
-- first row is header, rest of the rows is data
getDataFrameFromCSV :: CSV -> DataFrame
getDataFrameFromCSV csv = let maybehd = safeHead csv in 
    case maybehd of 
        Just hd -> createDataFrame hd (tail csv)
        Nothing -> EmptyDataFrame


fromCSV :: FilePath -> IO (DataFrame)
fromCSV fp = do 
    parsedCSV <- parseCSVFromFile fp
    case parsedCSV of 
        Left err -> error "Error in parsing."
        Right csv -> return $ getDataFrameFromCSV csv
