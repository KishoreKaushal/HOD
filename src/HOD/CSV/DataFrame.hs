module HOD.CSV.DataFrame where

import Text.CSV (parseCSVFromFile, CSV)
import Text.Parsec.Error (ParseError)

data DataFrame  = EmptyDataFrame
                | DataFrame {
                    header :: [String],
                    colIdx :: [Int],
                    rowIdx :: [Int],
                    dat :: [[Double]]
                } deriving(Show)


getDataFrameFromCSV :: CSV -> DataFrame
getDataFrameFromCSV csv' = EmptyDataFrame


fromCSV :: FilePath -> IO (DataFrame)
fromCSV fp = do 
    parsedCSV <- parseCSVFromFile fp
    case parsedCSV of 
        Left err -> error "Error in parsing."
        Right csv' -> return $ getDataFrameFromCSV csv'
