module HOD.CSV.DataFrame (
    fromCSV, 
    DataFrame,
    DataFrame (..),
    MAT
    ) where

import Text.CSV (parseCSVFromFile, CSV, Record, Field)

import Text.Parsec.Error (ParseError)

import Numeric.Limits

import Data.List.Unique (uniq)

type MAT = [[Double]]


data DataFrame  = EmptyDataFrame
                | DataFrame {
                    header :: Record,
                    colIdx :: [Int],
                    rowIdx :: [Int],
                    dat :: MAT
                } deriving(Show)


-- return the min-max value of an attribute
getMinMaxValOfAttr :: MAT -> Int -> (Double, Double)
getMinMaxValOfAttr [] _ = (minValue, maxValue)
getMinMaxValOfAttr (row:mat) attrIdx = let  (min, max) = getMinMaxValOfAttr mat 
                                            currentVal = row !! attrIdx
                                            updMin = if currentVal < min then currentVal else min
                                            updMax = if currentVal > max then currentVal else max
                                          in
                                            (updMin, updMax)


-- get a column of the MAT
getCol :: MAT -> Int -> [Double]
getCol [] _ = []
getCol (row:mat) colIdx = (row !! colIdx) : getCol mat colIdx


-- get a row of the MAT
getRow :: MAT -> Int -> [Double]
getRow mat rowIdx = mat !! rowIdx


-- return the number of unique values of an attribute
getUniqueValOfAttr :: MAT -> Int -> Int 
getUniqueValOfAttr mat attrIdx = uniq . getCol mat attrIdx


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
