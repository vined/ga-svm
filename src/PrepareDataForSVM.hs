module Main where

import System.IO
import System.Environment
import Data.List


main = do
    (fileName:columns) <- getArgs
    if length columns == 0
        then do
            putStrLn "Usage: file-name label-column-number new-columns-order..."
            putStrLn "Note: indexes start from 0"
            putStrLn "Example: prepareDataForSVM some-file.data 0 3 1 2"
        else do
            input <- openFile fileName ReadMode
            output <- openFile "out.data" WriteMode
            reorderColumns input output (read (head columns) :: Int) (map read (tail columns) :: [Int])
            hClose input
            hClose output


reorderColumns :: Handle -> Handle -> Int -> [Int] -> IO ()
reorderColumns input output labelColIdx columnsOrder =
    do
        isEOF <- hIsEOF input
        if isEOF
            then return ()
            else do
                line <- hGetLine input
                hPutStrLn output (getLabelWithColumns labelColIdx columnsOrder (splitOn ',' line))
                reorderColumns input output labelColIdx columnsOrder


getLabelWithColumns :: Int -> [Int] -> [String] -> String
getLabelWithColumns labelColIdx columnsOrder columns =
    (columns !! labelColIdx) ++ " " ++ (getColumnsInOrder columnsOrder columns)


getColumnsInOrder :: [Int] -> [String] -> String
getColumnsInOrder columnsOrder columns =
    concat [ (show idx) ++ ":" ++ (columns !! colIdx) ++ " " | (idx, colIdx) <- zip [1..(length columns)] columnsOrder ]


splitOn :: Char -> String -> [String]
splitOn delim str =
    if length str < 1
        then []
        else (\(a, b) -> if length b < 1 then [str] else [a] ++ (splitOn delim (tail b))) (break (== delim) str)
