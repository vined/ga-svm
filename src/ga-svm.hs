module Main where

import System.IO
import System.Environment
import Data.List
import Control.Monad.Identity (Identity(..))
import GA (Entity(..), GAConfig(..), evolveVerbose)


-- Todo: try other crossover strategies
-- 1. Cross at random index
-- 2. Cross separately attributes and switches at random indexes


-- Data types --

type Clazz = Int
data SV = SV {clazz :: Clazz, values:: [Double]}

-- Rule (chromosome) is a set of Attribute Intervals (from, to)
-- and 'on' parameter which shows if attribute is used
data Range = Range {from :: Double, to :: Double} deriving (Show)
data Feature = Feature {range :: Range, on :: On} deriving (Show)
data Rule = Rule {clazz :: Clazz, feature :: [Feature]}


-- Reading data set --

splitOn :: Char -> String -> [String]
splitOn delim str =
    if length str < 1
        then []
        else (\(a, b) -> if length b < 1 then [str] else [a] ++ (splitOn delim (tail b))) (break (== delim) str)


parseClassValue :: String -> Int
parseClassValue str = if read str :: Double > 0 then 1 else 0


parseFeatureValue :: String -> Double
parseFeatureValue str = last . splitOn ':' str


readDataSetLine :: String -> SV
readDataSetLine line =
    let splits = splitOn ' ' line
        clazz = parseClassValue . head splits
        values = parseFeatureValue . tail splits
    in SV {clazz=clazz, values=values}


readDataSetFileLines :: [Stirng] -> [SV]
readDataSetFileLines strs =
    map readDataSetFileLines strs


-- Getting initial ranges pool with min-max values for all features

getMinValues :: [Double] -> [SV] -> [Double]
getMinValues initial svs =
    foldl (\acc next -> zipWith min acc $ values next) svs


getMaxValues :: [Double] -> [SV] -> [Double]
getMaxValues initial svs =
    foldl (\acc next -> zipWith max acc $ values next) svs


getMinMaxValues :: [SV] -> [(Double, Double)]
getMinMaxValues svs =
    let firstValues = values . head svs
        minValues = getMinValues svs
        maxValues = getMaxValues svs
    in zip minValues maxValues


getInitialRanges :: [SV] -> [Ranges]
getInitialRanges svs =
    [Range {from=mn, to=mx} | (mn, mx) <- (getMinMaxValues dataset)]


-- GA --

-- Entity entityType populationType dataSetType poolType monad
instance Entity Rule Double [make-datasettype] [Rule] Identity where

    -- return random entity for initial population
    genRandom initialRanges seed =
        0

    -- return crossed entity
    crossover _ _ seed entity1 entity2 =
        entity1

    -- return mutated entity
    mutation _ _ seed entity =
        entity

    -- score entity (lower is better) sqrt(sensitivity + specificity)
    -- sensitivity TP / (TP + FN)
    -- specificity TN / (TN + FP)
    -- but also active
    score' dataset entity =
        0


main = do
    (dataSetFile) <- getArgs
    dataSetContents <- readFile dataSetFile

    let dataset = readDataSetFileLines . lines dataSetContents
        initialRanges = getInitialRanges dataset
        cfg = GAConfig
                200 -- population size
                20 -- archive size (best entities to keep track of)
                200 -- maximum number of generations (use 1000)
                0.2 -- crossover rate (% of entities by crossover) (from 0.2 to 0.6)
                0.05 -- mutation rate (% of entities by mutation) (from 0.01 to 0.1)
                0.0 -- parameter passed to crossover (not used)
                0.0 -- parameter passed to mutation (not used)
                False -- whether or not to use check-pointing
                True -- don't re-score archive in each generation

    -- Todo generator
    es <- evolveVerbose generator cfg initialRanges dataset

