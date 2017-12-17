{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import GHC.Float (float2Double)
import System.IO (IOMode(ReadMode), hGetContents, withFile)
import System.Environment (getArgs)
import Data.List (drop, foldl, head, last, length, map, take, zip)
import System.Random (StdGen(..), mkStdGen, randomR, randoms)
import GA (Entity(..), GAConfig(..), ScoredEntity(..), evolveVerbose)


-- Todo: try other crossover strategies
-- 1. Cross at random index
-- 2. Cross separately attributes and switches at random indexes


-- Data types --

type Cls = Int
data SV = SV {clss :: Cls, values:: [Double]} deriving (Show)

-- Rule (chromosome) is a set of Attribute Intervals (from, to)
-- and 'on' parameter which shows if attribute is used
data Range = Range {from :: Double, to :: Double} deriving (Eq, Ord, Read, Show)
data Feature = Feature {range :: Range, on :: Bool} deriving (Eq, Ord, Read, Show)
data Rule = Rule {cls :: Cls, features :: [Feature]} deriving (Eq, Ord, Read, Show)


-- Reading data utils --

splitOn :: Char -> String -> [String]
splitOn delim str =
    if length str < 1
        then []
        else (\(a, b) -> if length b < 1 then [str] else [a] ++ (splitOn delim (tail b))) (break (== delim) str)


parseClassValue :: String -> Int
parseClassValue str = if 0 < (read str :: Double) then 1 else 0


parseFeatureValue :: String -> Double
parseFeatureValue str = read . last $ splitOn ':' str


readDataSetLine :: String -> SV
readDataSetLine line =
    let splits = splitOn ' ' line
        c = parseClassValue $ head splits
        vals = map parseFeatureValue $ tail splits
    in SV {clss=c, values=vals}


readDataSetFileLines :: [String] -> [SV]
readDataSetFileLines strs = map readDataSetLine strs


-- Getting initial ranges pool with min-max values for all features

getMinValues :: [Double] -> [SV] -> [Double]
getMinValues initial svs =
    foldl (\acc next -> zipWith min acc $ values next) initial svs


getMaxValues :: [Double] -> [SV] -> [Double]
getMaxValues initial svs =
    foldl (\acc next -> zipWith max acc $ values next) initial svs


getMinMaxValues :: [SV] -> [(Double, Double)]
getMinMaxValues svs =
    let firstValues = values $ head svs
        minValues = getMinValues firstValues svs
        maxValues = getMaxValues firstValues svs
    in zip minValues maxValues


getInitialRanges :: [SV] -> [Range]
getInitialRanges svs =
    [Range {from=mn, to=mx} | (mn, mx) <- (getMinMaxValues svs)]


-- Printing utils --

rangeToString :: Range -> String
rangeToString r = (show (from r)) ++ ":" ++ (show (to r))


rangesToPrettyStrings :: [Range] -> [String]
rangesToPrettyStrings ranges =
    [(show idx) ++ " " ++ (rangeToString r) | (idx, r) <- (zip [1..(length ranges)] ranges)]


featuresToPrettyStrings :: [Feature] -> [String]
featuresToPrettyStrings fts =
    let ranges = map range fts
        rangesStrs = rangesToPrettyStrings ranges
        ons = map on fts
    in [r ++ ", On: " ++ (show o) | (r, o) <- (zip rangesStrs ons)]


ruleToString :: Rule -> String
ruleToString rule =
    let c = "Class: " ++ show (cls rule)
        fts = featuresToPrettyStrings (features rule)
    in unlines (c : "Features:" : fts)


-- Rules utils

createFeaturesFromRanges :: [Range] -> [Bool] -> [Feature]
createFeaturesFromRanges ranges featureToggles =
    [Feature {range=r, on=o} | (r, o) <- zip ranges featureToggles]


createRuleFromRanges :: Cls -> [Range] -> [Bool] -> Rule
createRuleFromRanges c ranges featureToggles =
    Rule {cls=c, features=createFeaturesFromRanges ranges featureToggles}


-- Mutation utils

getMaxStep :: Float -> Range -> Double
getMaxStep stepRate r = ((to r) - (from r)) * (float2Double stepRate)


getRandomStep :: Double -> Int -> Double
getRandomStep maxStepSize seed =
    let g = mkStdGen seed
    in fst $ randomR (-maxStepSize, maxStepSize) g


mutateRange :: Range -> Double -> Bool -> Range
mutateRange r step isLower =
    if isLower
        then Range {from=((from r) + step), to=(to r)}
        else Range {from=(from r), to=((to r) + step)}


mutateFeatureValue :: Feature -> Bool -> Float -> Range -> Int -> Feature
mutateFeatureValue feature isLower stepRate initialRange seed =
    let r = range feature
        maxStepSize = getMaxStep stepRate initialRange
        step = getRandomStep maxStepSize seed
        newRange = mutateRange r step isLower
    in Feature {range=newRange, on=(on feature)}


toggleFeature :: Feature -> Feature
toggleFeature feature =
    Feature {range=(range feature), on=(not (on feature))}


mutateFeature :: Feature -> Float -> Range -> Int -> Feature
mutateFeature feature stepRate initialRange seed =
    let g = mkStdGen seed
        (mutationType, _) = randomR (0, 2) g :: (Int, System.Random.StdGen)
    in case mutationType of
        0 -> mutateFeatureValue feature True stepRate initialRange seed
        1 -> mutateFeatureValue feature False stepRate initialRange seed
        2 -> toggleFeature feature


mutateRule :: Rule -> Float -> [Range] -> Int -> Rule
mutateRule rule stepRate initialRanges seed =
    let g = mkStdGen seed
        fts = (features rule)
        max_i = (length fts) - 1
        (idx, _) = randomR (0, max_i) g
        newFeature = mutateFeature (fts !! idx) stepRate (initialRanges !! idx) seed
        newFeatures = (take idx fts) ++ [newFeature] ++ (drop (idx + 1) fts)
    in Rule {cls=(cls rule), features=newFeatures}


-- GA --

-- Entity entityType populationType dataSetType poolType monad
instance Entity Rule Double [SV] [Range] IO where

    -- return random entity for initial population
    genRandom initialRanges seed = return $
        let g = mkStdGen seed
            (c, _) = randomR (0, 1) g
            ons = randoms g
        in createRuleFromRanges c initialRanges ons

    -- return crossed entity
    crossover _ _ seed entity1 entity2 =
        return $ Just  entity1

    -- return mutated entity
    mutation initialRanges stepRate seed entity =
        return $ Just $ mutateRule entity stepRate initialRanges seed

    -- score entity (lower is better) sqrt(sensitivity + specificity)
    -- sensitivity TP / (TP + FN)
    -- specificity TN / (TN + FP)
    -- but also active
    score' dataSet entity =
        Just 0


main :: IO ()
main = do
    (dataSetFile:_) <- getArgs
    withFile dataSetFile ReadMode (\handle -> do

        dataSetContents <- hGetContents handle
        let svs = readDataSetFileLines $ lines dataSetContents
            initialRanges = getInitialRanges svs
            rangesForPrinting = rangesToPrettyStrings initialRanges
            seed = 0
            g = mkStdGen seed
            cfg = GAConfig
                    200 -- population size
                    20 -- archive size (best entities to keep track of)
                    200 -- maximum number of generations (use 1000)
                    0.2 -- crossover rate (% of entities by crossover) (from 0.2 to 0.6)
                    0.05 -- mutation rate (% of entities by mutation) (from 0.01 to 0.1)
                    0.0 -- parameter passed to crossover (not used)
                    0.001 -- parameter passed to mutation (attribute mutation max step size coef)
                    False -- whether or not to use check-pointing
                    True -- don't re-score archive in each generation

        putStrLn "Min max values"
        sequence_ $ map putStrLn rangesForPrinting

        putStrLn $ show $ getMaxStep 0.001 $ head initialRanges

        -- Do the evolution
        es <- evolveVerbose g cfg initialRanges svs
        putStrLn $ "Best entity: " ++ (ruleToString $ snd $ head es)
        putStrLn "-- All entities --"
        sequence_ $ map putStrLn $ map (\(s, r) -> "Score: " ++ (show s) ++ ", " ++ (ruleToString r)) (take 10 es)

        putStrLn "Done.")

