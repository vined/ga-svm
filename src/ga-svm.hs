{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import GHC.Float (float2Double)
import System.IO (IOMode(ReadMode), hGetContents, withFile)
import System.Environment (getArgs)
import Data.List (drop, foldl, filter, head, last, length, map, take, zip, zipWith)
import System.Random (StdGen(..), mkStdGen, randomR, randoms)
import GA (Entity(..), GAConfig(..), ScoredEntity(..), evolveVerbose)


-- Todo: try other crossover strategies
-- 1. Cross at random index
-- 2. Cross separately attributes and switches at random indexes


-- Data types --

data Score = TP | TN | FP | FN deriving (Eq, Show)

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


-- Common GA utils

getRandomFeature :: Int -> [Feature] -> (Int, Feature, StdGen)
getRandomFeature seed fts =
    let g = mkStdGen seed
        max_i = (length fts) - 1
        (idx, ng) = randomR (0, max_i) g
    in (idx, fts !! idx, ng)


-- Crossover utils

recombineFeature :: StdGen -> Feature -> Feature -> Feature
recombineFeature g ft1 ft2 =
    let (recombineAt, _) = randomR (0, 3) g :: (Int, StdGen)
    in case recombineAt of
        0 -> ft2
        1 -> Feature {range=(Range {from=(from (range ft1)), to=(to (range ft2))}), on=(on ft2)}
        2 -> Feature {range=(range ft1), on=(on ft2)}
        3 -> ft1


recombineRule :: Int -> Rule -> Rule -> Rule
recombineRule seed rule1 rule2 =
    let fts1 = (features rule1)
        fts2 = (features rule2)
        (idx, ft1, ng) = getRandomFeature seed fts1
        newFeature = recombineFeature ng (ft1) (fts2 !! idx)
        newFeatures = (take idx fts1) ++ [newFeature] ++ (drop (idx + 1) fts2)
    in Rule {cls=(cls rule1), features=newFeatures}


-- Mutation utils

getMaxStep :: Float -> Range -> Double
getMaxStep stepRate r = ((to r) - (from r)) * (float2Double stepRate)


getRandomStep :: Double -> StdGen -> Double
getRandomStep maxStepSize g = fst $ randomR (-maxStepSize, maxStepSize) g


mutateRange :: Range -> Double -> Bool -> Range
mutateRange r step isLower =
    if isLower
        then Range {from=((from r) + step), to=(to r)}
        else Range {from=(from r), to=((to r) + step)}


mutateFeatureValue :: Feature -> Bool -> Float -> Range -> StdGen -> Feature
mutateFeatureValue feature isLower stepRate initialRange g =
    let r = range feature
        maxStepSize = getMaxStep stepRate initialRange
        step = getRandomStep maxStepSize g
        newRange = mutateRange r step isLower
    in Feature {range=newRange, on=(on feature)}


toggleFeature :: Feature -> Feature
toggleFeature feature =
    Feature {range=(range feature), on=(not (on feature))}


mutateFeature :: Feature -> Float -> Range -> StdGen -> Feature
mutateFeature feature stepRate initialRange g =
    let (mutationType, ng) = randomR (0, 2) g :: (Int, System.Random.StdGen)
    in case mutationType of
        0 -> mutateFeatureValue feature True stepRate initialRange ng
        1 -> mutateFeatureValue feature False stepRate initialRange ng
        2 -> toggleFeature feature


mutateRule :: Rule -> Float -> [Range] -> Int -> Rule
mutateRule rule stepRate initialRanges seed =
    let fts = (features rule)
        (idx, feature, g) = getRandomFeature seed fts
        newFeature = mutateFeature (feature) stepRate (initialRanges !! idx) g
        newFeatures = (take idx fts) ++ [newFeature] ++ (drop (idx + 1) fts)
    in Rule {cls=(cls rule), features=newFeatures}


-- Scoring Utils --

divIntsToDbl :: Int -> Int -> Double
divIntsToDbl a b = (fromIntegral a) / (fromIntegral b)


isInRange :: Feature -> Double -> Bool
isInRange feature val =
    let r = (range feature)
        o = (on feature)
    in if o
        then val >= (from r) && val <= (to r)
        else True


evaluateFeatures :: [Feature] -> [Double] -> [Bool]
evaluateFeatures fts vals =
    zipWith isInRange fts vals


evaluateRule :: Rule -> SV -> Score
evaluateRule rule sv =
    let svClass = (clss sv)
        ruleClass = (cls rule)
        fts = (features rule)
        vals = (values sv)
        evals = evaluateFeatures fts vals
        allTrue = foldl (&&) True evals
    in
        if ruleClass == 1 && svClass == 1
            then if allTrue
                then TP
                else FN
            else if ruleClass == 0 && svClass == 0
                then if allTrue
                    then TN
                    else FP
                else if ruleClass == 1 && svClass == 0
                    then if allTrue
                        then FP
                        else TN
                    else if allTrue -- ruleClass == 0 && svClass == 1
                        then FN
                        else TP


countScore :: Score -> [Score] -> Int
countScore score scores = length $ filter (==score) scores


scoreEntity :: [SV] -> Rule -> Double
scoreEntity svs rule =
    let scores = map (\sv -> evaluateRule rule sv) svs
        tpCnt = countScore TP scores
        tnCnt = countScore TN scores
        fpCnt = countScore FP scores
        fnCnt = countScore FN scores
        allCnt = length scores
        sensitivity = divIntsToDbl tpCnt (tpCnt + fnCnt)
        specificity = divIntsToDbl tnCnt (tnCnt + fpCnt)
    in
        1 - (sqrt (sensitivity * specificity))


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
        return $ Just $ recombineRule seed entity1 entity2

    -- return mutated entity
    mutation initialRanges stepRate seed entity =
        return $ Just $ mutateRule entity stepRate initialRanges seed

    -- score entity (lower is better)
    score' dataSet entity =
        Just $ scoreEntity dataSet entity


main :: IO ()
main = do
    (dataSetFile:_) <- getArgs
    withFile dataSetFile ReadMode (\handle -> do

        dataSetContents <- hGetContents handle
        let svs = readDataSetFileLines $ lines dataSetContents
            initialRanges = getInitialRanges svs
            seed = 0
            g = mkStdGen seed
            cfg = GAConfig
                    200 -- population size
                    10 -- archive size (best entities to keep track of)
                    100 -- maximum number of generations (use 1000)
                    0.6 -- crossover rate (% of entities by crossover) (from 0.2 to 0.6)
                    0.1 -- mutation rate (% of entities by mutation) (from 0.01 to 0.1)
                    0.0 -- parameter passed to crossover (not used)
                    0.3-- parameter passed to mutation (attribute mutation max step size rate)
                    False -- whether or not to use check-pointing
                    True -- don't re-score archive in each generation

        -- Do the evolution
        es <- evolveVerbose g cfg initialRanges svs

        putStrLn "--Min max values --"
        sequence_ $ map putStrLn $ rangesToPrettyStrings initialRanges

        putStrLn "-- Elite --"
        sequence_ $ map putStrLn $ map (\(s, r) -> "Score: " ++ (show s) ++ ", " ++ (ruleToString r)) (take 10 es)

        putStrLn "-- First entity --"
        putStrLn ("Score: " ++ (show $ fst $ head es) ++ ", " ++ (ruleToString $ snd $ head es))
        putStrLn "-- Last entity --"
        putStrLn ("Score: " ++ (show $ fst $ last es) ++ ", " ++ (ruleToString $ snd $ last es))

        putStrLn "Done.")

