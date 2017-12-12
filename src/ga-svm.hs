module Main where

import GA (Entity(..), GAConfig(..), evolve)

-- Rule (chromosome) is a set of Attribute Intervals (from, to) and a set of Attributes Toggles
-- Toggle shows if attribute is used or not
type Toggle = Bool
type AttributeInterval = (Double, Double)
type Rule = ([AttributeInterval], [Toggle])

instance Entity [Rule] [Rule] Identity where

    genRandom entityPool randomSeed =
        0 -- return random entity

    crossover entityPool crossoverParameter randomSeed entity1 entity2 =
        entity1 -- return crossed entity

    mutation entityPool mutationParameter randomSeed entity =
        entity -- return mutated entity

    score' dataset entity =
        0 -- score entity (lower is better) sqrt(sensitivity + specificity)
        -- sensitivity TP / (TP + FN)
        -- specificity TN / (TN + FP)
        -- but also active

-- Utils
-- Get min-max

main = do
    let cfg = GAConfig
                200 -- population size
                20 -- archive size (best entities to keep track of)
                300 -- maximum number of generations
                0.2 -- crossover rate (% of entities by crossover) (from 0.2 to 0.6)
                0.2 -- mutation rate (% of entities by mutation) (from 0.01 to 0.1)
                0.2 -- parameter passed to crossover
                0.2 -- parameter passed to mutation
                False -- whether or not to use check-pointing
                False -- don't re-score archive in each generation

    -- Todo generator, entitiesPool, dataset - test-data without duplicates
    es <- evolveVerbose generator cfg entitiesPool dataset

