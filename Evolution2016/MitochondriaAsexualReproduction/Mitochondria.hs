module Mitochondria where 

import System.Random
import Control.Monad  ( foldM, filterM )
{-import qualified Data.Sequence  as S-}
import Text.Printf ( printf )

data MutationType = Beneficial | Harmful deriving (Show, Eq)

{-
 
 A mitochondria length is 16569 base pairs and mutation rates are observerd to
 be 30 ppm per 20 years (generation age). See reference
 http://www.sciencedirect.com/science/article/pii/S0005272898001613.
 
 This make the multation rate to be 0.5 mutation in mtDNA per generation. On
 average every second generation one base pair is mutated in mtDNA. It has been
 suggested that most mutations are of Harmful type.

 In any case, it is safe to assume that every generation there is one mutation
 which which could be of Beneficial type (with low probability p). Assuming that
 Harmful mutations are 10 times likely to occur than Beneficial type mutations.

-}

data Mitochondria = M { mid :: Int, age :: Int, fitness :: Float } 

instance Show Mitochondria where 
    show (M id age f) = printf "M%d age=%d fit=%.7f" id age f 

-- Mutation occurs during asexual reproduction. New cell gets the 0 age and same
-- id as of parent
mutate :: Mitochondria -> IO Mitochondria 
mutate (M id age f) = do 
    r <- randomIO :: IO Float
    df <- randomRIO (1e-9, 1e-5) 
    -- Since we are assuming that 90% mutations are of Harmful type.
    return $ case (r > 0.9) of 
                True -> M id 0 (f+df)  -- Beneficial
                False -> M id 0 (f-df) -- Harmful
        

data Population = P { mts :: [ Mitochondria ]  }

instance Show Population where 
    {-show (P ms) = unlines $ map show ms-}
    show (P ms) = printf "Population size %d" (length ms)


population :: Int -> Population 
population n = P $ map (\i -> M i 0 1.0) [0,1..n-1]

size (P mts) = length mts

mutatePopulation :: Population -> IO Population 
mutatePopulation p = do 
    mutated <- (mapM mutate $ mts p ) 
    return $ P mutated

-- birth
birth :: Population -> IO Population 
birth (P mts) = do 
    mtss <- mapM divide mts
    return $ P (concat mtss)

-- | The new cell produced may have mutation, and the parent cell remains
-- doesn't change at all.
divide :: Mitochondria -> IO [ Mitochondria ]
divide m = mutate m >>= \x -> return [M (mid m) (1+age m) (fitness m), x]


-- death 
death :: Population -> IO Population 
death p = do
    let newMts = filter canLiveLonger (mts p) 
    return $ P newMts

canLiveLonger (M id age fit) 
    | age >= 2 = False
    | fit < 0.95 = False
    | otherwise = True

-- | In each cycle, each individual reproduce with some mutations. And if their
-- have aged, they die
circleOfLife :: Population -> IO Population 
circleOfLife p = birth p >>= death 

-- Mutate a given population N times.
circleOfLife' pop 0 = return [ ]
circleOfLife' pop n = do
    v <- circleOfLife pop 
    vs <- circleOfLife' v (n-1)
    return $ (size v):vs


main = do 
    let p = population 10
    circleOfLife' p 30 >>= print
    putStrLn "All done"

