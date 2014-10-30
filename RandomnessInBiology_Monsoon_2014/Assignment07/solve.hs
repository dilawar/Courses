import Data.Random.Normal
import System.Random
import Graphics.EasyPlot

alphas :: (Random a, Floating a) => IO [a]
alphas = do
    g <- newStdGen
    return $ normals g

-- A normal distribution. Draw a number from it.
--alpha :: (Random a, Floating a) => IO a
alpha :: IO Float
alpha = normalIO 

-- This is a weiner terml; evaluated at x with given dt.
weiner :: Float -> Float -> IO Float
weiner x dt = do 
    a <- alpha 
    return $ a * x * (dt ** 0.5)

-- This is xterm, evaluated at x and dt
xterm :: Float -> Float -> IO Float
xterm x dt = do 
    let num = (v0 + v1 * k1k2 * (x**2.0))
    let den = (1 + k1k2 * (x**2.0))
    let sub = gamma * x
    n <- weiner x dt 
    return $ x + (num / den - sub) * dt  + n
    where 
        [v0, v1, k1k2, gamma] = [12.0, 200, 1e-4, 1]

simulate:: Float -> Float -> Int -> [Float] -> IO [Float]
simulate x dt 0 trajectory = do
    return trajectory 
simulate x dt steps trajectory = do
    x1 <- xterm x dt
    simulate x1 dt (steps - 1) (x1:trajectory)

main = do
    let steps = 1000
    let dt = 1e-3
    trajectory <- simulate (0.0::Float) dt steps []
    let dataToPlot = zip [1,2..steps] trajectory
    putStrLn "Done"
