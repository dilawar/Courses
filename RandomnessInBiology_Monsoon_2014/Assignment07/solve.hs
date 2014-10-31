import Data.Random.Normal
import System.Random
import qualified Data.Vector as V
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Foldable (toList)

alphas :: (Random a, Floating a) => IO [a]
alphas = do
    g <- newStdGen
    return $ normals g

-- A normal distribution. Draw a number from it.
--alpha :: (Random a, Floating a) => IO a
alpha :: IO Double
alpha = normalIO 

-- This is a weiner terml; evaluated at x with given dt.
weiner :: Double -> Double -> IO Double
weiner x dt = do 
    a <- alpha 
    return $  a * ((x * dt) ** 0.5)

-- This is xterm, evaluated at x and dt
xterm :: Double -> Double -> IO Double
xterm x dt = do 
    let num = (v0 + v1 * k1k2 * (x**2.0))
    let den = (1 + k1k2 * (x**2.0))
    let sub = gamma * x
    n <- weiner x dt 
    return $ x + (num / den - sub) * dt  + n
    where 
        [v0, v1, k1k2, gamma] = [12.0, 200, 1e-4, 1.0]

simulate:: Double -> Double -> Int -> V.Vector Double -> IO (V.Vector Double)
simulate x dt steps trajectory = do
    case steps of
        0 -> do 
            putStrLn $ "Done creating trajectory"
            return $! trajectory
        _ -> do x1 <- xterm x dt
                simulate x1 dt (steps-1) (V.snoc trajectory x1)

simulateNTimes :: Double -> Double -> Int -> Int -> [V.Vector Double] -> IO [V.Vector Double]
simulateNTimes initX dt steps n trajectories = do 
    case n of
        0 -> return $ reverse trajectories
        _ -> do trajectory <- simulate initX dt steps V.empty
                simulateNTimes initX dt steps (n-1) (trajectory:trajectories)

main = do
    let time = 10000
    let dt = 1e-2
    let steps = floor $ time / dt
    trajectories <- simulateNTimes 0.0 dt steps 2 []
    let dataToPlot = map (\l -> zip [1,2..steps] (toList l)) trajectories
    toFile def "trajectories.png" $ do
        layout_title .= "Trajectories"
        plot (line "trajectories" $  dataToPlot )
    putStrLn "Done"
