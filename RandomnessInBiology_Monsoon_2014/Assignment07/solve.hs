import Data.Random.Normal
import System.Random
import Graphics.Gnuplot.Simple
import Data.Foldable (toList)

-- Let's put the globals here 
data Global = Global {
    stepSize :: Double
    , simTime :: Double
    , totalSteps :: Integer
} deriving Show 

global :: Global 
global = Global 0.01 10000.0 (floor $ simTime global/stepSize global)

-- A normal distribution. Draw a number from it by repeatedly calling it.
alpha :: IO Double
alpha = normalIO 

-- This is a weiner term; evaluated at x with given dt.
weiner :: Double -> Double -> IO Double
weiner x dt = do 
    alpha >>= \a -> return $  a * ((x * dt) ** 0.5)

-- This is xterm, evaluated at x with step size dt
xterm :: Double -> Double -> IO Double
xterm x dt = do 
    let [v0, v1, k1k2, gamma] = [12.0, 200, 1e-4, 1.0]
    let [num,den,sub] = [(v0 + v1 * k1k2 * (x**2.0)),(1 + k1k2 * (x**2.0)),gamma*x]
    weiner x dt >>= \n -> return $ x + (num / den - sub) * dt  + n

simulate:: Double -> Integer -> [Double] -> IO [Double]
simulate x steps trajectory = do
    let dt = stepSize global
    case steps of
        0 -> return $ reverse trajectory
        _ -> xterm x dt >>= \x1 -> simulate x1 (steps-1) (x1:trajectory)

simulateNTimes :: Double  -> Int -> [[Double]] -> IO [[Double]]
simulateNTimes initX n trajectories = do 
    let steps = totalSteps global
    case n of
        0 -> return $ reverse trajectories
        _ -> do 
                trajectory <- simulate initX steps []
                putStrLn $ "Created one trajectory. Left: " ++ show (n-1)
                simulateNTimes initX (n-1) (trajectory:trajectories)
main = do
    let (time,dt,ntimes,steps) = (10000,1e-2, 3, floor $ time / dt)
    trajectories <- simulateNTimes 0.0 ntimes []
