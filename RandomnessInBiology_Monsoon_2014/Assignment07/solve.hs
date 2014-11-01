import Data.Random.Normal
import System.Random
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Foldable (toList)

-- A normal distribution. Draw a number from it by repeatedly calling it.
alpha :: IO Double
alpha = normalIO 

-- This is a weiner terml; evaluated at x with given dt.
weiner :: Double -> Double -> IO Double
weiner x dt = do 
    alpha >>= \a -> return $  a * ((x * dt) ** 0.5)

-- This is xterm, evaluated at x and dt
xterm :: Double -> Double -> IO Double
xterm x dt = do 
    let [v0, v1, k1k2, gamma] = [12.0, 200, 1e-4, 1.0]
    let [num,den,sub] = [(v0 + v1 * k1k2 * (x**2.0)),(1 + k1k2 * (x**2.0)),gamma*x]
    weiner x dt >>= \n -> return $ x + (num / den - sub) * dt  + n

simulate:: Double -> Double -> Int -> [Double] -> IO [Double]
simulate x dt steps trajectory = do
    case steps of
        0 -> return $ reverse trajectory
        _ -> xterm x dt >>= \x1 -> simulate x1 dt (steps-1) (x1:trajectory)

simulateNTimes :: Double -> Double -> Int -> Int -> [[Double]] -> IO [[Double]]
simulateNTimes initX dt steps n trajectories = do 
    case n of
        0 -> return $ reverse trajectories
        _ -> do 
                trajectory <- simulate initX dt steps []
                putStrLn $ "Created one trajectory. Left: " ++ show (n-1)
                simulateNTimes initX dt steps (n-1) (trajectory:trajectories)

main = do
    let (time,dt,ntimes,steps) = (10000,1e-2,2,floor $ time / dt)
    trajectories <- simulateNTimes 0.0 dt steps ntimes []
    let dataToPlot = map (\l -> zip [1,2..steps] (toList l)) trajectories
    putStrLn $ "Plotting trajectories"
    -- Use cairo to plot the data.
    toFile def "trajectories.png" $ do
        layout_title .= "Trajectories"
        mapM_ (\(i,t) -> plot (line ("trajectory"++show i) [t])) (zip [1,2..ntimes] dataToPlot)
    putStrLn "Done plotting all"
