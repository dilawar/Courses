-- This program solves assignment given in lecture 06. 
-- All units are in S.I.

type Temp = Float
type Energy = Float

-- FractionalState is the fraction of molecule in a state compared to grount
-- state.
type FractionalState = Float


-- Energy is given in Kcal / mole. 
state :: Energy -> Temp -> FractionalState
state energy temp =  exp (- energy  / ( 1.987e-3 * temp )) 

-- Now for a given energy level, compute the values of state at different
-- temperature between 2 - 5000 K., step size of 2K

states energy = map (\t -> (t,  state energy t)) temps
    where temps = [2,4..5000]

zipColumns ys = helper y0 (tail ys) where 
    helper res [] = res
    helper res (e:es) = helper (result res e) es
    result :: [[a]] -> [a] -> [[a]]
    result [] [] = []
    result (r:rs) (t:ts) = (r++[t]) : result rs ts
    y0 = map (\x -> [x]) (head ys)

-- plot xdata and ydata on a chart
plotFile :: String -> [Float] -> [[Float]] -> IO ()
plotFile filename x (y:ys) = do  
    putStrLn $ "Writing data to file " ++ filename
    let dataLines = toDataLines x ys
    
