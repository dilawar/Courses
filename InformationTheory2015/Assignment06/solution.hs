import Data.List as L
{-import Numeric.Probability.Distribution as D-}
{-import Control.Monad (liftM2, replicateM)-}

data Symbol = NULL | One | Two | Three | Four | Five | Six 
    deriving (Eq,Ord,Show)

alphas :: [ Symbol ]
alphas = [ One, Two, Three, Four, Five, Six ]

choices x = L.map ( \y -> if y == x then x else NULL ) alphas
allchoices = map choices alphas

main = do
    putStrLn "Done"


