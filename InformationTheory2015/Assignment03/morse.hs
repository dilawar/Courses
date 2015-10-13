-- A problem based on morse code.

import Data.Graph.Inductive 
import Control.Monad.Random

data Symbol = DT | DS | LS | WS deriving (Show,Ord,Eq)
type Prob = Double

symbols = [ DT, DS, LS, WS ]

node_indices :: [Int]
node_indices = [0..length symbols - 1]

transition_probs :: [[Prob]]
transition_probs =  [ 
        [ 1/4, 1/4, 1/4, 1/4], [1/4, 1/4, 1/4, 1/4]
        , [1/2, 1/2, 0, 0], [1/2, 1/2, 0, 0]
    ]

transitions = [ (x, y, transition_probs!!x!!y) 
        | x <- node_indices, y <- node_indices 
    ]

-- Gr (Node data type) (Edge data type)
state_graph :: Gr Symbol Prob
state_graph = mkGraph (zip node_indices symbols) transitions

-- Get next_state from graph
next_state :: MonadRandom m => Node -> m Int
next_state state = do 
    let weighted_list = map (\(a,b,p) -> (b, toRational  p)) $ out state_graph state
    next <- fromList weighted_list 
    return next

rest_sequence xs 0 = return $ map (\x -> symbols!!x) xs
rest_sequence xs n = do 
    next <- next_state (head xs)
    rest <- rest_sequence (next:xs) (n-1)
    return rest

-- Generate a sequence of n character
gen_sequence n = do 
    init_state <- uniform $ nodes state_graph
    let s1 = symbols!!init_state
    ns <- rest_sequence [init_state] n
    return ns

main = do
    s <- gen_sequence 1000
    print s
    putStrLn "All done"
