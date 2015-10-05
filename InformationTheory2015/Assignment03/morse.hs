-- A problem based on morse code.

import Data.Graph.Inductive 
import Data.List

data Symbol = DOT | DASH | LSPACE | WSPACE deriving (Show,Ord,Eq)
type Prob = Double

symbols = [ DOT, DASH, LSPACE, WSPACE ]

node_indices :: [Int]
node_indices = [0..length symbols - 1]

transition_probs :: [[Prob]]
transition_probs =  [ 
        [ 1/4, 1/4, 1/4, 1/4], [1/4, 1/4, 1/4, 1/4]
        , [1/2, 1/2, 0, 0], [1/2, 1/2, 0, 0]
    ]

transitions = [ (x, y, transition_probs!!x!!y) | x <- node_indices, y <- node_indices ]

-- Gr (Node data type) (Edge data type)
state_graph :: Gr Symbol Prob
state_graph = mkGraph (zip node_indices symbols) transitions

main = do
    prettyPrint state_graph
    putStrLn "All done"
