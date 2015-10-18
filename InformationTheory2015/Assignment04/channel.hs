import Data.GraphViz
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types
import Data.Graph
import System.Environment
import qualified Data.Text as T

read_dot_file :: String -> IO (DotGraph String)
read_dot_file filename = readDotFile filename

main = do
    files <- getArgs
    dots <- mapM read_dot_file files
    print $ dots
    putStrLn "Done"
    
