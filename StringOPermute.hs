-- String-O-Permute problem, permute characters two-in-two
-- Solved in Wed 12 Jul 2017 04:52:03 AM -03
-- Manoel Vilela

import Control.Monad (replicateM)

stringPermute :: String -> String
stringPermute [] = []
stringPermute (s:[]) = s:[]
stringPermute (s:r:ss) = r:s:stringPermute ss

main :: IO ()
main = do n <- getLine
          inputs <- replicateM (read n :: Int) getLine
          mapM_ (putStrLn) $ map stringPermute inputs
