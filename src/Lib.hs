module Lib
    ( quantumRun
    ) where

import Numeric.LinearAlgebra

quantumRun :: IO ()
quantumRun = do putStrLn "Some Func"
                let vect = fromList [2, 1]
                putStrLn $ show $ notGate (Qubit vect)

data Qubit a = Qubit (Vector a)

-- Show the Qubit using Ket notation
instance (Show a, Numeric a) => Show (Qubit a) where
    show (Qubit x) = '(':(show first) ++ ")|0> + (" ++ (show second) ++ ")|1>"
                    where first = (fromList [1, 0]) <.> x
                          second = fromList [0, 1] <.> x

notGate :: Qubit R -> Qubit R
notGate (Qubit x) = let invertMatrix = (2><2) [-1, 0, 0, -1] in
                        Qubit (invertMatrix #> x)
