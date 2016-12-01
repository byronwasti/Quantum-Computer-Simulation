module Lib
    ( quantumRun
    ) where

import Numeric.LinearAlgebra

quantumRun :: IO ()
quantumRun = do putStrLn "Some Func"
                putStrLn $ show $ notGate (Qubit 2 3)

data Qubit a = Qubit a a deriving Show

notGate :: (Fractional a) => Qubit a -> Qubit a
notGate (Qubit x y) = Qubit (-x) (-y)
