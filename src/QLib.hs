module QLib
    ( quantumRun,
      Qubit(..),
    ) where

import Data.Complex
import Data.List
import Text.Printf
import Numeric.LinearAlgebra

quantumRun :: IO ()
quantumRun = do putStrLn "Some Func"
                putStrLn $ printf "%b" (3 :: Int)
                let c = Qubit $ fromList [1 :+ 0, 0]
                putStrLn $ show c
                let d = sauliX c
                putStrLn $ show d
                let e = Register $ take 5 $ repeat (Qubit $ fromList [1, 0])
                --let e = sauliZ d
                putStrLn $ show e

data Qubit = Qubit (Vector C)
data Register = Register [Qubit]

instance Show Qubit where
    show (Qubit vect) = show vect

instance Show Register where
    show (Register xs) = concatMap show xs

sauliX :: Qubit -> Qubit
sauliX (Qubit x) = Qubit (xgate #> x)
                where xgate = (2><2)[0, 1, 1, 0] :: Matrix C

sauliY :: Qubit -> Qubit
sauliY (Qubit x) = Qubit (ygate #> x)
                where ygate = (2><2)[0:+1, 0, 0, 0:+1] :: Matrix C

sauliZ :: Qubit -> Qubit
sauliZ (Qubit x) = Qubit (zgate #> x)
                where zgate = (2><2)[1, 0, 0, -1] :: Matrix C
                

--data Bit = Zero | One deriving Eq
--
--instance Show Bit where
--    show Zero = "|0>"
--    show One  = "|1>"
--
--data Qubit a = Qubit (Complex a) (Complex a) deriving Show
--
--sauliX :: Qubit a -> Qubit a
--sauliX (Qubit x y) = Qubit y x
--
--sauliY :: (Fractional a, RealFloat a) => Qubit a -> Qubit a
--sauliY (Qubit x y) = Qubit rotY rotX
--        where i = 0 :+ 1
--              rotY = -y*i
--              rotX = x*i
--
--sauliZ :: (RealFloat a) => Qubit a -> Qubit a
--sauliZ (Qubit x y) = Qubit x (-y)



--data Qubit a = Qubit (Vector a)
--
---- Show the Qubit using Ket notation
--instance (Show a, Numeric a) => Show (Qubit a) where
--    show (Qubit x) = '(':(show first) ++ ")|0> + (" ++ (show second) ++ ")|1>"
--                    where first = (fromList [1, 0]) <.> x
--                          second = fromList [0, 1] <.> x
--
--notGate :: Qubit R -> Qubit R
--notGate (Qubit x) = let invertMatrix = (2><2) [-1, 0, 0, -1] in
--                        Qubit (invertMatrix #> x)
