module QLib
    ( quantumRun,
      Qubit(..)
    ) where

import Data.Complex

quantumRun :: IO ()
quantumRun = do putStrLn "Some Func"
                let c = Qubit (1 :+ 0) (0 :+ 0)
                putStrLn $ show c
                let d = sauliY c
                putStrLn $ show d
                let e = sauliZ d
                putStrLn $ show e

                

data Bit = Zero | One deriving Eq

instance Show Bit where
    show Zero = "|0>"
    show One  = "|1>"

data Qubit a = Qubit (Complex a) (Complex a) deriving Show

sauliX :: Qubit a -> Qubit a
sauliX (Qubit x y) = Qubit y x

sauliY :: (Fractional a, RealFloat a) => Qubit a -> Qubit a
sauliY (Qubit x y) = Qubit rotY rotX
        where i = 0 :+ 1
              rotY = -y*i
              rotX = x*i

sauliZ :: (RealFloat a) => Qubit a -> Qubit a
sauliZ (Qubit x y) = Qubit x (-y)

hadamard :: (RealFloat a) => Qubit a -> Qubit a
hadamard (Quibit x y) = Qubit hadX hadY
         where hadX =


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
