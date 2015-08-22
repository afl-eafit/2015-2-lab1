module Data.Automata.AFA.Examples where

import Data.Automata.AFA (AFA(..),S(..))

import Data.Set (Set, fromList, singleton, empty )

data StateM = Q0 | Q1 | Q2 | Q3 | Q4 deriving (Eq, Ord)

data SymbolM = A | B | C | D deriving (Eq, Ord, Show)

m1 :: AFA StateM SymbolM
m1 = AFA (fromList [SE Q0, SA Q1, SE Q2])
        (fromList [A,B])
        deltaM
        (SE Q0)
        (singleton (SE Q2))
    where
      deltaM :: S StateM -> SymbolM -> Set (S StateM)
      deltaM (SE Q0) A = singleton (SE Q0)
      deltaM (SE Q0) B = fromList  [SA Q1,SE Q0]
      deltaM (SA Q1) A = singleton (SE Q2)
      deltaM (SA Q1) B = fromList [SA Q1,SE Q2]
      deltaM _       _ = empty

m2 :: AFA StateM SymbolM
m2 = AFA  (singleton $ SE Q0) empty (\_ _ -> empty) (SE Q0) empty

m3 :: AFA StateM SymbolM
m3 = AFA (singleton $ SE Q0)  empty (\_ _ -> empty) (SE Q0) (singleton $ SE Q0)

m4 :: AFA Int Int
m4 = AFA (fromList [SE 0,SE 1,SE 2,SE 3])
         (fromList [0..])
         (deltaM)
         (SE 0)
         (singleton  (SE 3))
    where
      deltaM :: S Int -> Int -> Set (S Int)
      deltaM (SE 0) 0 = fromList [SE 0, SE 1]
      deltaM (SE 1) 1 = fromList [SE 1, SE 2]
      deltaM (SE 2) 2 = fromList [SE 2, SE 3]
      deltaM _      _ = empty
