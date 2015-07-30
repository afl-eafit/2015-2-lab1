module Data.Automata.AFA.Examples where

import Data.Automata.AFA (AFA(..),S(..))

import Data.Set (Set, fromList, singleton, empty )

data StateM = Q0 | Q1 | Q2 deriving (Eq, Ord)

data SymbolM = A | B deriving (Eq, Ord)

m :: AFA StateM SymbolM
m = AFA (fromList [SE Q0, SA Q1, SE Q2])
        (fromList [A,B])
        deltaM
        (SE Q0)
        (singleton (SE Q2))
    where
      deltaM :: S StateM -> SymbolM -> Set (S StateM)
      deltaM (SE Q0) A = singleton (SE Q0)
      deltaM (SE Q0) B = fromList [SA Q1,SE Q0]
      deltaM (SA Q1) A = fromList [SE Q0,SE Q2]
      deltaM (SA Q1) B = fromList [SA Q1,SE Q2]
      deltaM _       _ = empty
