-- | Two-way deterministic finite automata
module Data.Automata.AFA
  ( AFA(AFA, delta, initial, states, symbols, finals)
  , S(SA,SE)
  )
where

import Data.Set ( Set )

-- | A state.
data S state = SE state -- ^ S∃
             | SA state -- ^ S∀
               deriving (Ord,Eq)


-- | An alternating finite automaton (AFA).
data AFA state symbol =
    AFA { states  :: Set (S state)                      -- ^ States (S∃ ∪ S∀)
        , symbols :: Set symbol                         -- ^ Input symbols
        , delta   :: S state -> symbol -> Set (S state) -- ^ Transition function
        , initial :: S state                            -- ^ Initial set
        , finals  :: Set (S state)                      -- ^ Final states
        }
