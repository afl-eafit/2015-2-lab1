{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- File   : SimAFA
-- Author : Alejandro Gómez Londoño
-- Date   : Thu Jul 30 22:04:59 2015
-- Description :
--------------------------------------------------------------------------------
-- Change log : Lab1 solution

--------------------------------------------------------------------------------

module SimAFA where

import Data.Automata.AFA
import Data.Automata.AFA.Examples
import Data.Set       (member)
import Data.Foldable  (all,any,Foldable,foldr)
import Prelude hiding (all,any,null,foldr)

accepts ∷ Ord state ⇒ AFA state symbol → [symbol] → Bool
accepts x σ = acc x σ (initial x)

null' ∷  Foldable t ⇒ t a -> Bool
null' = foldr (\_ _ -> False) True

acc ∷ Ord state ⇒ AFA state symbol → [symbol] → S state →  Bool
acc x []    ρ        = member ρ (finals x)
acc x (a:σ) ρ@(SE s) = let ω = (delta x ρ a) in any (acc x σ)  ω
acc x (a:σ) ρ@(SA s) = let ω = (delta x ρ a) in all (acc x σ)  ω
                                             && (not . null' $  ω)
