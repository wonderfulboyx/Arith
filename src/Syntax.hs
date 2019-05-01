module Syntax where

data Term = TmTrue
          | TmFalse
          | TmZero
          | TmIsZero Term
          | TmSucc Term
          | TmPred Term
          | TmIf Term Term Term
          deriving(Show)