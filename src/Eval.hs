-- Arith/Eval.hs
module Eval where

import Syntax

----------------------------------
eval1 :: Term -> Term
eval1 TmZero = TmZero
eval1 TmTrue = TmTrue
eval1 TmFalse = TmFalse
eval1 (TmIf TmTrue t1 t2) = t1
eval1 (TmIf TmFalse t1 t2) = t2
eval1 (TmIf t0 t1 t2) =
  let t0' = eval1 t0 in
    TmIf t0' t1 t2
eval1 (TmSucc t1) =
  let t1' = eval1 t1 in
    TmSucc t1'
eval1 (TmPred TmZero) = TmZero
eval1 (TmPred (TmSucc nv1))
  | isnumericval nv1 = nv1
eval1 (TmPred t1) =
  let t1' = eval1 t1 in
    TmPred t1'
eval1 (TmIsZero TmZero) = TmTrue
eval1 (TmIsZero (TmSucc nv1))
  | isnumericval nv1 = TmFalse
eval1 (TmIsZero t1) =  
  let t1' = eval1 t1 in
    TmIsZero t1'
----------------------------------

isnumericval :: Term -> Bool
isnumericval TmZero = True
isnumericval (TmSucc t1) = isnumericval t1
isnumericval _ = False