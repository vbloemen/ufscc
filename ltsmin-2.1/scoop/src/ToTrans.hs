module ToTrans where

import ToPA
import StepPA
import LPPE
import Confluence
import DataSpec
import Data.Ratio
import Data.HashMap

toTrans :: Bool -> PSpecification -> ConfluentSummands -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [String] -> String
toTrans ignorecycles spec confl checkConfluence checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions = show (size (fst statespace)) ++ " ? ?" ++ (printTransitions transitions (-1) 0)
  where
    (statespace,initial,_) = getStateSpace ignorecycles spec confl checkConfluence False checkDTMC isMA removeRates preserveDivergence showDeadlocks storeReps reachActions
    transitions            = snd statespace

printTransitions :: [(Int, String, EdgeLabel, [(Probability, Int)])] -> Int -> Int -> String 
printTransitions []                                  prev index = ""
printTransitions ((from, reward, label, []):ts)              prev index 
  | from == prev = printTransitions ts from (index + 1)
  | otherwise    = printTransitions ts from 0
printTransitions ((from, reward, label, ((prob,to):tos)):ts) prev index
  | from == prev = "\n" ++ show from ++ " " ++ show index ++ " " ++ show to ++ " " ++ probString ++ printTransitions ((from, reward, label, tos):ts) from index
  | otherwise    = "\n" ++ show from ++ " " ++ show 0     ++ " " ++ show to ++ " " ++ probString ++ printTransitions ((from, reward, label, tos):ts) from 0
  where
    probFrac   = getFraction prob
    probFrac2  = (toInteger (numerator probFrac)) % (toInteger (denominator probFrac))
    probString = show (fromRational probFrac2)
