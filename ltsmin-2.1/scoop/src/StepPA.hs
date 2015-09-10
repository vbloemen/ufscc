module StepPA where

import Expressions
import LPPE
import Data.List
import DataSpec 
import Processes
import Confluence
import Auxiliary
import Debug.Trace
import Data.Maybe

type State             = [Constant]
type Mapping           = [(Variable, Constant)]
type Transition        = (State, RewardString, EdgeLabel, [(Probability, State)])
type Probability       = String
type EdgeLabel         = String
type RewardString      = String

potentialBehaviour a b c d e f g = potentialBehaviour2 a b c d e f g

potentialBehaviour2 :: Bool -> PSpecification -> ConfluentSummands -> DataSpec -> State -> Mapping -> [PSummand] -> [Transition]
--potentialBehaviour followConfluent spec confluents dataspec sourceState mapping _      | trace(show followConfluent ++ " " ++ show sourceState) False = error("Blaat") 
potentialBehaviour2 followConfluent spec confluents dataspec sourceState mapping []     = []
potentialBehaviour2 followConfluent spec confluents dataspec sourceState mapping (s:ss) = trans ++ rest
  where
    trans = exploreSummand followConfluent spec confluents dataspec sourceState mapping s
    rest  = potentialBehaviour2 followConfluent spec confluents dataspec sourceState mapping ss

exploreSummand :: Bool -> PSpecification -> ConfluentSummands -> DataSpec -> State -> Mapping -> PSummand -> [Transition]
exploreSummand followConfluent spec confluent dataspec sourceState mapping (((var,sort):params), c, reward, a, aps, probChoices, g)
  = unfoldSummation followConfluent spec confluent dataspec sourceState mapping (var,sort) (getValues dataspec sort) (params, c, reward, a, aps, probChoices, g)
exploreSummand followConfluent spec confluent dataspec sourceState mapping ([], c, reward, a, aps, probChoices, g)
  | executable = transitions -- (states, transitions)
  | otherwise  = [] -- ([], [])
  where
    executable  = evalExpr (thd3 dataspec) mapping c == "T"
    steps       = generateTransitions followConfluent spec confluent dataspec mapping probChoices g "1"
--    states      = List.map snd steps
    transitions = [(sourceState, evalExpr (thd3 dataspec) mapping reward, a ++ (optionalParentheses (infixString (Data.List.map (evalExpr (thd3 dataspec) mapping) aps) ",")), steps)]

unfoldSummation :: Bool -> PSpecification -> ConfluentSummands -> DataSpec -> State -> Mapping -> (Variable,Type) -> [Constant] -> PSummand -> [Transition]
unfoldSummation followConfluent spec confluent dataspec sourceState mapping (var,sort) [] summand = []
unfoldSummation followConfluent spec confluent dataspec sourceState mapping (var,sort) (value:values) summand = t1 ++ t2
  where
    mapping2 = updateMapping mapping var value
    t1       = exploreSummand followConfluent spec confluent dataspec sourceState mapping2 summand
    t2       = unfoldSummation followConfluent spec confluent dataspec sourceState mapping (var,sort) values summand

generateTransitions :: Bool -> PSpecification -> ConfluentSummands -> DataSpec -> Mapping -> [(Variable, Type, ProbDistr)] -> NextPars -> String -> [(Probability, State)]
generateTransitions followConfluent spec confluent dataspec mapping [] g frac | nextProb /= "0" = [(nextProb, nextState2)]
                                                                                 | otherwise       = []
  where
    nextState  = Data.List.map (evalExpr (thd3 dataspec) mapping) g
    nextState2 = nextState -- if followConfluent then nextState else getRepresentative spec confluent nextState
    nextProb   = frac
generateTransitions followConfluent spec confluent dataspec mapping ((v,t,f):probChoices) g frac
  = unfoldProbChoice followConfluent spec confluent dataspec mapping (v,t) f g (getValues dataspec t) probChoices frac
   
unfoldProbChoice :: Bool -> PSpecification -> ConfluentSummands -> DataSpec -> Mapping -> (Variable, Type) -> ProbDistr -> NextPars -> [Constant] -> [(Variable, Type, ProbDistr)] -> String -> [(Probability, State)]
unfoldProbChoice followConfluent spec confluent dataspec mapping (v,t) f g [] probChoices frac             = []
unfoldProbChoice followConfluent spec confluent dataspec mapping (v,t) f g (value:values) probChoices frac = steps1 ++ steps2
  where
    mapping2 = updateMapping mapping v value -- (v,t) value
    nextProb = evalExpr (thd3 dataspec) mapping2 f
    newFrac  = writeFraction ((getFraction frac) * (getFraction nextProb))
    steps1   = generateTransitions followConfluent spec confluent dataspec mapping2 probChoices g newFrac
    steps2   = unfoldProbChoice followConfluent spec confluent dataspec mapping (v,t) f g values probChoices frac

evalExpr :: [FunctionDef] -> Mapping -> Expression -> Constant
-- --evalExpr dataspec mapping (Function "concat" [s1, s2])      = evalExpr dataspec mapping (Variable ((evalExpr dataspec mapping s1) ++ (evalExpr dataspec mapping s2)))
evalExpr functions mapping (Variable v) | value == Nothing   = v
                                        | otherwise           = Data.Maybe.fromJust value
  where
    value = getValue mapping v
--evalExpr functions mapping (Function "and" pars)             | evalPars!!0 == "F" = "F"
--                                                             | otherwise          = evaluateFunction "and" evalPars functions
--  where
--    evalPars = (Data.List.map (evalExpr functions mapping) pars)
--evalExpr functions mapping (Function "or" pars)             | evalPars!!0 == "T" = "T"
--                                                             | otherwise          = evaluateFunction "or" evalPars functions
--  where
--    evalPars = (Data.List.map (evalExpr functions mapping) pars)
evalExpr functions mapping (Function name pars)              = evaluateFunction name evalPars functions
  where
    evalPars = (Data.List.map (evalExpr functions mapping) pars)

getValue :: Mapping -> [Char] -> Maybe Constant
getValue [] k = Nothing -- error("No value defined for variable " ++ k)
getValue ((key,value):mapping) k | key == k = Just value
                                      | otherwise = getValue mapping k
--getValue ((key,sort,value):mapping) k | key == k = Just value
--                                      | otherwise = getValue mapping k

updateMapping :: Mapping -> String -> Constant -> Mapping
updateMapping [] v newVal = [(v, newVal)]
updateMapping ((key,value):mapping) v newVal | key == v  = ((v,newVal):mapping)
                                                  | otherwise = ((key,value):(updateMapping mapping v newVal))
-- updateMapping [] (v,t) newVal = [(v, t, newVal)]
--updateMapping ((key,sort,value):mapping) (v,t) newVal | key == v  = ((v,t,newVal):mapping)
--                                                | otherwise = ((key,sort,value):(updateMapping mapping (v,t) newVal))


computeReward :: PSpecification -> State -> [(Expression, Expression)] -> Int
computeReward _ state [] = 0
computeReward (lppe,initial,dataspec) state ((c,r):rest)
  = (if (evalExpr functions mapping c == "T") then parseInteger (evalExpr functions mapping r) else 0) + computeReward (lppe, initial, dataspec) state rest
  where
    pars = map fst (getLPPEPars lppe)
    mapping = [(pars!!i, state!!i) | i <- [0.. length pars - 1]]
    functions = thd3 dataspec