module Regularise where

import Processes
import Expressions
import Data.List
import Auxiliary
import DataSpec
import Debug.Trace

type ProcessBinding  = (ProcessName, ProcessTerm)
type State           = [([Char], [Char])]

toIntermediateForm :: [Process] -> Int -> [String] -> (Type -> String) -> ([Process], State)
toIntermediateForm context initialProcess initialState arbitraryConstant 
  | pcused              = error("Error: Variable name 'pc' reserved; please rename your variable.")
  | variableTwice /= [] = error("Error: Variables with distinct type but same name in use: " ++ (infixString variableTwice ", "))
  | correctInit         = (regularise context newInitState regprocs explored toExplore pars, newInitState)
  | otherwise           = error("Error: Initial state incorrect.")
          where
             regprocs      = []
             first         = context!!initialProcess
             correctInit   = length(initialState) == length(getProcPars first)
             initialState2 = zip (map fst (getProcPars first)) initialState
             rhs           = getRHS first
             explored      = [("X1", rhs)]
             toExplore     = [("X1", rhs)]
             pars          = nub ((getProcPars first) ++ (findNewVariables context [(getProcName first)] rhs))
             newParams     = pars \\ getProcPars first
             newInitState  = initialState2 ++ [(var, arbitraryConstant sort) | (var, sort) <- newParams]
             pcused        = elem "pc" (map fst pars)
             variableTwice = [x | x <- map fst pars \\ nub (map fst pars)]

findNewVariables :: [Process] -> [ProcessName] -> ProcessTerm -> [(Variable, Type)]
findNewVariables context pns (Implication c r1)                                   = findNewVariables context pns r1
findNewVariables context pns (ProcessInstance name pars)  
  | (not (elem name pns)) = (getProcPars proc) ++ findNewVariables context (pns ++ [name]) (getRHS proc)
  | otherwise             = []
  where
    proc = resolveProcess name context
findNewVariables context pns (Plus rs)                                            = concat (map (findNewVariables context pns) rs)
findNewVariables context pns (Sum v t r)                  | variableUsedInProcessTerm v r      = (v,t):(findNewVariables context pns r)
                                                     | otherwise             = (findNewVariables context pns r)
findNewVariables context pns (ActionPrefix reward a aps probs r) = newVariables ++ (findNewVariables context pns r)
  where
    newVariables = [(v,t) | (v,t,f) <- probs, or [(variableUsedInProcessTerm v r), (variableInExpression v f)]]


regularise :: [Process] -> State -> [Process] -> [ProcessBinding] -> [ProcessBinding] -> ProcessPars -> [Process]
regularise context initialState regprocs explored [] pars                     = regprocs
regularise context initialState regprocs explored ((name,rhs):toExplore) pars = regularise context initialState regprocsNew exploredNew toExploreNew pars
       where
         (newRHS, toExplore2) = transform context initialState explored pars rhs
         newProc              = Process name pars newRHS
         regprocsNew          = regprocs ++ [newProc]
         exploredNew          = explored ++ toExplore2
         toExploreNew         = (nub (toExplore ++ toExplore2)) \\ [(name,rhs)] 

transform :: [Process] -> State -> [ProcessBinding] -> ProcessPars -> ProcessTerm -> (ProcessTerm, [ProcessBinding])
transform context initialState explored pars (ActionPrefix reward a aps probs r) = (thisrhs, toExploreNew)
    where
       (newrhs, params)       = getNormalForm r context initialState pars
       processname            = getInstance explored newrhs (length(explored))
       thisrhs                = ActionPrefix reward a aps probs (ProcessInstance processname (params)) 
       toExploreNew           = if (elem processname (map fst explored)) then [] else [(processname, newrhs)] 
transform context initialState explored pars (Implication f rhs)          = (thisrhs, toExploreNew)
    where
       (newrhs, toExploreNew) = transform context initialState explored  pars rhs
       thisrhs                = Implication f newrhs
transform context initialState explored pars (Sum var t r)                 = (thisrhs, toExploreNew)
    where
       (newrhs, toExploreNew) = transform context initialState explored pars r
       thisrhs                = Sum var t newrhs
transform context initialState explored pars (ProcessInstance name parameters)   | correct = (thisrhs, toExploreNew)
                                                                                 | otherwise = error("Error: Process " ++ name ++ " instantiated with the wrong number of arguments")
    where
       processPars            = getProcPars (resolveProcess name context)
       correct                = length processPars == length parameters 
       (newrhs, toExploreNew) = transform context initialState explored pars (getRHS (resolveProcess name context))
       substitutions          = zip (map fst processPars) parameters
       thisrhs                = substituteInProcessTerm substitutions newrhs
transform context initialState explored pars (Plus rs)                = (thisrhs, toExploreNew)
    where
       resultsForAllSummands  = helptransformPlus context initialState explored pars rs
       toExploreNew           = concat (map snd resultsForAllSummands)
       thisrhs                = Plus (map fst resultsForAllSummands)

helptransformPlus :: [Process] -> State -> [ProcessBinding] -> ProcessPars -> [ProcessTerm] -> [(ProcessTerm, [ProcessBinding])]
helptransformPlus context initialState explored pars []     = []
helptransformPlus context initialState explored pars (r:rs) = [(thisrhs, toExploreNew)] ++ helptransformPlus context initialState (explored ++ toExploreNew) pars rs
         where
           (thisrhs, toExploreNew) = transform context initialState explored pars r

-- nog een keer checken of dit niet recursief beter gaat (voor als je een process X = Y() hebt).
getNormalForm :: ProcessTerm -> [Process] -> State -> ProcessPars -> (ProcessTerm, NextPars)
getNormalForm (ProcessInstance name actualPars) context initialState globalPars = (getRHS (resolveProcess name context), extractNextPars2 name (getProcPars (resolveProcess name context)) initialState globalPars actualPars)
getNormalForm x                           context initialState globalPars = (x, extractNextPars globalPars initialState x)


getInstance :: [ProcessBinding] -> ProcessTerm -> Int -> ProcessName
getInstance [] r i                                 = "X" ++ show (i+1) 
getInstance ((name, rhs):explored) r i | rhs == r  = name
                                       | otherwise = getInstance explored r i

extractNextPars :: ProcessPars -> State -> ProcessTerm -> NextPars
extractNextPars [] initialState          rhs = []
extractNextPars ((v,t):pps) initialState rhs | variableUsedInProcessTerm v rhs = (Variable v):(extractNextPars pps initialState rhs)
                                             | otherwise                       = (Variable (getInitialValue initialState v)):(extractNextPars pps initialState rhs)
            
extractNextPars2 :: ProcessName -> ProcessPars -> State -> ProcessPars -> NextPars -> NextPars
extractNextPars2 name formalPars initialState [] actualPars          = []
extractNextPars2 name formalPars initialState ((v,t):pps) actualPars | correct = (mapVariableInInstantiation name actualPars formalPars initialState actualPars v):(extractNextPars2 name formalPars initialState pps actualPars)
  where
    correct = if length formalPars /= length actualPars then error ("Error: Number of parameters in instantiation " ++ name ++ "[" ++ (infixString (map show actualPars) ", ") ++ "] incorrect.") else True

-- Krijgt de parameters van X mee en de expressies die in een processinstantiatie van X(...)
-- zijn meegegeven. Vervolgens wordt een variable v gemapt op hetgeen ie wordt volgens de
-- instantiatie, of op zichzelf als ie niet al voorkwam in de parameters.
mapVariableInInstantiation name pars [] initialState [] var                                         = Variable (getInitialValue initialState var)
mapVariableInInstantiation name pars ((v,t):formalPars) initialState (e:actualPars) var | v == var  = e
                                                                              | otherwise = mapVariableInInstantiation name pars formalPars initialState actualPars var
mapVariableInInstantiation name p w x y z                                                          = error ("Error: Number of parameters in instantiation incorrect.")

getInitialValue :: State -> [Char] -> [Char]
getInitialValue [] k = error("Error: No initial value defined for variable " ++ k)
getInitialValue ((par,value):initial) k | par == k  = value
                                 | otherwise = getInitialValue initial k