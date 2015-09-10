----------------------------------------------------
-- Processes and some function for handling them  --
--                                                --
-- Finished refactoring: 17-sep-2010              -- 
----------------------------------------------------
 
module Processes where

import Expressions
import Auxiliary
import Debug.Trace
import Data.List
import DataSpec 

---------------------------------------------
------------ Types for processes ------------
---------------------------------------------

type ProcessName = String
type ProcessPars = [(Variable, Type)]

data Process = Process ProcessName ProcessPars ProcessTerm deriving (Show, Eq)
data InitialProcess = InitialProcess ProcessName [Expression] ConstantDefs deriving (Show, Eq)

---------------------------------------------
------ Types for process terms (RHSs) -------
---------------------------------------------

type Action     = String
type ActionPars = [Expression]
type NextPars   = [Expression]
type Updates    = [(String, Expression)]
type ProbDistr  = Expression
type Probabilities = [(Variable, Type, ProbDistr)]
type ProbDef = (Expression, ProcessTerm)
type ConstantDefs  = [(String, String)]
type Reward = Expression

data ProcessTerm  = ActionPrefix     Reward Action ActionPars Probabilities ProcessTerm
                  | ActionPrefix2    Reward Action ActionPars [ProbDef]
                  | Implication      Expression ProcessTerm
                  | LambdaPrefix     Reward Expression ProcessTerm
                  | Plus             [ProcessTerm]
                  | ProcessInstance  ProcessName NextPars
                  | ProcessInstance2 ProcessName Updates
                  | Sum              Variable Type ProcessTerm
          deriving (Eq, Show)

--------------------------------------------------------------
----------- Helper functions to work with processes ----------
---------------------- and process sets ----------------------
--------------------------------------------------------------

-- Given a process, this function returns its name
getProcName :: Process -> ProcessName
getProcName (Process name pars rhs) = name

-- Given a process, this function returns its right-hand side (the process term).
getRHS :: Process -> ProcessTerm
getRHS (Process name pars rhs) = rhs

resolveProcess :: ProcessName -> [Process] -> Process
resolveProcess name []                             = error ("Error: Reference to undefined process " ++ name)
resolveProcess name (x:xs) | name == getProcName x = x
                    | otherwise             = resolveProcess name xs 

getProcPars :: Process -> ProcessPars
getProcPars (Process name pars rhs) = pars

unfoldUpdates :: [Process] -> Process -> Process
unfoldUpdates procs (Process name pars rhs) = Process name pars (unfoldUpdates2 procs rhs)

unfoldUpdates2 :: [Process] -> ProcessTerm -> ProcessTerm
unfoldUpdates2 procs (Sum var typ rhs) = Sum var typ (unfoldUpdates2 procs rhs)
unfoldUpdates2 procs (Plus rhss) = Plus (map (unfoldUpdates2 procs) rhss)
unfoldUpdates2 procs (ProcessInstance name pars) = ProcessInstance name pars
unfoldUpdates2 procs (Implication cond rhs) = Implication cond (unfoldUpdates2 procs rhs)
unfoldUpdates2 procs (LambdaPrefix reward lambda rhs) = LambdaPrefix reward lambda (unfoldUpdates2 procs rhs)
unfoldUpdates2 procs (ActionPrefix reward act pars prob rhs) = ActionPrefix reward act pars prob (unfoldUpdates2 procs rhs)
unfoldUpdates2 procs (ActionPrefix2 reward act pars distrs) = ActionPrefix2 reward act pars [(e, unfoldUpdates2 procs r) | (e,r) <- distrs]
unfoldUpdates2 procs (ProcessInstance2 name pars) | correct      = ProcessInstance name newPars
                                                  | not(correct) = error(message)
  where 
    otherPars = getProcPars (resolveProcess name procs)
    newPars   = [if elem parName (map fst pars) then [v | (n,v) <- pars, n == parName]!!0 else Variable parName | parName <- map fst otherPars]
    correct_  = [elem v (map fst otherPars) | (v,u) <- pars]
    correct   = and correct_
    message   = "Error: Variable update " ++ show (pars!!((elemIndices False correct_)!!0)) ++ " incorrect."

functionaliseProbabilities :: Int -> Process -> Process
functionaliseProbabilities i (Process name pars rhs) = Process name pars (fst (functionaliseProbabilities2 i 1 rhs))

functionaliseProbabilities2 :: Int -> Int -> ProcessTerm -> (ProcessTerm, Int)
functionaliseProbabilities2 i j (ProcessInstance name pars) = (ProcessInstance name pars, j)
functionaliseProbabilities2 i j (Sum var typ rhs) = (Sum var typ newRHS, nextIndex)
  where
    (newRHS, nextIndex) = functionaliseProbabilities2 i j rhs
functionaliseProbabilities2 i j (Implication cond rhs) = (Implication cond newRHS, nextIndex)
  where
    (newRHS, nextIndex) = functionaliseProbabilities2 i j rhs
functionaliseProbabilities2 i j (LambdaPrefix reward lambda rhs) = (LambdaPrefix reward lambda newRHS, nextIndex)
  where
    (newRHS, nextIndex) = functionaliseProbabilities2 i j rhs
functionaliseProbabilities2 i j (ActionPrefix reward act pars prob rhs) = (ActionPrefix reward act pars prob newRHS, nextIndex)
  where
    (newRHS, nextIndex) = functionaliseProbabilities2 i j rhs
functionaliseProbabilities2 i j (Plus rhss) = (Plus newRHSs, nextIndex)
  where
    (newRHSs, nextIndex) = functionaliseList i j rhss
functionaliseProbabilities2 i j (ActionPrefix2 reward act pars distrs) | not(specificCase) = (ActionPrefix reward act pars prob  newRHS , nextIndex) 
                                                                | otherwise         = (ActionPrefix reward act pars prob2 newRHS2, nextIndex2)
  where
    (prob, newRHS, nextIndex)    = transformDistributionGeneral i j distrs
    instantiations               = [getInstantiationName r | (p,r) <- distrs]
    specificCase                 = length (nub instantiations) == 1 && instantiations!!0 /= ""
    (prob2, newRHS2, nextIndex2) = transformDistributionSpecific i j distrs

functionaliseList :: Int -> Int -> [ProcessTerm] -> ([ProcessTerm], Int)
functionaliseList i j [rhs]      = ([newRHS], nextIndex)
  where
    (newRHS, nextIndex) = functionaliseProbabilities2 i j rhs
functionaliseList i j (rhs:rhss) = ((newRHS:newRHSs), nextIndex)
  where
    (newRHS, k)          = functionaliseProbabilities2 i j rhs
    (newRHSs, nextIndex) = functionaliseList i k rhss

getInstantiationName :: ProcessTerm -> String
getInstantiationName (ProcessInstance name pars) = name
getInstantiationName _                           = ""

getInstantiationParameters :: ProcessTerm -> NextPars
getInstantiationParameters (ProcessInstance name pars) = pars
getInstantiationParameters _                           = error("Error: Cannot get process variables of a non-ProcessInstance")

transformDistributionGeneral :: Int -> Int -> [ProbDef] -> (Probabilities, ProcessTerm, Int)
transformDistributionGeneral i j distrs = ([(("i0_" ++ show i ++ "_" ++ show j), TypeRange 1 (length distrs), probDistr)], newRHS, nextIndex)
  where
    probDistr        = createProbDistr i j (map fst distrs) 1
    (rhss,nextIndex) = functionaliseList i (j+1) (map snd distrs)
    newRHS           = Plus (addConditionsToRHSs i j [1..length distrs] rhss)

transformDistributionSpecific :: Int -> Int -> [ProbDef] -> (Probabilities, ProcessTerm, Int)
transformDistributionSpecific i j distrs = ([(("i0_" ++ show i ++ "_" ++ show j), TypeRange 1 (length distrs), probDistr)], newRHS, nextIndex)
  where
    probDistr         = createProbDistr i j (map fst distrs) 1
    (rhss, nextIndex) = functionaliseList i (j+1) (map snd distrs)
    procname          = [getInstantiationName r | (p,r) <- distrs]!!0
    nrPars            = length ([getInstantiationParameters r | (p,r) <- distrs]!!0)
    newRHS            = ProcessInstance procname [ createProbDistr i j (map (!!parNr) (map getInstantiationParameters (map snd distrs))) 1 | parNr <- [0..nrPars -1]] 

addConditionsToRHSs :: Int -> Int -> [Int] -> [ProcessTerm] -> [ProcessTerm]
addConditionsToRHSs nr nr2 [] []             = []
addConditionsToRHSs nr nr2 (i:is) (rhs:rhss) = (Implication (Function "eq" [Variable ("i0_" ++ show nr ++ "_" ++ show nr2), Variable (show i)]) rhs):(addConditionsToRHSs nr nr2 is rhss)

createProbDistr :: Int -> Int -> [Expression] -> Int -> ProbDistr
createProbDistr nr nr2 [e] i    = e
createProbDistr nr nr2 (e:es) i = Function "ifthenelse" [Function "eq" [Variable ("i0_" ++ show nr ++ "_" ++ show nr2), Variable (show i)], e, createProbDistr nr nr2 es (i+1)]

--------------------------------------------------------------
-------------- Helper functions to work with RHSs ------------
--------------------------------------------------------------

-- This function substitutes value expressions for variables in 
-- a RHS. Note that this function can only be applied to RHSs
-- that are already in Intermediate Regular Form.
-- 
-- If summations occur, the variable over which the summation is will be renamed,
-- in order to prevent naming clashes.
substituteInProcessTerm :: [(Variable, Expression)] -> ProcessTerm -> ProcessTerm
substituteInProcessTerm subs (Plus rs)           = Plus (map (substituteInProcessTerm subs) rs)
substituteInProcessTerm subs (Implication c rhs) = Implication (substituteInExpression subs c) 
                                                       (substituteInProcessTerm subs rhs)
substituteInProcessTerm subs (Sum var typ rhs)   = Sum (var ++ "_") typ (substituteInProcessTerm subs2 (substituteInProcessTerm [(var, Variable (var ++ "_"))] rhs))
  where
	subs2 = removeFromSubstitutionsList [var] subs
substituteInProcessTerm subs (ActionPrefix reward a aps probs (ProcessInstance name nextPars))
  = ActionPrefix newReward a newAps newProbs (ProcessInstance name nextvals)
    where
      newReward = substituteInExpression subs reward
      newAps   = map (substituteInExpression subs) aps
      newProbs = [(v, t, substituteInExpression (removeFromSubstitutionsList [v] subs) f) | (v,t,f) <- probs]
      subs2    = removeFromSubstitutionsList [v | (v,t,f) <- probs] subs
      nextvals = map (substituteInExpression subs2) nextPars
substituteInProcessTerm subs other = error("SubstituteInProcessTerm function applied to non-IRF RHS.")

-- This function takes a variable and a list of variable-expression substitutions.
-- It returns the same list, but with the substitution removed for the variable
-- given as the first parameter.
removeFromSubstitutionsList :: [Variable] -> [(Variable, Expression)] -> [(Variable, Expression)]
removeFromSubstitutionsList vars []             = []
removeFromSubstitutionsList vars ((v,val):subs) | elem v vars = rest
                                                | otherwise = (v,val):rest
  where
	rest = removeFromSubstitutionsList vars subs

-- This function checks whether or not a certain variable occurs (unbound) in a RHS.
variableUsedInProcessTerm :: Variable -> ProcessTerm -> Bool
variableUsedInProcessTerm var (Implication cond rhs)      = or [(variableInExpression var cond), 
                                                          (variableUsedInProcessTerm var rhs)]
variableUsedInProcessTerm var (ProcessInstance name pars) = or (map (variableInExpression var) pars)
variableUsedInProcessTerm var (Plus rhss)                 = or (map (variableUsedInProcessTerm var) rhss)
variableUsedInProcessTerm var (Sum v typ rhs) | var == v  = False
                                      | otherwise = variableUsedInProcessTerm var rhs
variableUsedInProcessTerm var (ActionPrefix reward act pars probs rhs)
  = inReward || inAps || inDistr || ((not inProbSums) && variableUsedInProcessTerm var rhs)
    where
      inReward   = variableInExpression var reward
      inAps      = or (map (variableInExpression var) pars)
      inDistr    = or [variableInExpression var f | (v,t,f) <- probs, var /= v]
      inProbSums = [v | (v,t,f) <- probs, var == v] /= []