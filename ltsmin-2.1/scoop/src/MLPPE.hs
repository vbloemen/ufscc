module MLPPE where

import Processes
import DataSpec
import Expressions
import Auxiliary
import Data.List
import LPPE
import Debug.Trace

type MSpecification = (MLPPE, InitialState, DataSpec)

type MSummand       = (LocalPars, Condition, Expression, NextPars)
data GeneralSummand = MSummand MSummand | PSummand PSummand deriving (Show, Eq)

data MLPPE          = MLPPE ProcessName ProcessPars [GeneralSummand] deriving (Show, Eq)
	
data MLPPEShowMAPA = MLPPEShowMAPA MSpecification
instance Show MLPPEShowMAPA where
  show (MLPPEShowMAPA (MLPPE name params summands, initialState, dataspec)) = datastring ++ "\n"  ++ name ++ printPairs (newParams) ++ 
                                                      " = \n      " ++ summandsString ++ "\n\ninit "++ name ++ "[" ++ (infixString initialState ", ") ++ "]"
    where
      summandsString = infixString (map (printSummandMAPA dataspec name params) summands) " \n    ++ "
      types                 = nub (["Bool"] 
                            ++ map (printType.snd) params ++ [printType t | summand <- summands, (v,t) <- getMLocalPars summand]
                            ++ [printType t | summand <- summands, (v,t,f) <- getMProbChoices summand] )
      functions      = concat (map getFunctionsInMSummand summands)
      dataspec2      = removeUnusedTypesAndFunctions dataspec (types \\ ["Bool"]) functions
      datastring     = "\n  \n" ++ printDataSpec dataspec2 ++ "\n  \n"
      newParams      = map (\x -> (fst x, printType (snd x))) params

data MLPPEShow = MLPPEShow MSpecification Bool
instance Show MLPPEShow where
  show (MLPPEShow (MLPPE name params summands, initialState, dataspec) style) = name ++ printPairs newParams ++ " = \n      " ++ summandsString
                                                                              ++ "\n\nInitial state: " ++ name ++ commaList initialState
    where
      summandsString = infixString (map (printMSummand dataspec style name params) summands) " \n    + "
      newParams      = map (\x -> (fst x, printType (snd x))) params
  
-- This function prints a summand in MAPA style.
printSummandMAPA :: DataSpec -> ProcessName -> ProcessPars -> GeneralSummand -> String
printSummandMAPA dataspec procName procPars (PSummand (params, c, reward, a, aps, probChoices, g)) = 
  printPSummandPRCRL dataspec procName procPars (params, c, reward, a, aps, probChoices, g)
printSummandMAPA dataspec procName procPars (MSummand (params, c, lambda, g)) = 
	"(" ++ localParsToString ++ printExpression c ++ " => <" ++ show lambda ++ "> . "  
  ++ printNextState dataspec True procName procPars (params, c, Variable "0", "", [], [], g) [] g
  ++ (if (length params == 0) then "" else ")") ++ ")"
    where
      parsList = (infixString [left ++ " : " ++ (printType right) | (left, right) <- params] ", ")
      localParsToString | length params == 0 = ""
	                    | otherwise          = "sum(" ++ parsList ++ ", "


-- This function prints a summand in MAPA style.
printMSummand :: DataSpec -> Bool -> ProcessName -> ProcessPars -> GeneralSummand -> String
printMSummand dataspec style procName procPars (PSummand summand)  = 
  printPSummand dataspec style procName procPars summand
printMSummand dataspec style procName procPars (MSummand (params, c, lambda, g)) =
	"(" ++ localParsToString ++ show c ++ " => (" ++ show lambda ++ ") . " ++ 
	printNextState dataspec style procName procPars (params, c, Variable "0", "", [], [], g) [] g
  ++ (if (length params == 0) then "" else ")") ++ ")"
    where
      parsList = (infixString [left ++ " : " ++ (printType right) | (left, right) <- params] ", ")
      localParsToString | length params == 0 = ""
	                    | otherwise          = "sum(" ++ parsList ++ ", "



decode :: PSpecification -> MSpecification
decode (LPPE name params summands, initialState, dataSpec) = (MLPPE name params newSummands, initialState, dataSpec)
  where
    newSummands = map decodeSummand summands

decodeSummand :: PSummand -> GeneralSummand
decodeSummand (params, c, reward, a, aps, probChoices, g) | a == "rate" = MSummand (params, c, aps!!0, g)
                                                          | otherwise   = PSummand (params, c, reward, a, aps, probChoices, g)

encode :: MSpecification -> PSpecification
encode (MLPPE name pars summands, initialState, dataSpec) = (LPPE name pars newSummands, initialState, dataSpec)
  where
    newSummands = map encodeSummand summands

getMLPPESize :: MSpecification -> Int
getMLPPESize (MLPPE name pars summands, initialState, dataSpec) = initialProcess + initialStateSize + summation + parsSize + summandsSize
  where
    initialProcess     = 1 
    initialStateSize   = length initialState 
    summation          = length summands - 1
    parsSize           = length pars
    summandsSize       = sum (map getSummandSize summands)

getSummandSize (MSummand (params, c, l, g)) = parSize + conditionSize + rateSize + nextStateSize
  where
    parSize       = if params == [] then 0 else 1 + length params
    conditionSize = 1 + getExpressionSize c
    rateSize      = getExpressionSize l
    nextStateSize = 1 + sum (map getExpressionSize g)
getSummandSize (PSummand (params, c, reward, a, aps, probChoices, g)) = parSize + conditionSize + rewardSize + actionSize + probSize + nextStateSize
  where
    parSize       = if params == [] then 0 else 1 + length params
    conditionSize = 1 + getExpressionSize c
    rewardSize    = 1 + getExpressionSize reward
    actionSize    = 1 + sum (map getExpressionSize aps)
    probSize      = length probChoices + sum (map getExpressionSize (map thd3 probChoices))
    nextStateSize = 1 + sum (map getExpressionSize g)

encodeSummand :: GeneralSummand -> PSummand
encodeSummand (MSummand (params, c, l, g)) | labelTooSmall l = error("Rate " ++ show l ++ " not allowed.")
                                           | otherwise       = (params, c, Variable "0", "rate", [l], [], g)
encodeSummand (PSummand summand)           = summand

labelTooSmall (Variable v) = isFraction v && (getFraction v <= getFraction "0")
labelTooSmall _            = False

-- Provides the local parameters of a summand
getMLocalPars :: GeneralSummand -> LocalPars
getMLocalPars (PSummand summand)           = getLocalPars summand
getMLocalPars (MSummand (params, c, l, g)) = params

-- Provides the condition of a summand
getMCondition :: GeneralSummand -> Condition
getMCondition (PSummand summand)           = getCondition summand
getMCondition (MSummand (params, c, l, g)) = c

isMarkovian :: GeneralSummand -> Bool
isMarkovian (PSummand summand) = False
isMarkovian (MSummand summand) = True

isInteractive :: GeneralSummand -> Bool
isInteractive (PSummand summand) = True
isInteractive (MSummand summand) = False

getNrParamsM :: MSpecification -> Int
getNrParamsM (MLPPE name params newSummands, initialState, dataSpec) = length params

-- Provides the action of a summand
getMAction :: GeneralSummand -> Action
getMAction (PSummand summand)           = getAction summand

-- Provides the probabililstic summations of a summand
getMProbChoices :: GeneralSummand -> [ProbChoice]
getMProbChoices (PSummand summand)           = getProbChoices summand
getMProbChoices (MSummand (params, c, l, g)) = []

getFunctionsInMSummand :: GeneralSummand -> [String]
getFunctionsInMSummand (PSummand summand)           = getFunctionsInPSummand summand
getFunctionsInMSummand (MSummand (params, c, l, g)) = getFunctionsInPSummand (params, c, Variable "0", "", [l], [], g)

getMNextState :: GeneralSummand -> [Expression]
getMNextState (PSummand (p,c,reward,a,aps,prob,g)) = g
getMNextState (MSummand (params, c, l, g))  = g

changeMNextState :: GeneralSummand -> [Expression] -> GeneralSummand
changeMNextState (PSummand (p,c,reward,a,aps,prob,g)) g2 = PSummand (p,c,reward,a,aps,prob,g2)
changeMNextState (MSummand (params, c, l, g))  g2 = MSummand (params, c, l, g2)

changeMProbChoices :: GeneralSummand -> [ProbChoice] -> GeneralSummand
changeMProbChoices (PSummand (p,c,reward,a,aps,prob,g)) prob2 = PSummand (p,c,reward,a,aps,prob2,g)
changeMProbChoices (MSummand (params, c, l, g))  prob2 = MSummand (params ++ [(v,t) | (v,t,f) <- prob2], c, Function "multiply" ([f | (v,t,f) <- prob2] ++ [l]), g)

makeConditionMLPPE :: GeneralSummand -> ProcessPars -> Condition
makeConditionMLPPE summand pars = makeCondition pars (getMNextState summand)