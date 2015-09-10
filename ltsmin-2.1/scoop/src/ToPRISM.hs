module ToPRISM where

import LPPE
import Auxiliary
import Data.List
import Expressions
import DataSpec
import ParserUntil
import ParserAux
import Processes
import Simplify
import Debug.Trace

data PRISMSpec = PRISMSpec String
instance Show PRISMSpec  where
  show (PRISMSpec string)    = string

type ActionTypes = [(String, Type)]

toPRISM :: ActionTypes -> Bool -> [String] -> PSpecification -> PRISMSpec
toPRISM actiontypes_ checkuntil untilformula ((LPPE name pars summands), init, dataspec) = 
  toPRISM2 actiontypes_ checkuntil untilformula ((LPPE name pars summands2), init, dataspec2)
    where
	   (dataspec2, summands2) = dataToPRISM (fst3 dataspec) dataspec summands

toPRISM2 :: ActionTypes -> Bool -> [String] -> PSpecification -> PRISMSpec
toPRISM2 actiontypes_ checkuntil untilformula ((LPPE name pars summands), init, dataspec) 
  | checkuntil && length untilformula /= 1 = error("Error: Please specify precisely one until formula.") 
  | otherwise = PRISMSpec spec
  where
    until               = (parseUntil.lexerUntil) (untilformula!!0)
    untilActions        = getUntilActions until
    (summands2,updates) = uniqueActions summands untilActions
    summands3           = if checkuntil then summands2 else summands
    actiontypes         = if checkuntil then [] else actiontypes_
    unfoldedSummands    = unfoldSummands dataspec summands3
    parameters          = parametersToPRISM init pars dataspec
    transitions         = summandsToPRISM pars checkuntil actiontypes dataspec unfoldedSummands
    actions             = nub [(name,if elem (takeWhile (/= '_') name) untilActions then pars else []) | (name, pars) <- map (\x -> (getAction x, getActionPars x)) summands3]
    until2              = updateUntilFormula until updates
    untilobserver       = if not(checkuntil) then "" else createUntilObserver dataspec until2 actions ++ "\n\nlabel \"satisfied\" = untilState = 1;"
    spec                = "mdp" ++ "\n\n" ++ 
	                      "module System" ++ "\n\n" ++ parameters ++ "\n" ++ printActionpars actiontypes ++ "\n" ++ transitions ++ "endmodule" ++ untilobserver

printActionpars []                             = []
printActionpars ((name, NoType):rest) = "  " ++ name ++ " : bool init false;\n" ++ (printActionpars rest)
printActionpars ((name, TypeName "Bool"):rest) = "  " ++ name ++ " : bool init false;\n" ++ "  " ++ name ++ "Value : bool init false;\n" ++ (printActionpars rest)
printActionpars ((name, TypeRange from to):rest) = "  " ++ name ++ " : bool init false;\n" ++ "  " ++ name ++ "Value : " ++ prismType ++ " init " ++ show from ++ ";\n" ++ (printActionpars rest)
  where
    prismType = "[" ++ show from ++ ".." ++ show to ++ "]"

parametersToPRISM init [] _ = ""
parametersToPRISM (init:inits) ((var, TypeName "Bool"):pars) dataspec = "  " ++ var ++ " : bool init " ++ (if init == "T" then "true" else (if init == "F" then "false" else init)) ++ ";\n" ++ (parametersToPRISM inits pars dataspec)
parametersToPRISM (init:inits) ((var, TypeRange from to):pars) dataspec = "  " ++ var ++ " : " ++ prismType ++ " init " ++ (if init == "T" then "true" else (if init == "F" then "false" else init)) ++ ";\n" ++ (parametersToPRISM inits pars dataspec)
  where
    prismType = "[" ++ show from ++ ".." ++ show to ++ "]"
parametersToPRISM (init:inits) ((var,TypeName name):pars) dataspec = "  " ++ var ++ " : " ++ prismType ++ " init " ++ (if init == "T" then "true" else (if init == "F" then "false" else init)) ++ ";\n" ++ (parametersToPRISM inits pars dataspec)
  where
    prismType = getTypeSpec name (fst3 dataspec)

getTypeSpec typeName ((EnumInt name from to):xs) | name == typeName = "[" ++ show from ++ ".." ++ show to ++ "]"
	                                             | otherwise        = getTypeSpec typeName xs
getTypeSpec typeName ((EnumString name values):xs) | name == typeName = error("No non-integer types allowed when translating to PRISM.")
	                                               | otherwise      = getTypeSpec typeName xs

summandsToPRISM _ _ _ _ [] = ""
summandsToPRISM procPars checkuntil actions dataspec ((params, c, reward, a, aps, probChoices, g):xs) | reward /= Variable "0" = error("Rewards not yet supported in export to PRISM.")
                                                                                                      | otherwise = "  " ++ thisSummand ++ ";\n" ++ (summandsToPRISM procPars checkuntil actions dataspec xs)
  where
    action      = "[" ++ a ++ "]" -- if checkuntil then "[" ++ a ++ "]" else (if (elem a (map fst actions)) then "[" ++ a ++ "]" else "[]")
    condition   = expressionToPRISM c
    updates     = getUpdated probChoices (zip (map fst procPars) g) 
    observables = if (elem a (map fst actions)) then ["(" ++ a ++ "' = true)"] 
	                                        ++ (if length aps > 0 then ["(" ++ a ++ "Value' = " ++ (expressionToPRISM (aps!!0)) ++ ")"] 
		                                                          else [])
	              else []
    observables2 = observables ++ (map (\x -> "(" ++ x ++ "' = false)") [action | (action,t) <- actions, action /= a])
                               ++ (map (\x -> "(" ++ x ++ "Value' = false)") [action | (action,TypeName "Bool") <- actions, action /= a])
                               ++ (map (\(x,y) -> "(" ++ x ++ "Value' = " ++ show y ++ ")") [(action,from) | (action,TypeRange from to) <- actions, action /= a])
    observeString = (if length observables2 > 0 then " & " ++ infixString observables2 " & " else "") 
    thisSummand = action ++ " " ++ condition ++ " -> " ++ (unfoldProb dataspec observeString probChoices updates)  
                  

expressionToPRISM expression = expressionToPRISM2 (simplifyExpression ([],[],[]) expression)

expressionToPRISM2 (Variable v) | v == "F"  = "false"
                               | v == "T"  = "true"
                               | otherwise = v
expressionToPRISM2 (Function "and" pars) = "(" ++ infixString (map (\x -> expressionToPRISM2 x) pars) " & " ++ ")"
expressionToPRISM2 (Function "or" pars) = "(" ++ infixString (map (\x -> expressionToPRISM2 x) pars) " | " ++ ")"
expressionToPRISM2 (Function "eq" [x,y]) = "(" ++ (expressionToPRISM2 x) ++ " = " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "min" [x,y]) = "min(" ++ (expressionToPRISM2 x) ++ ", " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "max" [x,y]) = "max(" ++ (expressionToPRISM2 x) ++ ", " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "mod" [x,y]) = "mod(" ++ (expressionToPRISM2 x) ++ ", " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "lt" [x,y]) = "(" ++ (expressionToPRISM2 x) ++ " < " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "leq" [x,y]) = "(" ++ (expressionToPRISM2 x) ++ " <= " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "gt" [x,y]) = "(" ++ (expressionToPRISM2 x) ++ " > " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "geq" [x,y]) = "(" ++ (expressionToPRISM2 x) ++ " >= " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "plus" [x,y]) = "(" ++ (expressionToPRISM2 x) ++ " + " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "multiply" [x,y]) = "(" ++ (expressionToPRISM2 x) ++ " * " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "divide" [x,y]) = "(" ++ (expressionToPRISM2 x) ++ " / " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "minus" [x,y]) = "(" ++ (expressionToPRISM2 x) ++ " - " ++ (expressionToPRISM2 y) ++ ")"
expressionToPRISM2 (Function "ifthenelse" [x,y,z]) = "(" ++ (expressionToPRISM2 x) ++ " ? " ++ (expressionToPRISM2 y) ++ " : " ++ (expressionToPRISM2 z) ++ ")"  
expressionToPRISM2 (Function "not" [x]) = "(" ++ "!" ++ (expressionToPRISM2 x) ++ ")"
expressionToPRISM2 (Function name pars)  = error("Error: Function " ++ name ++ " not available in PRISM.")

unfoldSummands :: DataSpec -> [PSummand] -> [PSummand] 
unfoldSummands _ [] = []
unfoldSummands dataspec (s:ss) = unfoldSummand dataspec s ++ unfoldSummands dataspec ss

unfoldSummand :: DataSpec -> PSummand -> [PSummand]
unfoldSummand dataspec ([], c, reward, a, aps, probChoices, g)     = [([], c, reward, a, aps, probChoices, g)]
unfoldSummand dataspec (((var,typ):pars), c, reward, a, aps, probChoices, g) = concat (map (unfoldSummand dataspec) newSummands)
  where
    newSummands = unfoldNonDet var (getValues dataspec typ) (pars, c, reward, a, aps, probChoices, g)

unfoldNonDet var [] _ = []
unfoldNonDet var (val:values) (pars, c, reward, a, aps, probChoices, g) = (pars, newCondition, newReward, a, newAps, newProbChoices, newG):(unfoldNonDet var values (pars, c, reward, a, aps, probChoices, g))
  where
    newReward      = substituteInExpression [(var, (Variable val))] reward
    newCondition   = substituteInExpression [(var, (Variable val))] c
    newAps         = map (substituteInExpression [(var, (Variable val))]) aps
    newProbChoices = map (\x -> (fst3 x, snd3 x, substituteInExpression [(var, (Variable val))] (thd3 x))) probChoices
    newG           = map (substituteInExpression [(var, (Variable val))]) g

getUpdated _ [] = []
getUpdated probChoices ((parName, nextState):xs) | unchanged = getUpdated probChoices xs
                                                  | otherwise = (parName, nextState):(getUpdated probChoices xs)
  where
	unchanged = nextState == Variable parName && (not(elem parName (map fst3 probChoices)))

unfoldProb :: DataSpec -> String -> [ProbChoice] -> [(Variable, Expression)] -> String
unfoldProb dataspec observeString _  []   = "true" ++ observeString
unfoldProb dataspec observeString [(v,t,Variable "1")] updates   = infixString (map (\x -> "(" ++ fst x ++ "'" ++ " = " ++ expressionToPRISM (substituteInExpression [(v, Variable ((getValues dataspec t)!!0))] (snd x)) ++ ")") updates) " & " ++ observeString
unfoldProb dataspec observeString [(v,t,probExpression)] updates = "\n      " ++ infixString (printProbDistr (unfoldProb3 dataspec probExpression v (getValues dataspec t) updates)) (observeString ++ "\n     +") ++ observeString
unfoldProb dataspec observeString probChoices updates | allOne   =  infixString (map (\x -> "(" ++ fst x ++ "'" ++ " = " ++ expressionToPRISM (snd x) ++ ")") updates) " & " ++ observeString
                                        | otherwise = "\n      " ++ infixString (printProbDistr2 probExpressions probProduct updates) (observeString ++ "\n     +")
  where
    allOne                          = ((nub [x | (v,t,Variable x) <- probChoices]) == ["1"]) && ([x | (v,t,Function x pars) <- probChoices] == [])
    (probProduct_, probExpressions) = probChoicesProduct dataspec probChoices
    probProduct                     = allCombinations probProduct_
    prob                            = infixString (map (\x -> "(" ++ expressionToPRISM x ++ ")") probExpressions) " * "

probChoicesProduct dataspec [(v,t,prob)]    = ([[(v, Variable val) | val <- getValues dataspec t]], [prob])
probChoicesProduct dataspec ((v,t,prob):xs) = (choices, probabilities)
  where
    (choicesRest,probRest) = probChoicesProduct dataspec xs
    choices                = ([(v, Variable val) | val <- getValues dataspec t]:choicesRest)
    probabilities          = prob:probRest

printProbDistr2 :: [Expression] -> [[Substitution]] -> [(Variable, Expression)] -> [String]
printProbDistr2 probExpressions [] nextState     =  []
printProbDistr2 probExpressions (x:xs) nextState = ("(" ++ infixString newNextState " * " ++ ") : " ++ updateString):(printProbDistr2 probExpressions xs nextState)
  where
    newNextState = map expressionToPRISM (map (simplifyExpression ([],[],[])) (map (substituteInExpression x) probExpressions))
    updateString = infixString (map (\y -> "(" ++ fst y ++ "'" ++ " = " ++ expressionToPRISM (substituteInExpression x (snd y)) ++ ")") nextState) " & "

allCombinations []           = [[]]
allCombinations (list:lists) = [[x] ++ y | x <- list, y <- allCombinations lists]

unfoldProb3 :: DataSpec -> Expression -> Variable -> [String] -> [(Variable, Expression)] -> [(Expression, [(Variable, Expression)])]
unfoldProb3 dataspec prob var [] updates           = []
unfoldProb3 dataspec prob var (val:values) updates = (prob2, newUpdates):(unfoldProb3 dataspec prob var values updates)
  where
    newUpdates = zip (map fst updates) (map (\x -> substituteInExpression [(var, (Variable val))] (snd x)) updates)
    prob2 = simplifyExpression dataspec (substituteInExpression [(var, (Variable val))] prob)

printProbDistr [] = []
printProbDistr ((prob, updates):rest) = ("(" ++ expressionToPRISM prob ++ ") : " ++ updateString):(printProbDistr rest)
  where
    updateString = infixString (map (\x -> "(" ++ fst x ++ "'" ++ " = " ++ expressionToPRISM (snd x) ++ ")") updates) " & "


optionalUnderscore :: String -> String
optionalUnderscore ""   = ""
optionalUnderscore text = "_" ++ text 

-- This function takes a list of items and makes an underscore-separated list of it,
-- surrounded by underscores.
actionList :: [String] -> String
actionList items = optionalUnderscore (infixString items "_")


---------------------------

createUntilObserver :: DataSpec -> UntilFormula -> [(Action, ActionPars)] -> String
createUntilObserver dataspec untilformula actions
             = "\n\nmodule UntilObserver\n\n  untilState : [0..2] init 0;\n\n  "
             ++ printEnablingTransitions dataspec untilformula actions     
             ++ "\n\nendmodule"

printEnablingTransitions :: DataSpec -> UntilFormula -> [(Action, ActionPars)] -> String
printEnablingTransitions dataspec (UntilFormula before until) actions = result
  where
    afterActions     = [(a, simplifyExpression dataspec e) | (a,e) <- getActionExpressionActions until actions, simplifyExpression dataspec e /= Variable "F"]
    beforeActions    = getActionExpressionActions before actions
    beforeActions2   = [(a, simplifyExpression dataspec e) | (a,e) <- removeActions beforeActions afterActions, simplifyExpression dataspec e /= Variable "F"]
    remainingActions = [(a, simplifyExpression dataspec e) | (a,e) <- removeActions (removeActions [(a,Variable "T") | (a,p) <- actions] beforeActions) afterActions, simplifyExpression dataspec e /= Variable "F"]
    result       = infixString ["[" ++ fst a ++ "] (untilState = 0)" ++ (if snd a == Variable "T" then "" else (" & (" ++ expressionToPRISM (snd a)) ++ ")") ++ " -> (untilState' = 1);" | a <- afterActions] "\n  "
                ++ "\n  " ++ infixString ["[" ++ fst a ++ "] (untilState = 0)" ++ (if snd a == Variable "T" then "" else (" & (" ++ expressionToPRISM (snd a)) ++ ")") ++ " -> (untilState' = 0);" | a <- beforeActions2] "\n  "
                ++ "\n  " ++ infixString ["[" ++ fst a ++ "] (untilState = 0)" ++ (if snd a == Variable "T" then "" else (" & (" ++ expressionToPRISM (snd a)) ++ ")") ++ " -> (untilState' = 2);" | a <- remainingActions] "\n  "
   
data ParametersRequirement = Equal | Unequal  deriving (Show, Eq)

getActionExpressionActions :: ActionExpression -> [(Action, ActionPars)] -> [(Action, Expression)]
getActionExpressionActions (ActionName name pars) []                     = error("Error: Until formula refers to nonexisting action " ++ name)
getActionExpressionActions (ActionTrue) actions                          = [(name, Variable "T") | name <- map fst actions]
getActionExpressionActions (ActionName name pars) (a:as) | name == fst a && pars == []                    = [(name, Variable "T")]  
                                                         | name == fst a && length pars /= length (snd a) = error("Error: number of parameters of action " ++ name ++ " in until formula incorrect.")
	                                                     | name == fst a = [(name, if pars == [] then Variable "T" else expr)]
                                                         | otherwise     = getActionExpressionActions (ActionName name pars) as
  where
    expr = Function "and" [Function "eq" [(snd a)!!i, Variable (pars!!i)] | i <- [0..length pars - 1]]
getActionExpressionActions (ActionNot left) actions      = inverseActionExpressionActions actions (getActionExpressionActions left actions)
getActionExpressionActions (ActionOr left right) actions = mergeActions (getActionExpressionActions left actions) (getActionExpressionActions right actions)


inverseActionExpressionActions :: [(Action,ActionPars)] -> [(Action, Expression)] -> [(Action, Expression)]
inverseActionExpressionActions [] acts                                          = []
inverseActionExpressionActions ((a,pars):as) acts | not (elem a (map fst acts)) = (a,Variable "T"):(inverseActionExpressionActions as acts)
                                                  | elem a (map fst acts)       = (action, Function "not" [expr]):inverseActionExpressionActions as acts
  where
    (action,expr) = [act | act <- acts, fst act == a]!!0

mergeActions :: [(Action, Expression)] -> [(Action, Expression)] ->  [(Action, Expression)]
mergeActions [] second                    = second
mergeActions ((action, expr):rest) second = result:(mergeActions rest newSecond)
  where
    correspondings = [x | x <- second, fst x == action]
    (a,expr2)      = correspondings!!0
    newSecond      = if correspondings == [] then second else second \\ [(a,expr2)]
    result         = if correspondings == [] then (action,expr) else  (action, Function "or" [expr, expr2])

removeActions actions []                                                            = actions
removeActions actions (toRemove:rest) | elem toRemove actions                       = removeActions (actions \\ [toRemove]) rest
                                      | not (elem (fst toRemove) (map fst actions)) = removeActions actions rest
                                      | elem (fst toRemove) (map fst actions)       = removeActions newActions rest
  where
    (action,expr) = [act | act <- actions, fst act == fst toRemove]!!0
    newExpression = simplifyExpression ([],[],[]) (Function "and" [expr, Function "not" [snd toRemove]])
    actions2      = actions \\ [(action,expr)]
    newActions    = (action, newExpression):actions2 

getUntilActions (UntilFormula before after) = nub (getUntilActions2 before ++ getUntilActions2 after)

getUntilActions2 (ActionName name pars) = [name]
getUntilActions2 (ActionNot x)          = getUntilActions2 x
getUntilActions2 (ActionTrue)           = []
getUntilActions2 (ActionOr x y)         = nub (getUntilActions2 x ++ getUntilActions2 y)

uniqueActions summands [] = (summands, [])
uniqueActions summands (a:as) = (newSummands2, (a,i):updates) 
  where
    (newSummands,i)       = uniqueActions2 summands a 1
    (newSummands2,updates) = uniqueActions newSummands as

uniqueActions2 [] a i     = ([], i)
uniqueActions2 (s:ss) a i | getAction s == a = ((setAction s (a ++ "_" ++ show i)):summands, nextI)
                          | otherwise        = (s:summands2, nextI2)
  where
    (summands, nextI)   = uniqueActions2 ss a (i+1)
    (summands2, nextI2) = uniqueActions2 ss a i

updateUntilFormula (UntilFormula before after) updates = UntilFormula newBefore newAfter
  where
    newBefore = updateUntilFormula2 before updates
    newAfter = updateUntilFormula2 after updates

updateUntilFormula2 (ActionOr x y) updates      = ActionOr  (updateUntilFormula2 x updates) (updateUntilFormula2 y updates)
updateUntilFormula2 (ActionNot x) updates       = ActionNot (updateUntilFormula2 x updates)
updateUntilFormula2 (ActionTrue) updates        = ActionTrue
updateUntilFormula2 (ActionName x pars) updates | to == 1   = ActionName (x ++ "_1") pars
                                                | otherwise = makeOr x pars to
  where
    to = [i | (a,i) <- updates, a == x]!!0 - 1

makeOr x pars to | to == 2   = ActionOr (ActionName (x ++ "_1") pars) (ActionName (x ++ "_2") pars) 
                          | otherwise = ActionOr (ActionName (x ++ "_" ++ show to) pars) (makeOr x pars (to - 1))

dataToPRISM :: [DataType] -> DataSpec -> [PSummand] -> (DataSpec, [PSummand])
dataToPRISM [] (dataTypes, typeValues, functions) summands = ((dataTypes, typeValues, functions), summands)
dataToPRISM ((EnumInt name from to):dts) (dataTypes, typeValues, functions) summands = dataToPRISM dts (dataTypes, typeValues, functions) summands
dataToPRISM ((EnumString "Bool" els):dts) (dataTypes, typeValues, functions) summands = dataToPRISM dts (dataTypes, typeValues, functions) summands
dataToPRISM ((EnumString name elements):dts) (dataTypes, typeValues, functions) summands = dataToPRISM dts (dataTypes2, typeValues2, functions) summands2
  where
    (dataTypes2, typeValues2, summands2) = replaceEnum name (length elements) dataTypes typeValues summands

replaceEnum :: String -> Int -> [DataType] -> [TypeValues] -> [PSummand] -> ([DataType], [TypeValues], [PSummand])
replaceEnum name elements [] typeValues summands     = ([], typeValues, summands)
replaceEnum name elements ((EnumString n es):tvs) typeValues summands | n == name = ((EnumInt name 1 elements):tvs, newTypeValues, newSummands)
                                                                      | otherwise = ((EnumString n es):newDataTypes, newTypeValues2, newSummands2)
   where 
     (newDataTypes, newTypeValues2, newSummands2) = replaceEnum name elements tvs typeValues summands
     newSummands = map (replaceEnumInSummand es) summands
     newTypeValues = map (replaceEnumInTypeValue name) typeValues
replaceEnum name elements (t:tvs) typeValues summands                             = (t:newDataTypes, newTypeValues, newSummands)
  where
    (newDataTypes, newTypeValues, newSummands) = replaceEnum name elements tvs typeValues summands

replaceEnumInSummand :: [String] -> PSummand ->  PSummand
replaceEnumInSummand values summand = substituteInPSummand subs summand
  where
    subs = [(values!!(i-1), Variable (show i)) | i <- [1..length values]]

replaceEnumInTypeValue :: String -> TypeValues -> TypeValues
replaceEnumInTypeValue name (TypeName s, values) | s == name = (TypeName s, [show i | i <- [1..length values]])
                                                 | otherwise = (TypeName s, values) 
replaceEnumInTypeValue name other                            = other