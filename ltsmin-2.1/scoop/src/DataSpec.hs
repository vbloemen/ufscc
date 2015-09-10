module DataSpec where
	
import Auxiliary
import Data.Ratio
import Data.Char
import Debug.Trace

data DataType    = EnumInt String Int Int | EnumString String [String] deriving (Show, Eq) -- | NatType String | QueueType String deriving (Show, Eq)
data FunctionDef = FunctionDef String [([String], String)] deriving (Show, Eq)
data Type        = TypeName String | TypeRange Int Int | TypeRangeExpressions Expression Expression | NoType deriving (Show, Eq)

type TypeValues  = (Type, [String])

type DataSpec      = ([DataType], [TypeValues], [FunctionDef])

builtInTypes = [EnumString "Bool" ["T", "F"]]
builtInFunctions = []

correctValue :: DataSpec -> String -> Type -> Bool
correctValue dataspec value (TypeName "Nat")   = isInteger value
correctValue dataspec value (TypeName "Queue") = and [elem v (concat (map snd (snd3 dataspec))) || isInteger v | v <- split value ';'] || trace("Warning: queue " ++ show value ++ " has unexpected values.") True
correctValue dataspec value typ                = elem value (getValues dataspec typ)

allValues :: Type -> [DataType] -> [String]
allValues (TypeRange from to) _      = [show i | i <- [from..to]]
allValues (TypeName "Bool") _        = ["T", "F"]
allValues (TypeName "Queue") _       = ["empty"]
allValues (TypeName "Nat") _         = ["0"]
allValues (TypeName typeName) []     = error("Error: Undefined type " ++ typeName ++ " used.")
--allValues (TypeName typeName) ((NatType name):xs) | name == typeName = ["0"]
--                                               | otherwise        = allValues (TypeName typeName) xs
--allValues (TypeName typeName) ((QueueType name):xs) | name == typeName = ["empty"]
--                                               | otherwise        = allValues (TypeName typeName) xs
allValues (TypeName typeName) ((EnumInt name from to):xs) | name == typeName = [show i | i <- [from..to]]
                                               | otherwise        = allValues (TypeName typeName) xs
allValues (TypeName typeName) ((EnumString name values):xs) | name == typeName = values
                                                 | otherwise        = allValues (TypeName typeName) xs

getValues :: DataSpec -> Type -> [String]
getValues _ (TypeRange from to)                      = [show i | i <- [from..to]]
getValues (_,typeValues,_) (TypeName name)  | length valuesSet == 1 = valuesSet!!0 
                                            | otherwise             = error("Error: Values for type " ++ name ++ ": " ++ show valuesSet ++ show typeValues)
  where
    valuesSet = [values | (typ,values) <- typeValues, typ == TypeName name]

getTypeName :: DataType -> String
getTypeName (EnumInt name _ _)  = name
getTypeName (EnumString name _) = name

getTypeName2 :: Type -> String
getTypeName2 (TypeRange from to) = "{" ++ show from ++ ".." ++ show to ++ "}"
getTypeName2 (TypeName name)     = name

getFunctionName :: FunctionDef -> String
getFunctionName (FunctionDef name mappings) = name

printDataSpec :: DataSpec -> String
printDataSpec ([], _, []) = ""
printDataSpec ([], _, [FunctionDef name list])      = printFunction name list
printDataSpec ([],values, ((FunctionDef name list):fs)) = printFunction name list ++ "\n" ++ printDataSpec ([], values, fs)
printDataSpec ([typ], _, [])                        = printDataType typ
printDataSpec ([typ], values, fs)                        = printDataType typ ++ "\n" ++ printDataSpec ([], values, fs)
printDataSpec ((typ:types), values, fs)                  = printDataType typ ++ "\n" ++ printDataSpec (types, values, fs)

printFunction name list = "    function " ++ name ++ " = " ++ commaList (map printFunctionMapping list)

printFunctionMapping :: ([String], String) -> String
printFunctionMapping (from,to) = (infixString from " * ") ++ " -> " ++ to

printDataType (EnumInt name from to) = "    type " ++ name ++ " = {" ++ show from ++ ".." ++ show to ++ "}"
printDataType (EnumString name pars) = "    type " ++ name ++ " = " ++ commaList pars

stringToInt :: String -> Int
stringToInt s | isInteger s = parseInteger s
              | otherwise   = error("Error: Could not parse " ++ s ++ " as an integer. Maybe you made a typing mistake or forgot to define a constant?")

-- For easier debugging, re-enable the check for "and".
evaluateFunction :: String -> [String] -> [FunctionDef] -> String
evaluateFunction function arguments _  | function == "and"    = --if (and [x == "F" || x == "T" | x <- arguments])  then  
                                                                (if (elem "F" arguments) then "F" else "T") 
                                                                --   else error ("Error in condition: not all arguments are booleans in and(" ++ show arguments ++ ")")
                                       | function == "or"     = if (elem "T" arguments) then "T" else "F"
                                       | function == "count"  = show (length [x | x <- arguments, x == "T"])
                                       | function == "wcount" = show (computeWCount arguments)
evaluateFunction "ifthenelse" [i,t,e] _ = if (i == "T") then t else if (i == "F") then e else error("Error: cannot evaluate '" ++ i ++ "'") 
evaluateFunction "eq" [x,y] _     = if x == y || (DataSpec.isNumber x && DataSpec.isNumber y && (getFraction x == getFraction y)) then "T" else "F"
evaluateFunction "eq" (x:y:xs) functions = if (evaluateFunction "eq" [x,y] functions == "T" && (evaluateFunction "eq" (y:xs)) functions == "T") then "T" else "F"
evaluateFunction "not" ["F"] _  = "T"
evaluateFunction "not" ["T"] _   = "F"
evaluateFunction function [i, j] _     | function == "gt"        = if (getFraction i > getFraction j) then "T" else "F"
                                       | function == "geq"       = if (getFraction i >= getFraction j) then "T" else "F"
                                       | function == "leq"       = if (getFraction i <= getFraction j) then "T" else "F"
                                       | function == "lt"        = if (getFraction i < getFraction j) then "T" else "F"
                                       | function == "min"       = writeFraction (min (getFraction i) (getFraction j))
                                       | function == "max"       = writeFraction (max (getFraction i) (getFraction j))
                                       | function == "plus"      = writeFraction (getFraction i + getFraction j)
                                       | function == "power" && isInteger i && isInteger j = show ((parseInteger i) ^ (parseInteger j))
									   | function == "power"     = error("Error: Cannot compute powers of non-integers.")
                                       | function == "minus"     = writeFraction (getFraction i - getFraction j)
                                       | function == "multiply"  = writeFraction ((getFraction i) * (getFraction j))
                                       | function == "divide"    = writeFraction ((getFraction i) / (getFraction j))
                                       | function == "concat"    = i ++ j
                                       | function == "mod" && isInteger i && isInteger j = show (mod (parseInteger i) (parseInteger j))
                                       | function == "mod"                               = error("Error: Cannot do modulo computation with non-integers.")
                                       | function == "add" && i == "empty"        = j
                                       | function == "add"                        = i ++ ";" ++ j
                                       | function == "addToQueue" && i == "empty"        = j
                                       | function == "addToQueue"                        = i ++ ";" ++ j
                                       | function == "contains"                          = listContains i j
                                       | function == "get" && isInteger j                = getElementFromVector i (parseInteger j)
                                       | function == "remove" && isInteger j             = removeElementFromVector i (parseInteger j)
                                       | function == "removeElement"                     = removeItemFromVector i j
                                       | function == "get"                               = error ("Could not get(" ++ i ++ ", " ++ j ++ ")")
                                       | function == "remove"                            = error ("Could not remove(" ++ i ++ ", " ++ j ++ ")")
evaluateFunction "plus" arguments _  = writeFraction (sum [getFraction i | i <- arguments])
evaluateFunction "add"  [q,e,n] _    = addNtoQ (parseInteger n) e q 
evaluateFunction function     [q] _    | function == "head"  = takeWhile (/= ';') q
                                       | function == "tail"  = if elem ';' q then drop 1 (dropWhile (/= ';') q) else "empty"
                                       | function == "size"  = if q == "empty" then "0" else show (length [x | x <- q, x == ';'] + 1)
evaluateFunction "set"        [v,n,w] _    | isInteger n = setElementInVector v w (parseInteger n)
                                           | otherwise   = error ("Could not set(" ++ v ++ ", " ++ n ++ ", " ++ w ++ ")")
evaluateFunction functionName arguments [] = error("Error: Undefined function " ++ functionName ++ show arguments ++ " used.")
evaluateFunction functionName arguments ((FunctionDef name mapping):xs) | name == functionName = useMapping name mapping arguments
                                                                        | otherwise = evaluateFunction functionName arguments xs 

removeItemFromVector "empty" j                                = "empty"
removeItemFromVector list    j | not (elem ';' list)          = if j == list then "empty" else list
                               | takeWhile (/= ';') list == j = drop 1 (dropWhile (/= ';') list)
                               | otherwise                    = (takeWhile (/= ';') list) ++ rest2
  where
    rest = removeItemFromVector (drop 1 (dropWhile (/= ';') list)) j
    rest2 = if rest == "empty" then "" else ";" ++ rest

listContains "empty" j = "F"
listContains list    j | takeWhile (/= ';') list == j = "T"
                       | otherwise                    = if elem ';' list then listContains (drop 1 (dropWhile (/= ';') list)) j else "F"
	
addNtoQ :: Int -> String -> String -> String
addNtoQ 0 i j       = j
addNtoQ n i "empty" = addNtoQ (n-1) i (i)
addNtoQ n i j       = addNtoQ (n-1) i (i ++ ";" ++ j)

computeWCount []         = 0
computeWCount [x]        = error("Odd number of arguments in wcount function")
computeWCount (x:y:rest) = (if x == "T" then parseInteger y else 0) + computeWCount rest

removeElementFromVector ""      n = error("Index out of bounds error in function remove")
removeElementFromVector "empty" n = error("Index out of bounds error in function remove")
removeElementFromVector v       n | n == 0 && not (elem ';' v) = "empty"
                                  | otherwise        = drop 1 (removeElementFromVector2 (';':v) n)


removeElementFromVector2 "" n = error("Index out of bounds error in function remove")
removeElementFromVector2 v 0  = (dropWhile (/= ';') (drop 1 v))
removeElementFromVector2 v n  = ";" ++ takeWhile (/= ';') (drop 1 v) ++ (removeElementFromVector2 (dropWhile (/= ';') (drop 1 v)) (n-1))

getElementFromVector "empty" 0 = "INDEX OUT OF BOUNDS IN FUNCTION GET"
getElementFromVector "" n = "INDEX OUT OF BOUNDS IN FUNCTION GET"
getElementFromVector v 0  = takeWhile (/= ';') v 
getElementFromVector v n  = getElementFromVector (drop 1 (dropWhile (/= ';') v)) (n-1)

setElementInVector v  w 0 = w ++ dropWhile (/= ';') v
setElementInVector v  w n = takeWhile (/= ';') v ++ ";" ++ (setElementInVector (drop 1 (dropWhile (/= ';') v)) w (n-1))
 	
useMapping :: String -> [([String], String)] -> [String] -> String
useMapping fName [] arguments = error("Error: Function " ++ fName ++ " applied to constants " ++ show arguments ++ ", without having defined the result")
useMapping fName ((from, to):xs) arguments | from == arguments = to
                                     | otherwise         = useMapping fName xs arguments

allTypes :: [DataType] -> [Type]
allTypes [] = []
allTypes ((EnumInt name from to):xs)   = (TypeName name):(allTypes xs)
allTypes ((EnumString name values):xs) = (TypeName name):(allTypes xs)

-- This function checks whether a string is an element of one of the datatypes.
stringIsConstant :: DataSpec -> String -> Bool
stringIsConstant dataspec constant | elem ';' constant = and [stringIsConstant dataspec c | c <- split constant ';']
stringIsConstant dataspec constant = DataSpec.isNumber constant || elem constant (concat [getValues dataspec t | t <- allTypes (fst3 dataspec)])

isInteger :: String -> Bool
isInteger i   = i == "0" || i == "1" || i == "1.0" || i == "2" || i == "3" || i == "4" || i == "5" || i == "6" || i == "7" || i == "8" || i == "9" || i == "10" || isInt
  where
    nr        = takeWhile isDigit (if (head i == '-') then tail i else i)  
    isInt     = (length nr > 0) && (nr == i || ('-':nr) == i || nr ++ ".0" == i || ('-':nr) ++ ".0" == i || 
                                    nr ++ "/1" == i || ('-':nr)++"/1" == i || nr ++ ".0/1" == i || ('-':nr) ++ ".0/1" == i)

parseInteger :: String -> Int
parseInteger i | i == "0"  = 0
               | i == "1"  = 1
               | i == "2"  = 2
               | i == "3"  = 3
               | i == "4"  = 4
               | i == "5"  = 5
               | i == "6"  = 6
               | i == "7"  = 7
               | i == "8"  = 8
               | i == "9"  = 9
               | i == "10"  = 10
               | otherwise = read (takeWhile (\x -> x /= '.' && x /= '/') i)

isReal :: String -> Bool
isReal i = correct
  where
    beforePoint = takeWhile isDigit (if (head i == '-') then tail i else i)  
    afterPoint  = takeWhile isDigit (drop 1 (dropWhile (/= '.') i))
    correct     = length beforePoint > 0 && length afterPoint > 0 && (beforePoint ++ "." ++ afterPoint == i ||
                                             "-" ++ beforePoint ++ "." ++ afterPoint == i)

parseReal :: String -> Ratio Int
parseReal i = fraction
  where
    beforePoint = takeWhile (/= '.') i  
    afterPoint  = drop 1 (dropWhile (/= '.') i)
    number      = read (beforePoint ++ afterPoint)
    fraction    = number % 10^(length afterPoint)

isFraction :: String -> Bool
isFraction i = correct
  where
    beforeDash = takeWhile isDigit (if (head i == '-') then tail i else i)  
    afterDash  = takeWhile isDigit (drop 1 (dropWhile (/= '/') i))
    correct    = length beforeDash > 0 && length afterDash > 0 && (beforeDash ++ "/" ++ afterDash == i ||
	                                             "-" ++ beforeDash ++ "/" ++ afterDash == i)

parseFraction :: String -> Ratio Int
parseFraction i = (read beforeDash) % (read afterDash)
  where
    beforeDash = takeWhile (/= '/') i  
    afterDash  = drop 1 (dropWhile (/= '/') i)

writeFraction :: Ratio Int -> String
writeFraction i | denominator i == 1 = show (numerator i)
	            | otherwise          = show (numerator i) ++ "/" ++ show (denominator i)

getFraction :: String -> Ratio Int
getFraction frac | frac == "1"     = 1 % 1
                 | frac == "2"     = 2 % 1
                 | frac == "3"     = 3 % 1
                 | frac == "4"     = 4 % 1
                 | frac == "5"     = 5 % 1
                 | frac == "6"     = 6 % 1
                 | frac == "7"     = 7 % 1
                 | frac == "8"     = 8 % 1
                 | frac == "9"     = 9 % 1
                 | isInteger  frac = parseInteger  frac % 1
                 | isReal     frac = parseReal     frac
                 | isFraction frac = parseFraction frac
                 | otherwise       = error("Error: Cannot parse " ++ frac ++ " as a number.")

isNumber :: String -> Bool
isNumber i = i == "1" || isInteger i || isReal i || isFraction i