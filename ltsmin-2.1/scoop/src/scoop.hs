module Main where

import Auxiliary
import InputParser
import LPPE
import MLPPE
import ToPA
import ToAUT
import StepPA
import ToTrans
import Expressions
import ToPRISM
import System.IO
import System.Environment (getArgs)
import Simplify
import Confluence
import Sumelm
import Constelm
import Parelm
import MaximalProgress
import DeadVariable
import Control.Monad
import Data.HashMap
import Data.List

main = do
  args <- getArgs
  input <- hGetContents stdin
  hPutStrLn stderr "SCOOP (last changed on 13 October 2014)"
  -- Parsing the command-line parameters.
  let flags      = ["-conf", "-dead", "-mlppe", "-keeprates", "-maxprogress", "-divergence", "-ma", "-pa", "-mapa", "-imca",
                    "-checkuntil", "-cadp", "-verbose", "-trans", "-lppe", "-size", "-prism", "-aut", "-visited", "-noprob",
                    "-ignorecycles", "-statespace", "-io", "-prcrl", "-deadlocks", "-store", "-parelm", "-exact", "-removecycles", "-PAstyle", "-strong", "-dtmc", "-nobasics", "-shared"]

  let ma                   = elem "-ma" args     
  let pa                   = elem "-pa" args
  let confluence           = elem "-conf" args                -- only for PA
  let dead                 = elem "-dead" args
  let verbose              = elem "-verbose" args
  let printLPPE            = (elem "-mlppe" args || elem "-lppe" args) && pa
  let printMLPPE           = (elem "-mlppe" args || elem "-lppe" args) && ma
  let prcrl                = (elem "-prcrl" args || elem "-mapa" args || elem "-exact" args) && pa
  let mapa                 = (elem "-prcrl" args || elem "-mapa" args || elem "-exact" args) && ma
  let stateSpaceSize       = elem "-size" args
  let prism                = elem "-prism" args               -- only for PA
  let noprobs              = elem "-noprob" args
  let sharedActions        = elem "-shared" args
  let ioActions            = elem "-io" args
  let preserveDivergence   = elem "-divergence" args || ma    -- only in PA mode this can be disabled
  let visited              = elem "-visited" args             -- only for PA
  let printAUTfile         = elem "-aut" args
  let printIMCfile         = elem "-imca" args
  let storeReps            = elem "-store" args 
  let keepRates            = elem "-keeprates" args           -- do not apply maximal progress when generating MA
  let dtmc                 = elem "-dtmc" args                -- only for PA
  let cadp                 = elem "-cadp" args            
  let states               = elem "-statespace" args    
  let checkuntil           = elem "-checkuntil" args          -- only for PA
  let trans                = elem "-trans" args               -- only for PA
  let maxprogress          = elem "-maxprogress" args         -- only for MA
  let includingParelm      = elem "-parelm" args          
  let removecycles         = elem "-removecycles" args        -- only for PA
  let ignorecycles         = elem "-ignorecycles" args        -- only for PA
  let pastyle              = elem "-PAstyle" args || prcrl    -- do not use x := 5 notation in next state when pretty-printing (M)LPPE
  let strong               = elem "-strong" args              -- only for PA
  let nobasics             = elem "-nobasics" args
  let deadlocks            = elem "-deadlocks" args || states
  let wrongFlags           = [x | x <- args, not(elem x flags || take 8 x == "-source=" || take 10 x == "-constant-")]
  let sourcefiles          = [x | ('-':'s':'o':'u':'r':'c':'e':'=':x) <- args]
  let constantdefs         = [x | ('-':'c':'o':'n':'s':'t':'a':'n':'t':'-':x) <- args]

  when (length wrongFlags > 0)          $ putStrLn ("Warning: flags " ++ show wrongFlags ++ " not recognised.")
  when (pa && ma)                       $ error ("Error: Choose either -pa or -ma.")
  when (printIMCfile && noprobs)        $ error ("Error: -noprobs not allowed in combination with -imca.")
  when (not (pa || ma))                 $ error ("Error: Choose either -pa or -ma.")

  let paFlags                           = [ "-prism", "-dtmc", "checkuntil", "-trans", "-ignorecycles"]
  let maFlags                           = []

  let maErrors                          = [x | x <- args, elem x paFlags]
  let paErrors                          = [x | x <- args, elem x maFlags]

  when (ma && length maErrors > 0)      $ error ("Flags " ++ show maErrors ++ " not allowed in combination with -ma")
  when (pa && length paErrors > 0)      $ error ("Flags " ++ show paErrors ++ " not allowed in combination with -pa")

  -- Parsing the constants.
  let constants               = [(takeWhile (/= '=') constant, Variable (drop 1 (dropWhile (/= '=') constant))) | constant <- constantdefs]
  
  -- Outputting information about the procedure.
  when (verbose)                              $ putStrLn ("In " ++ (if ma then "MAPA " else "prCRL ") ++ "mode.\n") 
  when (verbose && (length sourcefiles) == 1) $ putStrLn ("Specification that was analysed: " ++ (sourcefiles!!0))
  when (verbose && pa) $ putStrLn ("Confluence reduction: " ++ show confluence)
  when (verbose)       $ putStrLn ("Basic reductions: " ++ show (not nobasics))
  when (verbose)       $ putStrLn ("Dead variable reduction: " ++ show dead ++ "\n")

  -- Parsing the input, and doing all the relevant transformations for prCRL mode.
  let (basicSpec, actiontypes, untilformula,reach,stateRewards) = (parseInput ma sharedActions ioActions False False constants) input
  let reachNames                                   = [takeWhile (/= '(') r | r <- reach]
  let standardTransformations                      = if nobasics then id else simplify . sumelm . simplify . constelm . sumelm
  let transformations                              = standardTransformations . 
                                                     (if dead                 then (constelm . (deadVariableReduction verbose))
	                                                 else if includingParelm then (constelm . parelm) 
		                                             else id) . 
		                                             standardTransformations
  let specification                                = transformations basicSpec
  let lppe                                         = getLPPE specification
 
  -- Showing the LPPE and the number of parameters / summands.
  when (pa && prcrl)                   $ print (LPPEShowPRCRL specification)
  when (pa && printLPPE && not(prcrl)) $ print (LPPEShow specification pastyle) 
  when (pa && (printLPPE || prcrl) && verbose)    $ putStrLn ("Number of parameters: " ++ show (length (getLPPEPars lppe)))
  when (pa && (printLPPE || prcrl) && verbose)    $ putStrLn ("Number of summands:   " ++ show (length (getPSummands lppe)))

  -- Detecting confluence.
  let confluents_                    = if confluence then getConfluentSummands specification strong reach else []
  let confluentSummands              = if not(removecycles) then confluents_ else removeConfluentCycles specification confluents_ 
  when (pa && confluence && verbose) $ putStrLn ("Confluent summands:   " ++ show confluentSummands)

  -- Computing the actual state space.
  let statespacesize = getStatesAndTransitionsConfluence ignorecycles specification confluentSummands confluence True dtmc False False preserveDivergence deadlocks storeReps reachNames

  -- Showing information about the state space.
  when (pa && stateSpaceSize) $ putStrLn ("Number of states: " ++ show (fst (fst statespacesize)))
  when (pa && stateSpaceSize) $ putStrLn ("Number of transitions: " ++ show (snd (fst statespacesize)))
  when (pa && visited)        $ putStrLn ("Number of states visited: " ++ show (fst (snd statespacesize)))
  when (pa && visited)        $ putStrLn ("Number of transitions visited: " ++ show (snd (snd statespacesize)))

  -- Printing the state space to an AUT file (either probabilistically or non-probabilistically).
  when (pa && printAUTfile && dtmc)                        $ print (toAUTDTMC    ignorecycles specification cadp confluentSummands confluence True False False preserveDivergence deadlocks storeReps reachNames)
  when (pa && printAUTfile && noprobs && not(dtmc))        $ print (toAUTNonProb ignorecycles specification cadp confluentSummands confluence dtmc False False preserveDivergence deadlocks storeReps reachNames)
  when (pa && printAUTfile && (not(noprobs)) && not(dtmc)) $ print (toAUTProb    ignorecycles specification cadp confluentSummands confluence dtmc False False preserveDivergence deadlocks storeReps reachNames)

  -- Printing the state space to PRISM's trans format.
  when (pa && trans)               $ putStrLn (toTrans ignorecycles specification confluentSummands confluence dtmc False False preserveDivergence deadlocks storeReps reachNames)

  -- Printing all the actual states and transitions.
  let ((statesX,transitionsX),initialX,_) = getStateSpace ignorecycles specification confluentSummands confluence False dtmc False False preserveDivergence deadlocks storeReps reachNames
  let statesOrderedX = (Prelude.map snd . sort . Prelude.map (\x -> (snd x, fst x)) . toList) statesX
  when (pa && states && length statesOrderedX /= -1) $ putStrLn ("\nInitial state:\n" ++ show initialX ++ "\n")
  when (pa && states) $ putStrLn ("States:")
  when (pa && states) $ printStates statesOrderedX 0
  when (pa && states) $ do { putStrLn "Transitions:" ; Main.printTransitions (sort transitionsX) }



  -- Outputting to PRISM.
  let specificationPRISM = if confluence then doSyntacticConfluence specification confluentSummands else specification
  when (pa && prism) $ print (toPRISM actiontypes checkuntil untilformula specificationPRISM)

  -- when prism $ print (toPRISM actiontypes checkuntil untilformula specificationPRISM)
  -- when markov $ print (getCTMC mspecification)
  -- when toAUTfileMA $ print (toAUTMA ignorecycles specification cadp confluents)
 
  -- Dealing with MAPA
  let basicSpecM               = decode basicSpec
  let standardTransformationsM = if nobasics then id else decode . simplify . encode . sumelmM . decode . simplify . constelm . encode . sumelmM
  let transformationsM         = standardTransformationsM . 
                                 (if dead then (decode . constelm . (deadVariableReduction verbose) . encode) else 
                                  if includingParelm then decode . parelm . encode else id) . 
                                 (if maxprogress then removeUnnecessaryRates else id) . 
                                  standardTransformationsM
  let specificationM           = transformationsM basicSpecM

  let confluentsM_                    = if confluence then getConfluentSummands (encode specificationM) strong reach else []
  let confluentSummandsM              = if not(removecycles) then confluentsM_ else removeConfluentCycles (encode specificationM) confluentsM_ 
  when (ma && confluence && verbose) $ putStrLn ("Confluent summands:   " ++ show confluentSummandsM)

  when (ma && verbose)                 $ putStrLn ("Number of parameters: " ++ show (getNrParamsM specificationM))
  when (ma && verbose)                 $ putStrLn ("MLPPE size: " ++ show (getMLPPESize specificationM))
  let stateRewardsString = infixString (Data.List.map (\x -> "- Reward " ++ (show (snd x)) ++ " for every state satistying " ++ (show (fst x))) stateRewards) "\n"
  when (ma && verbose)                 $ putStrLn ("State rewards: " ++ if stateRewardsString == "" then "none" else "\n" ++ stateRewardsString ++ "\n")

--  putStrLn ("Reward in state [0]: " ++ (show (computeReward specification ["0","0"] stateRewards)))

  when (ma && (getNrParamsM specificationM) >= 0 && False) $ putStrLn "Blaat"
  when (pa && (getNrParams specification) >= 0 && False) $ putStrLn "Blaat"


  when (ma && mapa)                    $ print (MLPPEShowMAPA specificationM)
  when (ma && printMLPPE && not(mapa)) $ print (MLPPEShow specificationM pastyle) 

  let removeRates     = not keepRates
  let statespacesizeM = getStatesAndTransitionsConfluence ignorecycles (encode specificationM) confluentSummandsM confluence True dtmc True removeRates preserveDivergence deadlocks storeReps reachNames

  when (ma && stateSpaceSize) $ putStrLn ("Number of states: " ++ show (fst (fst statespacesizeM)))
  when (ma && stateSpaceSize) $ putStrLn ("Number of transitions: " ++ show (snd (fst statespacesizeM)))
  when (ma && visited)        $ putStrLn ("Number of states visited: " ++ show (fst (snd statespacesizeM)))
  when (ma && visited)        $ putStrLn ("Number of transitions visited: " ++ show (snd (snd statespacesizeM)))

  when (ma && printAUTfile && noprobs)        $ print (toAUTNonProb ignorecycles (encode specificationM) cadp confluentSummandsM confluence False True removeRates preserveDivergence deadlocks storeReps reachNames)
  when (ma && printAUTfile && (not(noprobs))) $ print (toAUTProb    ignorecycles (encode specificationM) cadp confluentSummandsM confluence False True removeRates preserveDivergence deadlocks storeReps reachNames)

  when (ma && printIMCfile && (not(noprobs))) $ print (toIMCA  reach  ignorecycles (encode specificationM) cadp confluentSummandsM confluence False True removeRates preserveDivergence deadlocks storeReps reachNames)
  when (pa && printIMCfile && (not(noprobs))) $ print (toIMCA  reach  ignorecycles specification cadp confluentSummandsM confluence False True removeRates preserveDivergence deadlocks storeReps reachNames)

  let ((statesY,transitionsY),initialY,_) = getStateSpace ignorecycles (encode specificationM) confluentSummandsM confluence False False True removeRates preserveDivergence deadlocks storeReps reachNames
  let statesOrderedY = (Prelude.map snd . sort . Prelude.map (\x -> (snd x, fst x)) . toList) statesY
  when (ma && states && length statesOrderedY /= -1) $ putStrLn ("\nInitial state:\n" ++ show initialY ++ "\n")

  when (ma && states) $ putStrLn ("States:")
  when (ma && states) $ printStates statesOrderedY 0
  when (ma && states) $ do { putStrLn "Transitions:" ; Main.printTransitions (sort transitionsY) }

  when (pa && deadlocks && not(states) && not(trans) && not(printAUTfile) && not(printIMCfile) && size statesX /= -1 && False) $ putStrLn "Blaat"
  when (ma && deadlocks && not(states) && not(trans) && not(printAUTfile) && not(printIMCfile) && size statesY /= -1 && False) $ putStrLn "Blaat"

printStates [] i = putStrLn "\n"
printStates (x:xs) i = do { putStrLn ("State " ++ show i ++ ": " ++ show x) ; printStates xs (i+1)}

printTransitions [] = putStrLn "\n"
printTransitions ((from, reward, label, distr):xs) = do {putStrLn (show from ++ " -- " ++ label ++ "@(" ++ reward ++ ") --> {" ++ printDistribution distr) ; Main.printTransitions xs}

printDistribution [(prob,to)] = prob ++ " -> " ++ show to ++ "}"
printDistribution ((prob,to):xs) = prob ++ " -> " ++ show to ++ ", " ++ printDistribution xs

