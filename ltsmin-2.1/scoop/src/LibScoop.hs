{-# LANGUAGE ForeignFunctionInterface #-}

module LibScoop where

import Data.Int
import Data.List
import Data.Array.Storable -- this will have to be Data.Array.Unsafe in future releases.
#if __GLASGOW_HASKELL__>=708
import Data.Array.Unsafe
#endif

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr

import Auxiliary
import DataSpec
import InputParser
import LPPE
import MLPPE
import StepPA
import Expressions
import System.IO
import System.Environment (getArgs)
import Simplify
import Confluence
import Sumelm
import Constelm
import Parelm
import DeadVariable
import Control.Monad
import Usage

type ScoopSpec = (PSpecification,[String],[(Expression, Expression)])

load_prcrl :: CString -> StablePtr [(String,String)] -> IO (StablePtr ScoopSpec)
load_prcrl cname cconst = do load_common cname cconst False

load_mapa :: CString -> StablePtr [(String,String)] -> IO (StablePtr ScoopSpec)
load_mapa cname cconst = do load_common cname cconst True


load_common :: CString -> StablePtr [(String,String)] -> Bool -> IO (StablePtr ScoopSpec)
load_common cname cconst mapa = do
  name <- peekCString cname
  inFile <- openFile name ReadMode
  input <- hGetContents inFile
  const <- deRefStablePtr cconst
  let constants = [(var,Variable val)|(var,val)<-const]
  let (basicSpec, actiontypes, untilformula,reach,state_rewards) = parseInput mapa False False False False constants input
--  putStrLn (show reach)
--  putStrLn (show (getDataSpec basicSpec))
  putStrLn (show state_rewards)
  sequence_
    [ do withCString action (\cs -> report_reach cs)
    | action <- reach ]
  let _sumelm = if mapa then encode . sumelmM . decode else sumelm
  let transformations = simplify . _sumelm . simplify . constelm . _sumelm
  let !specification                                = transformations basicSpec
--  let confluents = getConfluentSummands specification False reach
--  putStrLn (show confluents)
--  let confluents_ = getConfluentSummands specification True []
--  putStrLn (show confluents_)
  hClose inFile
  newStablePtr (specification,reach,state_rewards)

const_empty = newStablePtr []

const_put old var val = do
    _old <- deRefStablePtr old
    freeStablePtr old
    _var <- peekCString var
    _val <- peekCString val
    let new=(_var,_val):_old
    newStablePtr new

get_confluent_summands spec = do
  (specification,reach,_) <- deRefStablePtr spec
  let confluents = getConfluentSummands specification False reach
--  putStrLn (show confluents)
  newStablePtr confluents

get_diamond_summands spec = do
  (specification,reach,_) <- deRefStablePtr spec
  let confluents = getConfluentSummands specification False []
--  putStrLn (show confluents)
  newStablePtr confluents

empty_conf lst = do
  ll <- deRefStablePtr lst
  return (fromIntegral (if ll==[] then 1 else 0))

head_conf lst = do
  ll <- deRefStablePtr lst
  return (fromIntegral (head ll))

tail_conf lst = do
  ll <- deRefStablePtr lst
  freeStablePtr lst
  newStablePtr (tail ll)


foreign export ccall prcrl_get_state_reward :: StablePtr ScoopSpec -> (Ptr Int32) -> (Ptr Int32) -> IO ()

foreign export ccall get_confluent_summands :: StablePtr ScoopSpec -> IO (StablePtr [Int])

foreign export ccall get_diamond_summands :: StablePtr ScoopSpec -> IO (StablePtr [Int])

foreign export ccall empty_conf :: StablePtr [Int] -> IO CInt

foreign export ccall head_conf :: StablePtr [Int] -> IO CInt

foreign export ccall tail_conf :: StablePtr [Int] -> IO (StablePtr [Int])

foreign export ccall const_empty :: IO (StablePtr [(String,String)])

foreign export ccall const_put :: StablePtr [(String,String)] -> CString -> CString -> IO (StablePtr [(String,String)])

foreign export ccall load_prcrl :: CString -> StablePtr [(String,String)] -> IO (StablePtr ScoopSpec)

foreign export ccall load_mapa :: CString -> StablePtr [(String,String)] -> IO (StablePtr ScoopSpec)

foreign export ccall prcrl_get_action :: StablePtr ScoopSpec -> CInt -> IO CString

foreign export ccall print_prcrl :: StablePtr ScoopSpec -> IO ()

foreign export ccall prcrl_pars :: StablePtr ScoopSpec -> IO CInt

foreign export ccall prcrl_rewards :: StablePtr ScoopSpec -> IO CInt

foreign export ccall prcrl_summands :: StablePtr ScoopSpec -> IO CInt

foreign export ccall prcrl_is_used :: StablePtr ScoopSpec -> CInt -> CInt -> IO CInt

foreign export ccall prcrl_par_name :: StablePtr ScoopSpec -> CInt -> IO CString

foreign export ccall prcrl_par_type :: StablePtr ScoopSpec -> CInt -> IO CString

foreign export ccall prcrl_get_init :: StablePtr ScoopSpec -> (Ptr Int32) -> IO ()

foreign export ccall prcrl_explore :: StablePtr ScoopSpec -> (Ptr Int32) -> (Ptr Int32) -> (Ptr Int32) -> IO CInt

foreign export ccall prcrl_explore_long :: StablePtr ScoopSpec -> CInt -> (Ptr Int32) -> (Ptr Int32) -> (Ptr Int32) -> IO CInt

foreign import ccall report_reach :: CString -> IO ()

foreign import ccall action_get_index :: CString -> IO CInt

foreign import ccall term_get_index :: CInt -> CString -> IO CInt

foreign import ccall term_get_value :: CInt -> CInt -> IO CString

foreign import ccall prcrl_callback :: IO ()

foreign import ccall get_sequence_no :: IO CInt

foreign import ccall write_prob_label :: CString -> (Ptr Int32) -> IO ()

foreign import ccall write_rate_label :: CString -> (Ptr Int32) -> IO ()

foreign import ccall write_reward_label :: CString -> (Ptr Int32) -> IO ()


prcrl_get_state_reward :: StablePtr ScoopSpec -> (Ptr Int32) -> (Ptr Int32) -> IO ()
prcrl_get_state_reward sptr src lbl = do
  (p_spec,_,rewards) <- deRefStablePtr sptr
  let init = getInitialState p_spec
  src_ptr <- newForeignPtr_ src
  lbl_ptr <- newForeignPtr_ lbl
  src_arr <- unsafeForeignPtrToStorableArray src_ptr (0,(length init)-1) :: IO (StorableArray Int Int32)
  lbl_arr <- unsafeForeignPtrToStorableArray lbl_ptr (0,1) :: IO (StorableArray Int Int32)
  src_list <- sequence
            [do tmp <- readArray src_arr i
                cs <- term_get_value (fromIntegral i) (fromIntegral tmp)
                peekCString cs
            | i <- [0 .. (length init)-1 ]]
  let reward = computeReward p_spec src_list rewards
  writeArray lbl_arr 0 (fromIntegral reward)
  writeArray lbl_arr 1 1

prcrl_explore :: StablePtr ScoopSpec -> (Ptr Int32) -> (Ptr Int32) -> (Ptr Int32) -> IO CInt
prcrl_explore sptr src dst lbl = do
  (p_spec,_,_) <- deRefStablePtr sptr
  prcrl_explore_smds p_spec (getPSummands (getLPPE p_spec)) src dst lbl

prcrl_explore_long :: StablePtr ScoopSpec -> CInt -> (Ptr Int32) -> (Ptr Int32) -> (Ptr Int32) -> IO CInt
prcrl_explore_long sptr smd src dst lbl = do
  (p_spec,_,_) <- deRefStablePtr sptr
  prcrl_explore_smds p_spec [getPSummand (getLPPE p_spec) (fromIntegral smd)] src dst lbl

prcrl_explore_smds :: PSpecification -> [PSummand] -> (Ptr Int32) -> (Ptr Int32) -> (Ptr Int32) -> IO CInt
prcrl_explore_smds p_spec smds src dst c_lbl = do
  let lppe = getLPPE p_spec
  let init = getInitialState p_spec
  src_ptr <- newForeignPtr_ src
  dst_ptr <- newForeignPtr_ dst
  lbl_ptr <- newForeignPtr_ c_lbl
  src_arr <- unsafeForeignPtrToStorableArray src_ptr (0,(length init)-1) :: IO (StorableArray Int Int32)
  dst_arr <- unsafeForeignPtrToStorableArray dst_ptr (0,(length init)-1) :: IO (StorableArray Int Int32)
  lbl_arr <- unsafeForeignPtrToStorableArray lbl_ptr (0,6) :: IO (StorableArray Int Int32)
  src_list <- sequence
            [do tmp <- readArray src_arr i
                cs <- term_get_value (fromIntegral i) (fromIntegral tmp)
                peekCString cs
            | i <- [0 .. (length init)-1 ]]
  let next = do_next_smds p_spec src_list smds
  sequence_
    [ do let rate=(take 4 lbl)=="rate"
         act <- if rate
           then withCString "rate" (\cs -> action_get_index cs)
           -- rename reach to tau to avoid reach* action in type.
           else if (take 5 lbl)=="reach" then withCString "tau" (\cs -> action_get_index cs)
           else withCString lbl (\cs -> action_get_index cs)
         if rate then
            writeArray lbl_arr 6 2
         else if lbl=="tau" then
            writeArray lbl_arr 6 0
         else if lbl=="reachConditionAction" then
            writeArray lbl_arr 6 3
         else if lbl=="stateRewardAction" then
            writeArray lbl_arr 6 4
         else
            writeArray lbl_arr 6 1
         writeArray lbl_arr 2 (fromIntegral act)
         seqno <- get_sequence_no
         writeArray lbl_arr 3 (fromIntegral seqno)
         sequence_
           [ do if rate
                    then withCString lbl (\x -> write_rate_label x c_lbl)
                    else withCString p (\x -> write_prob_label x c_lbl)
                withCString rew (\x -> write_reward_label x c_lbl)
                make_call_back init dst_arr dst_list
           | (p,dst_list) <- edge_list ]
    | (_,rew,lbl,edge_list) <- next ]
  return (fromIntegral (length next))
    where
      make_call_back init dst_arr dst_list = do
        sequence_
          [ do tmp <- withCString (dst_list !! i) (\cs -> term_get_index (fromIntegral i) cs)
               writeArray dst_arr i (fromIntegral tmp)
          | i <- [0 .. (length init)-1 ]]
        prcrl_callback


prcrl_get_init :: StablePtr ScoopSpec -> (Ptr Int32) -> IO ()
prcrl_get_init sptr ptr = do
  (p_spec,_,_) <- deRefStablePtr sptr
  let lppe = getLPPE p_spec
  let init = getInitialState p_spec
  fptr <- newForeignPtr_ ptr
  arr <- unsafeForeignPtrToStorableArray fptr (0,(length init)-1) :: IO (StorableArray Int Int32)
--  writeArray arr 1 ([1,2,3,4] !! 1)
--  cs <- newCString (head init)
--  writeArray arr 0 (fromIntegral (term_get_index cs))
  sequence_ [do tmp <- withCString (init !! i) (\cs -> term_get_index (fromIntegral i) cs)
                writeArray arr i (fromIntegral tmp)
            | i <- [0 .. (length init)-1 ]]

--  putStrLn (show init)
--  arr <- unsafeForeignPtrToStorableArray fptr (1,10) :: IO (StorableArray Int Int)
--  writeArray arr 1 64

prcrl_is_used sptr smd idx = do
    (p_spec,_,_) <- deRefStablePtr sptr
    let lppe = getLPPE p_spec
    let pars = getLPPEPars lppe
    let (v,_) =  pars !! (fromIntegral idx)
    let smds = getPSummands lppe
    let used = isUsedInSummand (smds !! (fromIntegral smd)) (fromIntegral idx) v True
    let changed = isChangedInSummand lppe (fromIntegral smd) (fromIntegral idx)
    return (if (used || changed) then 1 else 0)

prcrl_par_name sptr idx = do
  (p_spec,_,_) <- deRefStablePtr sptr
  let lppe = getLPPE p_spec
  let pars = getLPPEPars lppe
  let (v,t) =  pars !! (fromIntegral idx)
--  putStrLn ("par "++ (show idx)++": " ++ v)
  newCString v

prcrl_get_action sptr smd = do
  (p_spec,_,_) <- deRefStablePtr sptr
  let lppe = getLPPE p_spec
  let smds = getPSummands lppe
  let v = getAction (smds !! (fromIntegral smd))
  newCString v

prcrl_par_type sptr idx = do
  (p_spec,_,_) <- deRefStablePtr sptr
  let lppe = getLPPE p_spec
  let pars = getLPPEPars lppe
  let (v,t) =  pars !! (fromIntegral idx)
--  putStrLn (v ++ ": " ++ (show t))
  let typename = f v t
  newCString (typename)
 where
   f v (TypeName x) = x
   f v _ = v ++ "_t"

prcrl_pars :: StablePtr ScoopSpec -> IO CInt
prcrl_pars sptr = do
  (p_spec,_,_) <- deRefStablePtr sptr
  let lppe = getLPPE p_spec
  let pars = getLPPEPars lppe
  let init = getInitialState p_spec
--  putStrLn (show init)
  return (fromIntegral (length pars))

prcrl_rewards :: StablePtr ScoopSpec -> IO CInt
prcrl_rewards sptr = do
  (_,_,rewards) <- deRefStablePtr sptr
  return (fromIntegral (length rewards))

prcrl_summands sptr = do
  (p_spec,_,_) <- deRefStablePtr sptr
  let lppe = getLPPE p_spec
  let smds = getPSummands lppe
  return (fromIntegral (length smds))

do_next_smds p_spec state smds = potentialBehaviour False p_spec [] (getDataSpec p_spec) [] (createmap p_spec state) smds

createmap p_spec state = zip (Data.List.map fst params) state
  where
    params = getLPPEPars (getLPPE p_spec)

print_prcrl :: StablePtr ScoopSpec -> IO ()
print_prcrl sptr = do
  (ptr,_,_) <- deRefStablePtr sptr
  print (LPPEShowPRCRL ptr)
  putStrLn "bye"
