----------------------
--      PARELM      --
----------------------

module Parelm where

import LPPE
import Usage

-- This function removes global parameters that are never used.
parelm :: PSpecification -> PSpecification
parelm (lppe, initial, dataspec) = (lppeReduced, initial2, dataspec)
  where
    unused      = [parNr | parNr <- [0..length (getLPPEPars lppe) - 1], 
                           and [not (isUsedInSummand summand parNr (fst ((getLPPEPars lppe)!!parNr)) False) | summand <- (getPSummands lppe)]]
    (lppeReduced,initial2,_) = removeParametersFromLPPE (lppe,initial,dataspec) unused
   