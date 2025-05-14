{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module DFCT.MintingPolicy where

import PlutusTx.Prelude
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts (ownCurrencySymbol)
import PlutusTx.AssocMap as Map

-- parameters for the minting policy
data MintingParams = MintingParams
  { mpMintingAuthority :: PubKeyHash
  , mpTokenName        :: TokenName
  }

-- minting policy script
{-# INLINABLE mkDFCMintingPolicy #-}
mkDFCMintingPolicy :: MintingParams -> BuiltinData -> BuiltinUnit
mkDFCMintingPolicy params ctx =
  let context = unsafeFromBuiltinData ctx :: ScriptContext
      txInfo = scriptContextTxInfo context
      cs = ownCurrencySymbol context
      minted = txInfoMint txInfo
      mintedMap = mintValueToMap minted
      tokenMap = fromMaybe Map.empty (Map.lookup cs mintedMap)
      expectedTokenName = mpTokenName params
      -- check if the token map contains exactly the expected token name with a positive amount
      tokenMapValid = case Map.toList tokenMap of
        [(tn, amt)] -> tn == expectedTokenName && amt > 0
        _ -> False
      -- signatories = txInfoSignatories txInfo
      -- authorizedPKH = mpMintingAuthority params
  -- not checking for authorities for now
  in if tokenMapValid then toOpaque ()
     else traceError "Invalid token map."

  -- in if checkAuthorized signatories authorizedPKH && tokenMapValid then toOpaque ()
  --    else if tokenMapValid then traceError "Unauthorized minting! " authMsg sigMsg
  --    else traceError "Invalid token map."

-- check if the transaction is signed by the authorized public key hash
{-# INLINABLE checkAuthorized #-}
checkAuthorized :: [PubKeyHash] -> PubKeyHash -> Bool
checkAuthorized signatories authPKH = elem authPKH signatories

-- helper to create parameters with the "DFC" token name and minting authority
mkMintingParams :: PubKeyHash -> TokenName -> MintingParams
mkMintingParams pkh tokenName = MintingParams
  { mpMintingAuthority = pkh
  , mpTokenName = tokenName
  }
