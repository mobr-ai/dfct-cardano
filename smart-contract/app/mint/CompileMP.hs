{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Cardano.Api
import PlutusLedgerApi.V3
import PlutusTx.Prelude
import PlutusTx
import Prelude (IO, putStrLn, show)

import DFCT.MintingPolicy

{-# INLINABLE mkWrappedPolicy #-}
mkWrappedPolicy :: MintingParams -> BuiltinData -> BuiltinUnit
mkWrappedPolicy params ctx = mkDFCMintingPolicy params ctx

-- | Compile the policy with hardcoded MintingParams
dfcmintingPolicy :: CompiledCode (BuiltinData -> BuiltinUnit)
dfcmintingPolicy =
    let params = mkMintingParams
                   (PubKeyHash "2968d98bb73b59a3c878ae7a2e8d1f4386bea032471db53c63697bee")
                   (TokenName "DFC")
    in $$(PlutusTx.compile [|| \ctx -> mkWrappedPolicy params ctx ||])

genV3Script :: PlutusScript PlutusScriptV3
genV3Script = PlutusScriptSerialised $ serialiseCompiledCode dfcmintingPolicy

main :: IO ()
main = do
  result <- writeFileTextEnvelope "dfct-minting-policy.plutus" Nothing genV3Script
  case result of
    Left err -> putStrLn $ "Error writing script: " ++ show err
    Right () -> putStrLn "Successfully wrote dfct-minting-policy.plutus"