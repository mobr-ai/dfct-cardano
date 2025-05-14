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
                   (PubKeyHash "b3607d483242edbaae8d40c1d1c592ef9a6dd4fdaa8649f832ced319")
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