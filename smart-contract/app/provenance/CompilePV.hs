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

import DFCT.Provenance (mkDFCTValidator)

-- replace with policy id
dfcSymbol :: CurrencySymbol
dfcSymbol = CurrencySymbol $ "ba620a995810f982de2a8994901335bda7fa041eeec1ae32dc57edfa"

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinUnit
mkWrappedValidator = mkDFCTValidator dfcSymbol

validator :: CompiledCode (BuiltinData -> BuiltinUnit)
validator = $$(PlutusTx.compile [|| mkWrappedValidator ||])

genV3Script :: PlutusScript PlutusScriptV3
genV3Script = PlutusScriptSerialised $ serialiseCompiledCode validator

main :: IO ()
main = do
  result <- writeFileTextEnvelope "dfct-provenance.plutus" Nothing genV3Script
  case result of
    Left err -> putStrLn $ "Error writing script: " ++ show err
    Right () -> putStrLn "Successfully wrote dfct-provenance.plutus"