{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ViewPatterns               #-}


module Minswap.OffChain where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Text                        (Text)
import           Ledger
import qualified Ledger.Ada              as Ada
import           Ledger.Contexts          as V
import           Ledger.Constraints               as Constraints
import qualified Ledger.Typed.Scripts             as Scripts
import qualified Ledger.Value            as Value
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import PlutusTx.Prelude as P
import qualified Prelude                          as H
import           Text.Printf                      (printf)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)

type UserSchema =
    BlockchainActions
        .\/ Endpoint "funds" ()
        .\/ Endpoint "test" ()

-- Gets the caller's funds.
ownFunds :: HasBlockchainActions s => Contract w s Text Value
ownFunds = do
    pkh <- pubKeyHash <$> ownPubKey
    logInfo $ "Get funds of " ++ show pkh
    os  <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
    return $ mconcat [txOutValue $ txOutTxOut o | o <- os]

-- Do nothing, just log something
test :: Contract w s Text ()
test = do
    logInfo @String "Test endpoint called"

endpoints :: Contract (Last Value) UserSchema Text ()
endpoints = (funds `select` test') >> endpoints
  where
    funds :: Contract (Last Value) UserSchema Text ()
    funds = h $ do
        endpoint @"funds"
        v <- ownFunds
        tell $ Last $ Just v

    test' :: Contract (Last Value) UserSchema Text ()
    test' = h $ do
        endpoint @"test"
        test
        
    h :: Contract (Last Value) UserSchema Text () -> Contract (Last Value) UserSchema Text ()
    h = handleError logError

