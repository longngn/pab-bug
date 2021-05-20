{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    ( main
    ) where

import           Control.Monad
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, ToJSON, Result (..), fromJSON, encode)
import           Data.Monoid                         (Last (..))
import           Plutus.Contract                     hiding (when)
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server

import           Minswap.OffChain
import           GHC.Generics
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin Contracts) "Starting Oracle PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    void $ liftIO getLine
    shutdown

data Contracts = Contracts
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty Contracts where
    pretty = viaShow

handleContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin Contracts))) effs
    )
    => ContractEffect (Builtin Contracts)
    ~> Eff effs
handleContracts = handleBuiltin getSchema getContract where
    getSchema = \_ -> endpointsToSchemas @(UserSchema .\\ BlockchainActions)
    getContract = \_ -> SomeBuiltin endpoints

handlers :: SimulatorEffectHandlers (Builtin Contracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin Contracts) []
    $ interpret handleContracts
