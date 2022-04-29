module Cardano.Faucet.Modules.OutputResolver where

import RIO (MonadIO, (<&>))

import Ledger
import CardanoTx.Models
import Explorer.Service
import Explorer.Class (ToCardanoTx(toCardanoTx))
import System.Logging.Hlog

data OutputResolver m = OutputResolver
  { resolve :: TxOutRef -> m (Maybe FullTxOut)
  }

mkOutputResolver :: (Monad f, MonadIO m) => Explorer m -> MakeLogging f m -> f (OutputResolver m)
mkOutputResolver explorer MakeLogging{..} = do
  logging <- forComponent "OutputResolver"
  pure OutputResolver
    { resolve = traceResolve logging (resolve' explorer)
    }

resolve' :: MonadIO m => Explorer m -> TxOutRef -> m (Maybe FullTxOut)
resolve' Explorer{..} ref = getOutput ref <&> (<&> toCardanoTx)

traceResolve :: (Monad m, Show t, Show b) => Logging m -> (t -> m b) -> t -> m b
traceResolve Logging{..} fn ref = do
  debugM $ "resolve " <> show ref
  r <- fn ref
  debugM $ "resolve " <> show r <> " -> " <> show r
  pure r
