module Cardano.Faucet.Modules.FundingOutputs where

import RIO hiding (writeChan, readChan, newChan)

import CardanoTx.Models
import Control.Concurrent.Chan.Unagi
import System.Logging.Hlog (Logging(Logging, debugM), MakeLogging(..))
import Cardano.Faucet.Types (DripAsset(..))
import Ledger.Value (AssetClass(AssetClass), toString)

data FundingOutputs m = FundingOutputs
  { put     :: FullTxOut -> m ()
  , acquire :: m FullTxOut -- blocks until UTxO is available
  }

mkFundingOutputs :: (MonadIO f, MonadIO m) => MakeLogging f m -> DripAsset -> f (FundingOutputs m)
mkFundingOutputs MakeLogging{..} (DripAsset (AssetClass (_, tn))) = do
  logging     <- forComponent $ "FundingOutputs#" <> toString tn
  (inc, outc) <- liftIO newChan
  pure FundingOutputs
    { put     = tracePut logging $ put' inc
    , acquire = traceAcquire logging $ acquire' outc
    }

put' :: MonadIO m => InChan FullTxOut -> FullTxOut -> m ()
put' channel = liftIO . writeChan channel

tracePut :: (Monad m, Show a, Show b) => Logging m -> (a -> m b) -> a -> m b
tracePut Logging{..} fn out = do
  debugM $ "put " <> show out
  r <- fn out
  debugM $ "put " <> show out <> " -> " <> show r
  pure r

acquire' :: MonadIO m => OutChan FullTxOut -> m FullTxOut
acquire' = liftIO . readChan

traceAcquire :: (Monad m, Show b) => Logging m -> m b -> m b
traceAcquire Logging{..} fa = do
  debugM @String "acquire"
  r <- fa
  debugM $ "acquire " <> " -> " <> show r
  pure r
