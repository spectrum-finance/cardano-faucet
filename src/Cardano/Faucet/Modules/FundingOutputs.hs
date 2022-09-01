module Cardano.Faucet.Modules.FundingOutputs where

import RIO

import Control.Monad.Trans.Resource (MonadResource)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson (encode, decode)

import System.Logging.Hlog (Logging(Logging, debugM), MakeLogging(..))

import CardanoTx.Models
import Cardano.Faucet.Types (DripAsset(..))
import Cardano.Faucet.Configs (OutputsStoreConfig(..))

import qualified Database.RocksDB as Rocks

data FundingOutputs m = FundingOutputs
  { putOutput  :: DripAsset -> FullTxOut -> m ()
  , getOutput  :: DripAsset -> m (Maybe FullTxOut)
  , dropOutput :: DripAsset -> m ()
  }

mkFundingOutputs
  :: (MonadIO f, MonadResource f, MonadIO m)
  => OutputsStoreConfig
  -> MakeLogging f m
  -> f (FundingOutputs m)
mkFundingOutputs OutputsStoreConfig{..} MakeLogging{..} = do
  logging <- forComponent "FundingOutputs"
  db      <- Rocks.open storePath
              Rocks.defaultOptions
                { Rocks.createIfMissing = createIfMissing
                }
  pure $ attachTracing logging FundingOutputs
    { putOutput  = putOutput' db Rocks.defaultWriteOptions
    , getOutput  = getOutput' db Rocks.defaultReadOptions 
    , dropOutput = dropOutput' db Rocks.defaultWriteOptions
    }

putOutput' :: MonadIO m => Rocks.DB -> Rocks.WriteOptions -> DripAsset -> FullTxOut -> m ()
putOutput' db opts asset out = liftIO $ Rocks.put db opts (asKey asset) (LBS.toStrict $ encode out)

getOutput' :: MonadIO m => Rocks.DB -> Rocks.ReadOptions -> DripAsset -> m (Maybe FullTxOut)
getOutput'  db opts asset = liftIO $ Rocks.get db opts (asKey asset) <&> (>>= (decode . LBS.fromStrict))

dropOutput' :: MonadIO m => Rocks.DB -> Rocks.WriteOptions -> DripAsset -> m ()
dropOutput' db opts asset = liftIO . Rocks.delete db opts $ asKey asset

asKey :: Show a => a -> ByteString
asKey = BS.pack . show

attachTracing :: Monad m => Logging m -> FundingOutputs m -> FundingOutputs m
attachTracing Logging{..} FundingOutputs{..} =
  FundingOutputs
    { putOutput = \asset out -> do
        debugM $ "putOutput " <> show asset <> " " <> show out
        r <- putOutput asset out
        debugM $ "putOutput " <> show asset <> " " <> show out <> " -> " <> show r
        pure r
    , getOutput = \asset -> do
        debugM $ "getOutput " <> show asset
        r <- getOutput asset
        debugM $ "getOutput " <> show asset <> " -> " <> show r
        pure r
    , dropOutput = \asset -> do
        debugM $ "dropOutput " <> show asset
        r <- dropOutput asset
        debugM $ "dropOutput " <> show asset <> " -> " <> show r
        pure r
    }
