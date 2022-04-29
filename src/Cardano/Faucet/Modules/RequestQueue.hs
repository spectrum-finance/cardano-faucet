module Cardano.Faucet.Modules.RequestQueue where

import RIO hiding (newChan)

import Alg.Natural

import Control.Concurrent.Chan.Unagi.Bounded
import Cardano.Faucet.Models

data RequestQueue m = RequestQueue
  { enqueue :: DripRequest -> m Bool
  , dequeue :: m (Maybe DripRequest)
  }

instance FunctorK RequestQueue where
  fmapK xa alg =
    RequestQueue
      { enqueue = xa . enqueue alg
      , dequeue = xa $ dequeue alg
      }

mkRequestQueue :: (MonadIO f, MonadIO m) => Int -> f (RequestQueue m)
mkRequestQueue capacity = liftIO $ do
  (inc, outc) <- newChan capacity
  lastElemVar <- newEmptyMVar
  pure $ RequestQueue
    { enqueue = enqueue' inc
    , dequeue = dequeue' lastElemVar outc
    }

enqueue' :: MonadIO m => InChan DripRequest -> DripRequest -> m Bool
enqueue' channel = liftIO . tryWriteChan channel

dequeue' :: MonadIO m => MVar (Element DripRequest) -> OutChan DripRequest -> m (Maybe DripRequest)
dequeue' lastElemVar channel = liftIO $ do
  e <- tryTakeMVar lastElemVar -- take last memoized element from var 
  let
    handleElement el = do
      r <- tryRead el -- try read the element
      maybe
        (putMVar lastElemVar el $> Nothing) -- if the element isn't available put the ref back
        (pure . Just)
        r
  case e of
    Just el -> handleElement el -- if there is a memoized element work with it
    Nothing -> do               -- take a new element from the channel otherwise
      (el, _) <- tryReadChan channel
      handleElement el
