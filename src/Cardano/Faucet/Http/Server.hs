module Cardano.Faucet.Http.Server where

import RIO hiding (Handler)
import Control.Monad.Except (ExceptT, mapExceptT)

import Servant
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp as Warp

import Cardano.Faucet.Http.Service
import Cardano.Faucet.Configs
import Cardano.Faucet.Http.V1.Endpoints (faucetApiV1Proxy, mkFaucetServer)

mkHandler :: UnliftIO f -> ExceptT ServerError f a -> Servant.Handler a
mkHandler UnliftIO{..} = Handler . mapExceptT unliftIO

httpApp :: Faucet f -> UnliftIO f -> Application
httpApp service ul =
    cors (const $ Just policy)
      $ serve faucetApiV1Proxy
      $ hoistServer faucetApiV1Proxy (mkHandler ul) (mkFaucetServer service)
  where
    policy = simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      }

runHttpServer :: (MonadIO f) => HttpConfig -> Faucet f -> UnliftIO f -> f ()
runHttpServer HttpConfig{..} service uIO =
  liftIO (Warp.run (fromIntegral getPort) (httpApp service uIO))
