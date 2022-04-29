module Cardano.Faucet.Http.V1.Endpoints
  ( FaucetAPI
  , faucetApiV1Proxy
  , mkFaucetServer
  ) where

import Servant
import Cardano.Faucet.Models (DripRequest, AckUtxo, DripOption)
import Control.Monad.Except
import Cardano.Faucet.Http.Service ( Faucet(..) )

type FaucetAPI = "v1" :> (
    "askdrip" :> ReqBody '[JSON] DripRequest :> Post '[JSON] () :<|>
    "ackutxo" :> ReqBody '[JSON] AckUtxo     :> Post '[JSON] () :<|>
    "assets"  :> Get '[JSON] [DripOption]
  )

faucetApiV1Proxy :: Proxy FaucetAPI
faucetApiV1Proxy = Proxy

mkFaucetServer :: Faucet f -> ServerT FaucetAPI (ExceptT ServerError f)
mkFaucetServer Faucet{..} = registerRequest :<|> ackFundingUtxo :<|> getDripOptions
