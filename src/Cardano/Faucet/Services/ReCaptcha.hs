module Cardano.Faucet.Services.ReCaptcha
  ( ReCaptcha(..)
  , mkReCaptcha
  ) where

import RIO

import Network.HTTP.Simple

import Data.Aeson (FromJSON)
import Data.ByteString.Char8

import Cardano.Faucet.Types (ReCaptchaToken(..), ReCaptchaSecret(..))
import System.Logging.Hlog
import Network.HTTP.Client.Conduit (urlEncodedBody)

data ReCaptcha m = ReCaptcha
  { verify :: ReCaptchaToken -> m Bool
  }

mkReCaptcha :: (Monad f, MonadIO m) => ReCaptchaSecret -> MakeLogging f m -> f (ReCaptcha m)
mkReCaptcha secret MakeLogging{..} = do
  logging <- forComponent "ReCaptcha"
  pure ReCaptcha
    { verify = traceVerify logging $ verify' secret
    }

data VerificationResult = VerificationResult
  { success :: Bool
  }
  deriving (Generic, FromJSON)

verify' :: MonadIO m => ReCaptchaSecret -> ReCaptchaToken -> m Bool
verify' (ReCaptchaSecret secret) (ReCaptchaToken token) = do
  let
    req =
      parseRequest_ "https://www.google.com/recaptcha/api/siteverify" &
      setRequestMethod "POST" &
      urlEncodedBody [("secret", pack secret), ("response", pack token)]
  response <- httpJSON @_ @VerificationResult req
  pure . success . getResponseBody $ response

traceVerify :: (Monad m, Show a, Show b) => Logging m -> (a -> m b) -> a -> m b
traceVerify Logging{..} fn token = do
  debugM $ "verify " <> show token
  r <- fn token
  debugM $ "verify " <> show token <> " -> " <> show r
  pure r
