-- | This modules provides functionality for determining if the testnet is up.
--
-- @since 0.1.0.0
module Maesarat.Testnet
  ( testnetUp,
    TestnetError (..),
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Maesarat.Servant (HTML ())
import Maesarat.Testnet.Config (TestnetPath (..))
import Maesarat.Utils ((<!>), (</>))
import Maesarat.Utils qualified as U
import Network.HTTP.Client (Manager)
import Servant.API (Get, OctetStream, type (:<|>) (..), type (:>))
import Servant.Client
  ( BaseUrl (..),
    ClientError,
    ClientM,
    Scheme (Http),
  )
import Servant.Client qualified as SC
import System.Directory qualified as Dir

type TestnetAPI =
  Get '[HTML] Text
    :<|> "metrics" :> Get '[OctetStream] ByteString

api :: Proxy TestnetAPI
api = Proxy

getEkg :: ClientM Text
getPrometheus :: ClientM ByteString
(getEkg :<|> getPrometheus) = SC.client api

-- | Errors encountered with the testnet.
--
-- @since 0.1.0.0
data TestnetError
  = -- | The EKG service was unresponsive.
    --
    -- @since 0.1.0.0
    EkgDown ClientError
  | -- | Prometheus was unresponsive.
    --
    -- @since 0.1.0.0
    PrometheusDown ClientError
  | -- | The node socket file was not found.
    --
    -- @since 0.1.0.0
    NodeSocketNotFound String
  deriving
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )

-- | Takes in a path to the testnet directory and tests that it is "up".
-- Returns 'Nothing' if testnet is up. Returns an error if /any/ of the
-- following is true.
--
-- * @testnetPath\/node-0\/node.socket@ cannot be found
-- * EKG is not up at @127.0.0.1:30100@
-- * Prometheus is not up at @127.0.0.1:30200\/metrics@
--
-- @since 0.1.0.0
testnetUp :: Manager -> TestnetPath -> IO (Maybe TestnetError)
testnetUp netManager tnPath = do
  socketExists tnPath <!> ekgUp netManager <!> prometheusUp netManager

ekgUp :: Manager -> IO (Maybe TestnetError)
ekgUp netManager = do
  let ekgEnv = SC.mkClientEnv netManager ekgBase
  ekgResult <- SC.runClientM getEkg ekgEnv
  pure $ U.errToM $ first EkgDown ekgResult
  where
    ekgBase =
      BaseUrl
        { baseUrlScheme = Http,
          baseUrlHost = "127.0.0.1",
          baseUrlPort = 30100,
          baseUrlPath = ""
        }

prometheusUp :: Manager -> IO (Maybe TestnetError)
prometheusUp netManager = do
  let prometheusEnv = SC.mkClientEnv netManager prometheusBase
  prometheusResult <- SC.runClientM getPrometheus prometheusEnv
  pure $ U.errToM $ first PrometheusDown prometheusResult
  where
    prometheusBase =
      BaseUrl
        { baseUrlScheme = Http,
          baseUrlHost = "127.0.0.1",
          baseUrlPort = 30200,
          baseUrlPath = ""
        }

socketExists :: TestnetPath -> IO (Maybe TestnetError)
socketExists (MkTestnetPath tnPath) = do
  let socketPath = tnPath </> "node-0/node.socket"
  b <- Dir.doesFileExist socketPath
  pure $
    if b
      then Nothing
      else Just $ NodeSocketNotFound socketPath
