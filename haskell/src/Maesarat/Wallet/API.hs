-- | This module provides the API for interacting with the cardano-wallet
-- application. We manually implement these ourselves to keep the
-- dependencies light and the API simpler, as we only need a fraction
-- of the functionality.
--
-- @since 0.1.0.0
module Maesarat.Wallet.API
  ( WalletAPI,
    getNetworkInfo,
    getWallets,
  )
where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API (Get, JSON, type (:<|>) (..), type (:>))
import Servant.Client (ClientM, client)

-- | Rest API for cardano-wallet.
--
-- @since 0.1.0.0
type WalletAPI =
  "network" :> "information" :> Get '[JSON] Text
    :<|> "wallets" :> Get '[JSON] Text

api :: Proxy WalletAPI
api = Proxy

-- | Handler for @network/information@.
--
-- @since 0.1.0.0
getNetworkInfo :: ClientM Text

-- | Handler for @/wallets@.
--
-- @since 0.1.0.0
getWallets :: ClientM Text
(getNetworkInfo :<|> getWallets) = client api
