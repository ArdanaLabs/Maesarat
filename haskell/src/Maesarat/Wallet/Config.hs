{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This modules provides the 'WalletConfig' type for cardano-wallet.
--
-- @since 0.1.0.0
module Maesarat.Wallet.Config
  ( WalletConfig (..),
    WalletExePath (..),
    IP (..),
    Port (..),
  )
where

import Data.String (IsString)
import GHC.Natural (Natural)
import Maesarat.Data.Network (IP (..), Port (..))
import Optics.TH qualified as OTH

-- | String newtype for the cardano-wallet executable path.
-- See the example for its 'Monoid' instance.
--
-- ==== __Examples__
-- >>> mempty @WalletExePath
-- MkWalletExePath {unWalletExePath = "cardano-wallet"}
--
-- @since 0.1.0.0
newtype WalletExePath = MkWalletExePath
  { -- | @since 0.1.0.0
    unWalletExePath :: FilePath
  }
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      IsString
    )
    via String

-- | @since 0.1.0.0
OTH.makeFieldLabelsNoPrefix ''WalletExePath

-- | @since 0.1.0.0
instance Semigroup WalletExePath where
  MkWalletExePath "cardano-wallet" <> r = r
  l <> _ = l

-- | @since 0.1.0.0
instance Monoid WalletExePath where
  mempty = MkWalletExePath "cardano-wallet"

-- | Configuration for running cardano-wallet. See the example for its
-- 'Monoid' instance.
--
-- ==== __Example__
-- >>> mempty @WalletConfig
-- MkWalletConfig {exePath = MkWalletExePath {unWalletExePath = "cardano-wallet"}, address = MkIP {unIP = "127.0.0.1"}, port = MkPort {unPort = 8090}, timeout = 30}
--
-- @since 0.1.0.0
data WalletConfig = MkWalletConfig
  { -- | Path to the cardano-wallet executable.
    --
    -- @since 0.1.0.0
    exePath :: WalletExePath,
    -- | IP address cardano-wallet should serve on.
    --
    -- @since 0.1.0.0
    address :: IP,
    -- | Port cardano-wallet should serve the REST API on.
    --
    -- @since 0.1.0.0
    port :: Port,
    -- | Length of time to wait for cardano-wallet to initialize,
    -- in seconds. The default is 30 seconds.
    --
    -- @since 0.1.0.0
    timeout :: Natural
  }
  deriving
    ( -- | @since 0.1.0.0
      Show
    )

-- | @since 0.1.0.0
OTH.makeFieldLabelsNoPrefix ''WalletConfig

-- | @since 0.1.0.0
instance Semigroup WalletConfig where
  MkWalletConfig e a p t <> MkWalletConfig e' a' p' t' =
    MkWalletConfig (e <> e') (a <> a') newPort newTimeout
    where
      newPort =
        if p == 8090
          then p'
          else p
      newTimeout =
        if t == 30
          then t'
          else t

-- | @since 0.1.0.0
instance Monoid WalletConfig where
  mempty = MkWalletConfig mempty mempty 8090 30
