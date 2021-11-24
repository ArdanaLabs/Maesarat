{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides common network types.
--
-- @since 0.1.0.0
module Maesarat.Data.Network
  ( IP (..),
    Port (..),
  )
where

import Data.String (IsString)
import GHC.Natural (Natural)
import Optics.TH qualified as OTH

-- | String newtype for IP. See the example for its 'Monoid' instance..
--
-- ==== __Example__
-- >>> mempty @IP
-- MkIP {unIP = "127.0.0.1"}
--
-- @since 0.1.0.0
newtype IP = MkIP
  { -- | @since 0.1.0.0
    unIP :: String
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
OTH.makeFieldLabelsNoPrefix ''IP

-- | @since 0.1.0.0
instance Semigroup IP where
  MkIP "127.0.0.1" <> r = r
  l <> _ = l

-- | @since 0.1.0.0
instance Monoid IP where
  mempty = MkIP "127.0.0.1"

-- | Natural newtype for Port. See the example for its 'Monoid' instance.
--
-- ==== __Example__
-- >>> mempty @Port
-- MkPort {unPort = 8000}
--
-- @since 0.1.0.0
newtype Port = MkPort {unPort :: Natural}
  deriving stock
    ( -- | @since 0.1.0.0
      Eq,
      -- | @since 0.1.0.0
      Show
    )
  deriving
    ( -- | @since 0.1.0.0
      Num
    )
    via Natural

-- | @since 0.1.0.0
OTH.makeFieldLabelsNoPrefix ''Port

-- | @since 0.1.0.0
instance Semigroup Port where
  MkPort 8000 <> r = r
  l <> _ = l

-- | @since 0.1.0.0
instance Monoid Port where
  mempty = MkPort 8000
