{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides configuration types for the testnet.
--
-- @since 0.1.0.0
module Maesarat.Testnet.Config
  ( TestnetPath (..),
  )
where

import Data.String (IsString)
import Optics.TH qualified as OTH

-- | String newtype for the testnet path.
--
-- @since 0.1.0.0
newtype TestnetPath = MkTestnetPath
  { -- | @since 0.1.0.0
    unTestnetPath :: FilePath
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
OTH.makeFieldLabelsNoPrefix ''TestnetPath
