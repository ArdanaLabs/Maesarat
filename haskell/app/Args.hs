{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for parsing command line args.
--
-- @since 0.1.0.0
module Args
  ( Args (..),
    getArgs,
  )
where

import Control.Applicative ((<**>))
import GHC.Natural (Natural)
import Maesarat.Data.Network (IP (..), Port (..))
import Maesarat.Testnet.Config (TestnetPath (..))
import Maesarat.Wallet.Config (WalletConfig (..), WalletExePath (..))
import Optics.TH qualified as OTH
import Options.Applicative (Parser, ParserInfo (..))
import Options.Applicative qualified as OApp
import Options.Applicative.Help.Chunk (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))

-- | Command line args for running maesarat.
--
-- @since 0.1.0.0
data Args = MkArgs
  { testnetPath :: TestnetPath,
    walletConfig :: WalletConfig
  }
  deriving (Show)

-- | @since 0.1.0.0
OTH.makeFieldLabelsNoPrefix ''Args

-- | Retrieves cli args as 'Args'.
--
-- @since 0.1.0.0
getArgs :: IO Args
getArgs = OApp.execParser parserInfoArgs

parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = Chunk Nothing,
      infoHeader = Chunk Nothing,
      infoFooter = Chunk Nothing,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }

argsParser :: Parser Args
argsParser =
  MkArgs
    <$> testnetParser
    <*> walletParser
    <**> OApp.helper

testnetParser :: Parser TestnetPath
testnetParser =
  MkTestnetPath
    <$> OApp.strOption
      ( OApp.long "testnet-path"
          <> OApp.short 't'
          <> OApp.help helpTxt
          <> OApp.metavar "PATH"
      )
  where
    helpTxt =
      "Path to testnet directory, e.g., cardano-node/run/current"

walletParser :: Parser WalletConfig
walletParser =
  MkWalletConfig
    <$> walletExeParser
    <*> walletAddressParser
    <*> walletPortParser
    <*> walletTimeoutParser

walletExeParser :: Parser WalletExePath
walletExeParser =
  MkWalletExePath
    <$> OApp.strOption
      ( OApp.long "wallet-exe"
          <> OApp.value "cardano-wallet"
          <> OApp.help helpTxt
          <> OApp.metavar "PATH"
      )
  where
    helpTxt =
      "The path to the cardano-wallet exe. Defaults to `cardano-wallet`"

walletAddressParser :: Parser IP
walletAddressParser =
  OApp.strOption
    ( OApp.long "wallet-host"
        <> OApp.value "127.0.0.1"
        <> OApp.help helpTxt
        <> OApp.metavar "IP"
    )
  where
    helpTxt =
      "The IP address for the cardano-wallet service."
        <> " Defaults to 127.0.0.1"

walletPortParser :: Parser Port
walletPortParser =
  MkPort
    <$> OApp.option
      OApp.auto
      ( OApp.long "wallet-port"
          <> OApp.value 8090
          <> OApp.help helpTxt
          <> OApp.metavar "PORT"
      )
  where
    helpTxt = "The port for the cardano-wallet service. Defaults to 8090."

walletTimeoutParser :: Parser Natural
walletTimeoutParser =
  OApp.option
    OApp.auto
    ( OApp.long "timeout"
        <> OApp.value 30
        <> OApp.help helpTxt
        <> OApp.metavar "NUM"
    )
  where
    helpTxt =
      "The amount of time we wait for cardano-wallet to "
        <> " become responsive, in seconds. Defaults to 30."
