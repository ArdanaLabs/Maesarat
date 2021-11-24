module Main (main) where

import Args qualified as Args
import Control.Concurrent qualified as CC
import Maesarat.Testnet qualified as TN
import Maesarat.Wallet qualified as Wallet
import Network.HTTP.Client qualified as Net
import Optics.Core ((^.))
import System.Exit qualified as SysEx
import System.Process qualified as P

main :: IO ()
main = do
  args <- Args.getArgs
  let tnPath = args ^. #testnetPath
  netManager <- Net.newManager Net.defaultManagerSettings
  isTestnetUp <- TN.testnetUp netManager tnPath
  case isTestnetUp of
    Just tnErr -> SysEx.die $ show tnErr
    Nothing -> pure ()

  ePh <- Wallet.runWallet tnPath (args ^. #walletConfig) netManager
  ph <- case ePh of
    Left walletErr -> SysEx.die $ show walletErr
    Right ph -> pure ph

  putStrLn "Wallet is up!"

  CC.threadDelay 5_000_000

  putStrLn "Cleaning up..."
  P.cleanupProcess (Nothing, Nothing, Nothing, ph)
