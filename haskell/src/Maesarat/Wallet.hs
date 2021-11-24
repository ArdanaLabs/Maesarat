-- | This module provides functionality for starting cardano-wallet.
--
-- @since 0.1.0.0
module Maesarat.Wallet
  ( runWallet,
    WalletError (..),
  )
where

import Control.Concurrent qualified as CC
import Control.Exception.Safe qualified as Ex
import Control.Monad qualified as M
import Data.Functor (($>))
import GHC.Natural (Natural)
import Maesarat.Testnet.Config (TestnetPath (..))
import Maesarat.Utils ((</>))
import Maesarat.Utils qualified as U
import Maesarat.Wallet.API qualified as WalletAPI
import Maesarat.Wallet.Config (WalletConfig, WalletExePath (..))
import Network.HTTP.Client (Manager)
import Optics.Core ((%), (^.))
import Servant.Client (BaseUrl (..), Scheme (..))
import Servant.Client qualified as SC
import System.Directory qualified as Dir
import System.Process (ProcessHandle)
import System.Process qualified as P
import System.Timeout qualified as TO

-- | Errors encountered while starting cardano-wallet.
--
-- @since 0.1.0.0
data WalletError
  = -- | The executable was not found.
    --
    -- @since 0.1.0.0
    ExeNotFound String
  | -- | An exception was encountered while starting the executable.
    --
    -- @since 0.1.0.0
    InitExeException String
  | -- | Timed out waiting for the REST API to become responsive.
    --
    -- @since 0.1.0.0
    TimedOut Natural
  deriving
    ( -- | @since 0.1.0.0
      Show
    )

-- | Starts the cardano-wallet executable, returning the 'ProcessHandle' if
-- successful. Otherwise returns a 'WalletError'.
--
-- @since 0.1.0.0
runWallet :: TestnetPath -> WalletConfig -> Manager -> IO (Either WalletError ProcessHandle)
runWallet tnPath config netManager = do
  eWalletExe <- walletExeFound (config ^. #exePath)
  eProcessHandle <- M.join <$> traverse (startWallet tnPath config) eWalletExe
  case eProcessHandle of
    Left err -> pure $ Left err
    Right ph -> walletUp config netManager $> Right ph

startWallet :: TestnetPath -> WalletConfig -> FilePath -> IO (Either WalletError ProcessHandle)
startWallet (MkTestnetPath tnPath) config exe = do
  putStrLn $
    "Starting cardano-wallet with the following args: "
      <> show walletArgs

  -- Most errors will result in the exe dying immediately, but we don't get
  -- the exit code since we want to run the wallet in the background.
  -- Thus we wait 1 second and check to see if the process has exited before
  -- moving onto the next stage.
  ePh <- Ex.tryAny (P.spawnProcess exe walletArgs)
  case ePh of
    Left ex -> pure $ Left $ InitExeException $ show ex
    Right ph -> do
      CC.threadDelay 1_000_000
      mExitCode <- P.getProcessExitCode ph
      case mExitCode of
        Nothing -> pure $ Right ph
        Just ec ->
          pure $
            Left $
              InitExeException $
                "cardano-wallet terminated early with exit code: " <> show ec
  where
    nodeSocket = tnPath </> "node-0/node.socket"
    genesisFile = tnPath </> "genesis/byron/genesis.json"
    walletArgs =
      [ "serve",
        "--node-socket",
        nodeSocket,
        "--testnet",
        genesisFile,
        "--listen-address",
        config ^. #address % #unIP,
        "--port",
        show (config ^. #port % #unPort)
      ]

-- This function is not total, so make sure it is called with a timeout!
walletUp :: WalletConfig -> Manager -> IO (Either WalletError ())
walletUp config netManager = do
  let env = SC.mkClientEnv netManager base
  result <- TO.timeout timeoutMicro (loopReq (SC.runClientM WalletAPI.getNetworkInfo env))
  pure $ case result of
    Just _ -> Right ()
    Nothing -> Left $ TimedOut timeout
  where
    timeout = config ^. #timeout
    timeoutMicro = secondsToMicro $ fromIntegral timeout
    base =
      BaseUrl
        { baseUrlScheme = Http,
          baseUrlHost = config ^. #address % #unIP,
          baseUrlPort = fromIntegral $ config ^. #port % #unPort,
          baseUrlPath = ""
        }

-- This function is not total, so make sure it is called with a timeout!
loopReq :: IO (Either e a) -> IO a
loopReq req = do
  res <- req
  case res of
    Right x -> pure x
    Left _ -> do
      CC.threadDelay $ secondsToMicro 5
      loopReq req

walletExeFound :: WalletExePath -> IO (Either WalletError FilePath)
walletExeFound (MkWalletExePath path) =
  U.mToE (ExeNotFound path) <$> Dir.findExecutable path

secondsToMicro :: Int -> Int
secondsToMicro = (* 1_000_000)
