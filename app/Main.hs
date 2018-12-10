module Main where

import Control.Monad      (forever)
import System.IO
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import Control.Exception  (evaluate)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import System.Process     (readCreateProcess, shell)
import Data.List.Split    (splitOn)

import qualified MasterSlave as MS

rtable :: RemoteTable
rtable = MS.__remoteTable initRemoteTable

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["builder", port, n, m] -> do
            rawHostIP <- readCreateProcess (shell "hostname -I") ""
            let hostIP = head $ splitOn " " rawHostIP
            hPrint stderr hostIP
            --------
            backend <- initializeBackend hostIP port rtable
            startMaster backend $ \slaves -> forever $ do
                result <- MS.builder (read n, read m) slaves
                liftIO $ hPrint stderr result
                liftIO $ threadDelay (10 ^ 6)
        ["worker", port] -> do
            hostIP <- readCreateProcess (shell "hostname -I") ""
            backend <- initializeBackend hostIP port rtable
            startSlave backend
        _ -> hPrint stderr "Error, Bad Args"
