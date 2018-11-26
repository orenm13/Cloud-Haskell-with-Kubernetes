module Main where

import Control.Monad      (forever)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import Control.Exception  (evaluate)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import qualified MasterSlave as MS

rtable :: RemoteTable
rtable = MS.__remoteTable initRemoteTable

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["master", host, port, n, m] -> do
            backend <- initializeBackend host port rtable
            startMaster backend $ \slaves -> forever $ do
                result <- MS.master (read n, read m) slaves
                liftIO $ print result
                liftIO $ threadDelay (10 ^ 6)
        ["slave", host, port] -> do
            backend <- initializeBackend host port rtable
            startSlave backend
        _ -> putStrLn "Error: Bad Args"
