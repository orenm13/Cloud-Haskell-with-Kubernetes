module Main where

import System.Environment (getArgs)
import Control.Exception (evaluate)
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
        ["master", host, port, n] -> do
            backend <- initializeBackend host port rtable
            startMaster backend $ \slaves -> do
                result <- MS.master (read n) slaves
                liftIO $ print result
        ["slave", host, port] -> do
            backend <- initializeBackend host port rtable
            startSlave backend
        _ -> putStrLn "Error: Bad Args"
