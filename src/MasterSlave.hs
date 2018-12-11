{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterSlave where

import "base"                Control.Monad
import "base"                System.IO (stderr, hPrint)
import "distributed-process" Control.Distributed.Process
import "distributed-process" Control.Distributed.Process.Closure
import "monad-logger"        Control.Monad.Logger
import "monad-logger-syslog" System.Log.MonadLogger.Syslog
import "random"              System.Random (randomRIO)
import                       Utils

defaultStderrOutput a b c d = hPrint stderr $ defaultLogStr a b c d

instance MonadLogger Process where
    monadLoggerLog loc logSource logLevel msg
        = liftIO $ defaultStderrOutput loc logSource logLevel (toLogStr msg)

worker :: (ProcessId, Integer) -> Process ()
worker (pid, n) = do
    nPrimeFactors <- liftIO $ numPrimeFactors n
    $(logDebugSH) nPrimeFactors
    send pid nPrimeFactors

remotable ['worker]

-- Wait for n integers and sum them all up
sumIntegers :: Int -> Process Integer
sumIntegers = go 0
    where
        go :: Integer -> Int -> Process Integer
        go acc 0 = return acc
        go acc n = do
            m <- expect
            go (acc + m) (n - 1)

builder :: (Integer, Integer) -> [NodeId] -> Process Integer
builder (n, m) slaves = do
    -- get a random number in the given range
    number <- liftIO $ randomRIO (n, m)

    us <- getSelfPid

    -- Reply with the next bit of work to be done and reconnect in the end
    forM_ (zip [1 .. number] (cycle slaves)) $ \(k, there) -> do
        them <- spawn there ($(mkClosure 'worker) (us, k))
        $(logDebugSH) (k, there)
        reconnect them

    -- Wait for the result
    sumIntegers (fromIntegral number)
