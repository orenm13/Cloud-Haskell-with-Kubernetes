{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterSlave where

import "base"                Control.Monad
import "distributed-process" Control.Distributed.Process
import "distributed-process" Control.Distributed.Process.Closure
import "monad-logger"        Control.Monad.Logger
import "monad-logger-syslog" System.Log.MonadLogger.Syslog
import "random"              System.Random (randomRIO)
import                       Utils

instance MonadLogger Process where
    monadLoggerLog loc logSource logLevel msg
        = liftIO $ defaultSyslogOutput loc logSource logLevel (toLogStr msg)

worker :: (ProcessId, Integer) -> Process ()
worker (pid, n) = send pid (numPrimeFactors n)
                -- (logDebugSH) (numPrimeFactors n)

remotable ['worker]

-- | Wait for n integers and sum them all up
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
    $(logDebugSH) number

    us <- getSelfPid
    $(logDebugSH) us

    workQueue <- spawnLocal $
        -- Reply with the next bit of work to be done and reconnect in the end
        forM_ (zip [1 .. n] (cycle slaves)) $ \(m, there) -> do
            them <- spawn there ($(mkClosure 'slave) (us, m))
            $(logDebugSH) them
            reconnect them

    -- Wait for the result
    sumIntegers (fromIntegral number)
