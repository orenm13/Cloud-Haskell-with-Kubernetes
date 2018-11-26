{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterSlave where

import "base"                Control.Monad
import "distributed-process" Control.Distributed.Process
import "distributed-process" Control.Distributed.Process.Closure
import "monad-logger"        Control.Monad.Logger
import "monad-logger-syslog" System.Log.MonadLogger.Syslog
import "random"              System.Random (randomRIO)
import Utils

instance MonadLogger Process where
    monadLoggerLog loc logSource logLevel msg
        = liftIO $ defaultSyslogOutput loc logSource logLevel (toLogStr msg)

slave :: (ProcessId, ProcessId) -> Process ()
slave (master, workQueue) = do
    us <- getSelfPid
    go us
    where
        go us = do
            -- Ask the queue for work
            send workQueue us

            -- If there is work, do it, otherwise terminate
            receiveWait
                [ match $ \n  -> do
                    calculatedFactors <- liftIO $ numPrimeFactors n
                    send master calculatedFactors >> go us
                    $(logDebugSH) calculatedFactors
                , match $ \() -> return ()
                ]

remotable ['slave]

-- | Wait for n integers and sum them all up
sumIntegers :: Int -> Process Integer
sumIntegers = go 0
    where
        go :: Integer -> Int -> Process Integer
        go acc 0 = return acc
        go acc n = do
            m <- expect
            go (acc + m) (n - 1)

master :: (Integer, Integer) -> [NodeId] -> Process Integer
master (n, m) slaves = do
    -- get a random number in the given range
    number <- liftIO $ randomRIO (n, m)
    $(logDebugSH) number

    us <- getSelfPid

    workQueue <- spawnLocal $ do
        -- Reply with the next bit of work to be done
        forM_ [1 .. number] $ \k -> do
            them <- expect
            send them k

        -- Once all the work is done, tell the slaves to terminate
        forever $ do
            pid <- expect
            send pid ()

    $(logDebugSH) workQueue

    -- Start slave processes
    forM_ slaves $ \nid -> spawn nid ($(mkClosure 'slave) (us, workQueue))

    -- Wait for the result
    sumIntegers (fromIntegral number)
