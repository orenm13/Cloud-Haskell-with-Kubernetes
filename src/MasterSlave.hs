{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module MasterSlave where

import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Types

primes :: [Integer]
primes = primes' (2:[3,5..])
  where
    primes' (x:xs) = x : primes' (filter (notDivisorOf x) xs)
    notDivisorOf d n = n `mod` d /= 0

factors :: [Integer] -> Integer -> [Integer]
factors qs@(p:ps) n
    | n <= 1 = []
    | m == 0 = p : factors qs d
    | otherwise = factors ps n
  where
    (d,m) = n `divMod` p

primeFactors :: Integer -> [Integer]
primeFactors = factors primes

numPrimeFactors :: Integer -> Integer
numPrimeFactors = fromIntegral . length . primeFactors

--------------------------------------------------------------------

slave :: (ProcessId, Integer) -> Process ()
slave (pid, n) = send pid (numPrimeFactors n)

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

master :: Integer -> SpawnStrategy -> [NodeId] -> Process Integer
master n spawnStrategy slaves = do
    us <- getSelfPid

    -- Distribute 1 .. n amongst the slave processes
    spawnLocal $ case spawnStrategy of
        SpawnSyncWithReconnect ->
            forM_ (zip [1 .. n] (cycle slaves)) $ \(m, there) -> do
                them <- spawn there ($(mkClosure 'slave) (us, m))
                reconnect them
        SpawnSyncNoReconnect ->
            forM_ (zip [1 .. n] (cycle slaves)) $ \(m, there) -> do
                _them <- spawn there ($(mkClosure 'slave) (us, m))
                return ()
        SpawnAsync ->
            forM_ (zip [1 .. n] (cycle slaves)) $ \(m, there) -> do
                spawnAsync there ($(mkClosure 'slave) (us, m))
                _ <- expectTimeout 0 :: Process (Maybe DidSpawn)
                return ()

    -- Wait for the result
    sumIntegers (fromIntegral n)

