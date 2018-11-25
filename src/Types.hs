module Types where

data SpawnStrategy = SpawnSyncWithReconnect
                   | SpawnSyncNoReconnect
                   | SpawnAsync
    deriving (Show, Read)