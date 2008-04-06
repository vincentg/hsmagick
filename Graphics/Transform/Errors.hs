module Errors where

import Control.Exception

-- Error handling functions

-- Todo: something better
signalException :: String -> IO a
signalException = throwIO . ErrorCall 

tellUser :: String -> IO ()
tellUser = putStrLn 

debug :: Int -> String -> IO ()
debug n | n <= debugLevel = putStrLn
debug _ = const $ return ()
debugLevel :: Int
debugLevel = 2
