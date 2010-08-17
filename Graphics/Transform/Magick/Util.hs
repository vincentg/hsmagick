{-# OPTIONS -Wall #-}
module Graphics.Transform.Magick.Util(
    allM, butLast, assertM, commaSep, groups) where

import Data.List
import Control.Exception

------------------
-- Monad utilities
------------------

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f xs = mapM f xs >>= (return . and)

assertM :: Bool -> String -> IO ()
assertM True _  = return ()
assertM False s = throwIO (AssertionFailed s)

-----------------
-- List utilities
-----------------

-- returns an empty list if passed the empty list
butLast :: [a] -> [a]
butLast = reverse.safeTail.reverse

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

sepBy :: [a] -> [[a]] -> [a]
sepBy separator things = concat (intersperse separator things)

commaSep :: Show a => [a] -> String
commaSep xs = sepBy "," (map show xs)

groups :: Integral a => a -> [b] -> [[b]]
groups n xs | genericLength xs <= n = [xs]
groups n xss = (genericTake n xss):(groups n (genericDrop n xss))
