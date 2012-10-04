module Default where

import Data.Maybe (isJust, fromJust, isNothing)

class Default n where
    defaultValue :: n
    run :: (a -> Maybe n) -> a -> n
    run f x
        | isNothing y = defaultValue
        | otherwise   = fromJust y
        where y = f x
    mapRun :: (a -> Maybe n) -> [a] -> n
    mapRun f xs
        | null ys   = defaultValue
        | otherwise = fromJust $ head ys
        where ys    = filter isJust $ map f xs
