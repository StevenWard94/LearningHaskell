{- | A haskell script to practice applying functions from Data.* modules
 - Module:        Learn.Data
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 09
 -}

import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . DL.nub

-- implementation of Data.Function.on
on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on'` g = \x y -> f (g x) (g y)

-- implementation of Data.Char.encode
encode' :: Int -> String -> String
encode' shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

-- same function defined with function composition
compEncode' :: Int -> String -> String
compEncode' shift msg = map (chr . (+ shift) . ord) msg

-- implementation of Data.Char.decode
decode' :: Int -> String -> String
decode' shift msg = encode (negate shift) msg

findkey :: (Eq k) => k -> [(k,v)] -> v
findkey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findkey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findkey' key [] = Nothing
findkey' key ((k,v):xs) = if key == k
                             then Just v
                             else findkey' key xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- implementation of Data.Map.fromList
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
