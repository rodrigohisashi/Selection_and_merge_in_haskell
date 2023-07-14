module Main (main) where

import Test.QuickCheck
import GHC.Base (TrName(TrNameD))


-- Decide se todos os valores lógicos de uma lista são True
and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) = x && and2 xs 

-- Concatena uma lista de listas
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs:xss) = xs ++ concat2 xss 

-- Produz uma lista com n valores idênticos
replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n x = x : replicate2 (n-1) x

replicate3 n x = foldr (\_ acc -> x : acc) [] [1..n] 

replicate5 n x = map (const x) [1..n]

-- Seleciona o enésimo elemento de uma lista
(!!!) :: [a] -> Int -> a
[] !!! _ = error "lista VAZIA"
(x:_) !!! 0 = x
(_:xs) !!! i = xs !!! (i-1)



-- Verifica se um valor é um elemento de uma lista
ele2m :: Eq a => a -> [a] -> Bool
ele2m _ [] = False
ele2m e (x:xs) = x == e || ele2m e xs


minimo :: Ord a => [a] -> a
minimo (x:xs) = minimo' x xs
  where 
    minimo' mi [] = mi
    minimo' mi (y:ys) | mi <= y = minimo' mi ys
                      | otherwise = minimo' y ys


minimo2 (x:xs) = foldr min x xs


descarta :: Eq a  => a -> [a] -> [a]
descarta e [] = []
descarta e (x:xs)
  | e == x = xs
  | otherwise = x : descarta  e xs

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = 
  m : selectionSort (descarta m xs)
  where 
    m = minimo xs


prop_tamanho :: Ord a => [a] -> Bool
prop_tamanho xs = length xs == length (selectionSort xs)

prop_ordem :: Ord a => [a] -> Bool
prop_ordem [] = True
prop_ordem [_] = True
prop_ordem ys =  x0 <= x1 && prop_ordem (x1:xs)
  where
 (x0:x1:xs) = selectionSort ys

prop_idempotencia :: Ord a => [a] -> Bool
prop_idempotencia xs = selectionSort xs == selectionSort (selectionSort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys =  ys
merge xs [] = xs
merge (x:xs) (y:ys) 
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

quebra :: [a] -> ([a], [a])
quebra xs = quebra' xs [] []
  where 
    quebra' [] zs ws = (zs, ws)
    quebra' (y:ys) zs ws = quebra' ys (y:ws) zs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort pt1) (mergeSort pt2)
  where (pt1, pt2) = quebra xs


prop_merge_selection :: Ord a => [a] -> Bool
prop_merge_selection xs = mergeSort xs == selectionSort xs




main :: IO ()
main = do
  quickCheck (prop_tamanho :: [Integer] -> Bool)
  quickCheck (prop_idempotencia :: [String] -> Bool)
  quickCheck (prop_ordem :: [Integer] -> Bool)


