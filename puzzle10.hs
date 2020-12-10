import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map

computeJolts :: [Int] -> Int
computeJolts adapters = (length $ filter (\y -> y == 3) (map (\x -> ((chain !! (x+1)) - (chain !! x))) [0..((length chain)-2)]))*((length $ filter (\y -> y == 1) (map (\x -> ((chain !! (x+1)) - (chain !! x))) [0..((length chain)-2)]))) where chain = (Data.List.sort (0:((maximum adapters)+3):adapters))

count :: [Int] -> [Int] -> Int -> Int
count adapters counts curr
    | (curr >= (length adapters)) = (last counts)
    | otherwise = count adapters (counts ++ [(foldl (+) 0 [counts !! x | x <- [0..(curr-1)], (viable (adapters !! curr) (adapters !! x))])]) (curr+1)

viable :: Int -> Int -> Bool
viable adapter source = ((0 <= adapter-source) && (adapter-source <= 3))

f :: [String] -> [Int]
f = map read

main = do
    handle <- openFile "puzzle10.txt" ReadMode
    contents <- hGetContents handle
    let input = f.lines $ contents
    putStrLn.show $ computeJolts input -- first star
    putStrLn.show $ count (Data.List.sort (0:((maximum input)+3):input)) [1] 1 -- second star
    hClose handle