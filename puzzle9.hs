import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map

findAnomaly :: [Int] -> Int -> Int
findAnomaly xmas window = (!!) xmas $ head $ filter (\x -> not $ (xmas !! x) `elem` [a + b | a <- (take window $ drop (x-window) $ xmas), b <- (take window $ drop (x-window) $ xmas), a /= b]) [window..((length xmas) - 1)]

findWeakness :: [Int] -> Int -> Int
findWeakness xmas anomaly = (maximum solution)+(minimum solution) 
    where solution = head $ filter (flip helper anomaly) $ generateContiguous xmas 0

helper :: [Int] -> Int -> Bool
helper cont anomaly
    | length cont < 2 = False
    | otherwise = ((foldl (+) 0 cont) == anomaly)

generateContiguous:: [Int] -> Int -> [[Int]]
generateContiguous xmas curr
    | curr >= (length xmas) = [[]]
    | otherwise = (map (flip take (drop curr xmas)) [1..(length $ drop curr xmas)]) ++ (generateContiguous xmas (curr+1))

f :: [String] -> [Int]
f = map read

main = do
    handle <- openFile "puzzle9.txt" ReadMode
    contents <- hGetContents handle
    let input = f.lines $ contents
    putStrLn.show $ findAnomaly input 25 -- first star
    putStrLn.show $ findWeakness input (findAnomaly input 25) -- second star
    hClose handle