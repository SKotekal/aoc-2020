import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map

f :: [String] -> [String]
f = (map (foldl (\x y -> x ++ y) [])).splitOnEmpty

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty xs = if remaining == [] 
    then 
        [(takeWhile (/="") xs)]
    else
        (takeWhile (/="") xs):(splitOnEmpty (tail remaining))
    where remaining = (dropWhile (/="") xs)

countBags :: [String] -> String -> Int
countBags rules [] = 0
countBags rules color = foldl (+) 0 (map (\x -> if x then 1 else 0) [x `elem` containers | x <- colors rules]) where containers = containerBags (parseRules rules) color 

containerBags :: [(String, String, Int)] -> String -> [String]
containerBags rules [] = []
containerBags rules color = containers ++ (concat (map (containerBags rules) containers)) where containers = [outer | (outer, inner, num) <- rules, inner == color]

parseRules :: [String] -> [(String, String, Int)]
parseRules rawRules = concat $ map parseRule rawRules

ruleNoOtherBag :: String -> Bool
ruleNoOtherBag rule = (length (words rule) == 7)

parseRule :: String -> [(String, String, Int)]
parseRule rule
    | ruleNoOtherBag rule = [(((words rule) !! 0) ++ " " ++ ((words rule) !! 1), "", 0)]
    | otherwise = [(((words rule) !! 0) ++ " " ++ ((words rule) !! 1), ((words rule) !! (c+1)) ++ " " ++ ((words rule) !! (c+2)), read ((words rule) !! c)) | c <- [4*inner | inner <- [1..(((length (words rule)) - 4)`div`4)]]]

colors :: [String] -> [String]
colors rules = map (\x -> ((words x) !! 0) ++ " " ++ ((words x) !! 1)) rules

innerBags :: [(String, String, Int)] -> String -> [(String, Int)]
innerBags rules [] = []
innerBags rules color = [(inner, num) | (outer, inner, num) <- rules, outer == color]

totalBags :: [String] -> String -> Int
totalBags rules [] = 0
totalBags rules color = foldl (+) 0 (map (\(x, y) -> y+y*(totalBags rules x)) innards) where innards = innerBags (parseRules rules) color


main = do
    handle <- openFile "puzzle7.txt" ReadMode
    contents <- hGetContents handle
    let rules = lines contents
    putStrLn.show $ countBags rules "shiny gold" -- first star
    putStrLn.show $ totalBags rules "shiny gold" -- second star
    hClose handle