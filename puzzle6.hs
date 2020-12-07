import System.IO
import Data.List

f :: [String] -> [String]
f = (map (foldl (\x y -> x ++ y) [])).splitOnEmpty

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty xs = if remaining == [] 
    then 
        [(takeWhile (/="") xs)]
    else
        (takeWhile (/="") xs):(splitOnEmpty (tail remaining))
    where remaining = (dropWhile (/="") xs)

countQuestions :: [String] -> [Int]
countQuestions groups = map (\x -> (foldl (+) 0 [if y `elem` x then 1 else 0 | y <- ['a'..'z']])) groups

countQuestions' :: [[String]] -> [Int]
countQuestions' groups = map (\x -> (foldl (+) 0 [if and [y `elem` z | z <- x] then 1 else 0 | y <- ['a'..'z']])) groups


main = do
    handle <- openFile "puzzle6.txt" ReadMode
    contents <- hGetContents handle

    putStrLn.show $ foldl (+) 0 $ countQuestions (f.lines $ contents) -- first star
    putStrLn.show $ foldl (+) 0 $ countQuestions' $ splitOnEmpty $ (lines contents) -- second star
    hClose handle