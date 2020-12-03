import System.IO

nextTobogganCoord :: Int -> Int -> (Int, Int) -> (Int, Int)
nextTobogganCoord down right current = (down + (fst current), right + (snd current))

f :: [String] -> [String]
f = map cycle 

countTrees :: [String] -> [(Int, Int)] -> Int
countTrees forest coords = length $ filter (\x -> x == '#') [((forest !! (fst c)) !! (snd c)) | c <- coords]

main = do
    handle <- openFile "puzzle3.txt" ReadMode
    contents <- hGetContents handle

    -- first star
    let forest = f.lines $ contents
    let coords = take ((length forest) `div` 1) $ iterate (nextTobogganCoord 1 3) (0, 0)
    putStrLn.show $ countTrees forest coords

    -- second star
    let slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
    putStrLn.show $ foldl (*) 1 [countTrees forest $ take ((length forest) `div` (fst slope)) $ iterate (nextTobogganCoord (fst slope) (snd slope)) (0, 0) | slope <- slopes] -- snd star
    hClose handle