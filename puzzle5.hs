import System.IO
import Data.List

seatRowCol :: String -> (Int, Int)
seatRowCol pass = (foldl (+) 0 (map (\(x, y) -> x*y) (zip (map ((^) 2) [6, 5, 4, 3, 2, 1, 0]) (map (\x -> if x == 'F' then 0 else 1) [pass !! t | t <- [0..6]]))), foldl (+) 0 (map (\(x, y) -> x*y) (zip (map ((^) 2) [2, 1, 0]) (map (\x -> if x == 'L' then 0 else 1) [pass !! t | t <- [7..9]]))))

main = do
    handle <- openFile "puzzle5.txt" ReadMode
    contents <- hGetContents handle

    let input = lines $ contents
    let seatIDs = (map (\(x, y) -> 8*x+y) (map seatRowCol input))
    let maxSeatID = maximum seatIDs
    let minSeatID = minimum seatIDs

    putStrLn.show $ maxSeatID -- first star
    putStrLn.show.head $ [x | x <- [minSeatID..maxSeatID], not (x `elem` seatIDs)] -- second star
    hClose handle