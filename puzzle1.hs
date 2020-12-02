import System.IO

cartProd :: [Int] -> [Int] -> [(Int, Int)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

cartProd3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
cartProd3 xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

f :: [String] -> [Int]
f = map read

multTupleElements :: (Int, Int) -> Int
multTupleElements (a,b) = a*b

multTupleElements3 :: (Int, Int, Int) -> Int
multTupleElements3 (a,b, c) = a*b*c

main = do
    handle <- openFile "puzzle1.txt" ReadMode
    contents <- hGetContents handle
    let input = (f (lines contents))
    putStrLn (show (multTupleElements (head (filter (\(x,y) -> x+y == 2020) (cartProd input input))))) -- first star
    putStrLn (show (multTupleElements3 (head (filter (\(x,y,z) -> x+y+z == 2020) (cartProd3 input input input))))) -- second star
    hClose handle