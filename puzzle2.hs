import System.IO

checkPolicy :: (Int, Int, Char, String) -> Int
checkPolicy (lbound, ubound, c, password) = if and [(occurences >= lbound), (occurences <= ubound)] then 1 else 0 where occurences = length [x | x <- password, x == c]

checkPolicy' :: (Int, Int, Char, String) -> Int
checkPolicy' (lbound, ubound, c, password) = if ((fst result) /= (snd result)) && ((fst result) || (snd result)) then 1 else 0 where result = (password !! (lbound-1) == c, password !! (ubound-1) == c)

f :: [String] -> [(Int, Int, Char, String)]
f = map parse

parse :: String -> (Int, Int, Char, String)
parse input = (read (takeWhile (\x -> x /= '-') (input_tokenized !! 0)), read (dropWhile (\x -> x == '-') (dropWhile (\x -> x /= '-') (input_tokenized !! 0))), head $ takeWhile (\x -> x /= ':') (input_tokenized !! 1), (input_tokenized !! 2)) where input_tokenized = words input

main = do
    handle <- openFile "puzzle2.txt" ReadMode
    contents <- hGetContents handle
    let input = (f (lines contents))
    putStrLn.show $ (foldl (+) 0 (map checkPolicy (input))) -- first star
    putStrLn.show $ (foldl (+) 0 (map checkPolicy' (input))) -- second star
    hClose handle