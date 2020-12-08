import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map


execute :: [(String, Int)] -> Int -> [(String, Int)]
execute instructions curr
    | fst (instructions !! curr) == "nop" = ("nop", 0):(execute instructions (curr+1))
    | fst (instructions !! curr) == "acc" = ("acc", snd (instructions !! curr)):(execute instructions (curr+1))
    | otherwise = ("jmp", snd (instructions !! curr)):(execute instructions (curr+(snd (instructions !! curr))))

executeIndex :: [(String, Int)] -> Int -> [Int]
executeIndex instructions curr
    | curr >= (length instructions) = []
    | fst (instructions !! curr) == "nop" = curr:(executeIndex instructions (curr+1))
    | fst (instructions !! curr) == "acc" = curr:(executeIndex instructions (curr+1))
    | otherwise = curr:(executeIndex instructions (curr+(snd (instructions !! curr))))

findRepeatInstruction :: [Int] -> Int
findRepeatInstruction index = (length (takeWhile (==False) (map (helper index) [0..])))

helper :: [Int] -> Int -> Bool
helper index x
    | (drop x index) == [] = True
    | otherwise = (head (drop x index)) `elem` (take x index)

accInfiniteLoop :: [(String, Int)] -> Int
accInfiniteLoop instructions = foldl (+) 0 $ map snd $ (filter (\(x, y) -> x == "acc") $ (take ((findRepeatInstruction (executeIndex instructions 0))) (execute instructions 0)))


doesTerminate :: [(String, Int)] -> Bool
doesTerminate [] = True
doesTerminate instructions = (take 1 $ (drop ((findRepeatInstruction (executeIndex instructions 0)) + 1) $ executeIndex instructions 0)) == []

generateInstructions :: [(String, Int)] -> Int -> [[(String, Int)]]
generateInstructions instructions curr
    | curr >= (length instructions) = []
    | fst (instructions !! curr) == "acc" = (generateInstructions instructions (curr+1))
    | fst (instructions !! curr) == "jmp" = ((take curr instructions) ++ [("nop", 0)] ++ (drop (curr+1) instructions)):(generateInstructions instructions (curr+1))
    | otherwise = ((take curr instructions) ++ [("jmp", snd (instructions !! curr))] ++ (drop (curr+1) instructions)):(generateInstructions instructions (curr+1))

accTerminate :: [(String, Int)] -> Int
accTerminate instructions = accInfiniteLoop $ head $ filter doesTerminate $ generateInstructions instructions 0

parseInstructions :: [String] -> [(String, Int)]
parseInstructions [] = []
parseInstructions raw
    | (words (head raw)) !! 0 == "nop" = ("nop", 0):(parseInstructions (tail raw))
    | (words (head raw)) !! 0 == "acc" = ("acc", move):(parseInstructions (tail raw))
    | otherwise = ("jmp", move):(parseInstructions (tail raw))
    where move = (\(s:x) -> if s == '+' then read x::Int else (-1)*(read x::Int)) ((words (head raw)) !! 1)
    
main = do
    handle <- openFile "puzzle8.txt" ReadMode
    contents <- hGetContents handle
    let instructions = parseInstructions.lines $ contents
    putStrLn.show $ accInfiniteLoop instructions -- first star
    putStrLn.show $ accTerminate instructions -- second star
    hClose handle