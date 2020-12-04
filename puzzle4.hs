import System.IO
import Data.List

f :: [String] -> [String]
f = (map (foldl (\x y -> x ++ " " ++ y) [])).splitOnEmpty

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty xs = if remaining == [] 
    then 
        [(takeWhile (/="") xs)]
    else
        (takeWhile (/="") xs):(splitOnEmpty (tail remaining))
    where remaining = (dropWhile (/="") xs)

fieldsPresent :: String -> Bool
fieldsPresent passport = foldl (&&) True (map (flip isInfixOf passport) fields) where fields = ["byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"]

fieldsCorrect :: String -> Bool
fieldsCorrect passport = foldl (&&) True $ map checkField $ map splitFieldEntry $ words passport

checkField :: (String, String) -> Bool
checkField (field, entry)
    | field == "byr" = (((read entry::Int) >= 1920) && ((read entry::Int) <= 2002))
    | field == "iyr" = (((read entry::Int) >= 2010) && ((read entry::Int) <= 2020))
    | field == "eyr" = (((read entry::Int) >= 2020) && ((read entry::Int) <= 2030))
    | field == "hgt" = ((isInfixOf "cm" entry) && (((read (init.init $ entry)::Int) >= 150) && ((read (init.init $ entry)::Int) <= 193))) || ((isInfixOf "in" entry) && (((read (init.init $ entry)::Int) >= 59) && ((read (init.init $ entry)::Int) <= 76)))
    | field == "hcl" = ((head entry) == '#') && (foldl (&&) True ((map (flip elem (['a'..'f']++(map head $ map show [0..9])))) $ tail entry))
    | field == "ecl" = elem entry ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    | field == "pid" = ((length entry) == 9) && (foldl (&&) True (map (flip elem (map head $ map show [0..9])) $ entry))
    | field == "cid" = True

splitFieldEntry :: String -> (String, String)
splitFieldEntry s = ((takeWhile (/=':') s), tail (dropWhile (/=':') s))

main = do
    handle <- openFile "puzzle4.txt" ReadMode
    contents <- hGetContents handle

    let input = f.lines $ contents
    putStrLn.show $ foldl (+) 0 (map (\x -> if x == True then 1::Int else 0::Int) (map fieldsPresent input)) -- first star

    putStrLn.show $ foldl (+) 0 (map (\x -> if x == True then 1::Int else 0::Int) (map (\p -> ((fieldsPresent p) && (fieldsCorrect p))) input)) -- second star
    hClose handle