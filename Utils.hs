module Utils where

replaceOne :: Eq a => [(a,a)] -> a -> a
replaceOne replacements x = case lookup x replacements of
    Just b  -> b
    Nothing -> x

replace :: Eq a => [(a,a)] -> [a] -> [a]
replace _ [] = []
replace replacements (x:xs) = replaceOne replacements x : replace replacements xs

count :: Eq a => a -> [a] -> Int
count el = foldl (\acc x -> if x == el then succ acc else acc) 0

sublist :: Int -> Int -> [a] -> [a]
sublist start end xs = take (end - start) $ drop start xs

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains el (x:xs)
    | el == x = True
    | otherwise = contains el xs

-- | Is the input a subset of xs? 
subset :: Eq a => [a] -> [a] -> Bool
subset xs = all ((flip contains) xs) 

splitParens :: String -> [String]
splitParens str = aux 0 "" str 
    where
        aux _ "" "" = []
        aux _ curr "" = curr : aux 0 "" ""
        aux 0 curr (' ':str) = curr : aux 0 "" str
        aux depth curr ('(':str) = aux (succ depth) (curr ++ "(") str
        aux depth curr (')':str) = aux (pred depth) (curr ++ ")") str
        aux depth curr (c:str) = aux depth (curr ++ [c]) str

addParens :: String -> String
addParens str = "(" ++ str ++ ")"

removeParens :: String -> String
removeParens = tail . init

indent :: Int -> String -> String
indent amount str = init . unlines $ map (replicate amount ' ' ++) (lines str)
