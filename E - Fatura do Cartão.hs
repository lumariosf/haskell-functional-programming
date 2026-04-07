import Data.List (foldl')

toDouble :: String -> Double
toDouble s = read s :: Double

toInt :: String -> Int
toInt s = read s :: Int

resposta :: String -> [(Int, String, String, Double)] -> Double
resposta meta xs = foldl' f 0 xs
  where
    f acc (_, b, _, d)
        | b == meta = acc + d
        | otherwise = acc

processar :: String -> [(Int, String, String, Double)]
processar s = separacao (divisao "" s)

separacao :: [String] -> [(Int, String, String, Double)]
separacao [] = []
separacao ("":xs) = separacao xs
separacao (a:b:c:d:xs) = (toInt a, b, c, toDouble d) : separacao xs

divisao :: String -> String -> [String]
divisao p [] = [p]
divisao p (x:xs)
    | x == ' ' || x == ';'  = p : divisao "" xs
    | otherwise             = divisao (p ++ [x]) xs

logMes :: String -> String -> Double
logMes meta s = resposta meta (processar s)

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result