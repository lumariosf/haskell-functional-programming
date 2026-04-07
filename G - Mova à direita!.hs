-- constroi string com quantidade n de espaços
addEspacos :: Int -> String
addEspacos n 
    | n == 0        = ""
    | otherwise     = addEspacos (n - 1) ++ " "

-- adicionar quantidade n de espaços à esquerda de um dado string
paraDireita :: Int -> String -> String
paraDireita n p = addEspacos n ++ p

parseInput str = let [n, s] = words str
                 in (read n, s)
main :: IO()
main = interact $ uncurry paraDireita . parseInput