type Comando = String
type Valor = Int

valor :: Int
valor = -666

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa lista = auxiliar 0 lista

auxiliar :: Int -> [(Comando, Valor)] -> Int
auxiliar y [] = y
auxiliar y ((comando, x) : xs)
    | comando == "Multiplica"               = auxiliar (y * x) xs
    | comando == "Soma"                     = auxiliar (y + x) xs
    | comando == "Subtrai"                  = auxiliar (y - x) xs
    | comando == "Divide" && valor == 0     = valor
    | otherwise                             = auxiliar (y div x) xs

main = do
    a <- getLine
    let result = executa (read a)
    print result