soma :: [Double] -> Double
soma [] = 0
soma (x : xs) = x + soma xs

processaAlunos :: [(String, [Double])] -> [(String, Double, String)]
processaAlunos [] = []
processaAlunos (((nomes, notas): xs)) 
    | (soma notas / fromIntegral (length notas)) >= 7   = (nomes, media, "Aprovado") : processaAlunos xs
    |(soma notas / fromIntegral (length notas)) >= 5    = (nomes, media, "Recuperacao") : processaAlunos xs
    | otherwise                                         = (nomes, media, "Reprovado") : processaAlunos xs
    
    where
      media = soma notas / fromIntegral(length notas)

main = do
    s <- getLine
    let entrada = read s :: [(String, [Double])]
    let result = processaAlunos entrada
    print result