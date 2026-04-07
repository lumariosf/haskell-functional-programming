-- inserir elemento na lista
inserir :: (String, Int) -> [(String, Int)] -> [(String, Int)]
inserir (nome, val) [] = [(nome, val)]
inserir (nome, val) ((nome1, val1) : xs)
    | (nome, val) <= (nome1, val1)  = (nome, val) : (nome1, val1) : xs
    | otherwise                     = (nome1, val1) : inserir (nome, val) xs

-- ordenar as tuplas alfabeticamente e dps pelo num
ordenaAlunos :: [(String, Int)] -> [(String, Int)]
ordenaAlunos [] = []
ordenaAlunos ((nome, val) : xs) = inserir (nome, val) (ordenaAlunos xs)


main = do
       a <- getLine
       let result = ordenaAlunos (read a :: [(String, Int)])
       print result