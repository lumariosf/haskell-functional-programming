vazio :: String
vazio = ""

toDouble :: String -> Double
toDouble s = read s :: Double

-- sortar resposta
inserir :: (String, Double) -> [(String, Double)] -> [(String, Double)]
inserir (time, kda) [] = [(time, kda)]
inserir (time, kda) ((time1, kda1) : xs)
    | (kda, time) >= (kda1, time1)  = (time, kda) : (time1, kda1) : xs
    | otherwise                     = (time1, kda1) : inserir (time, kda) xs

ordena :: [(String, Double)] -> [(String, Double)]
ordena [] = []
ordena ((time, kda):xs) = inserir (time, kda) (ordena xs)

-- arredondar pra duas casas decimais
arredondar :: Double -> Double
arredondar x = fromIntegral (round (x * 100)) / 100

resposta :: String -> [(String, String, Double, Double, Double)] -> [(String, Double)]
resposta meta [] = []
resposta meta ((a,b,c,d,e) : xs)
    | meta == a     = (b, arredondar kda) : resposta meta xs
    | otherwise     = resposta meta xs
    where
        kda = if d == 0 then c + e else (c + e) / d

processar :: String -> [(String, String, Double, Double, Double)] 
processar s = separacao (divisao vazio s)

separacao :: [String] -> [(String, String, Double, Double, Double)]
separacao [] = []
separacao ("":xs) = separacao xs
separacao (a:b:c:d:e:xs) = (a, b, toDouble c, toDouble d, toDouble e) : separacao xs

divisao :: String -> String -> [String]
divisao p [] = [p]
divisao p (x:xs) 
    | x == ';'      =  p : divisao vazio xs
    | otherwise     =  divisao (p ++ [x]) xs

rankTime :: String -> String -> [(String, Double)]
rankTime time s = ordena (resposta time (processar s))

main = do
    time <- getLine
    s <- getLine
    let result = rankTime time s
    print result