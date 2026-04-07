-- produto elemento por elemento entre as listas
-- se tam diff entao mul por 0
mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 (x : xs) (y : ys) = x * y : mul2 xs ys
mul2 [] (_:ys) = 0 : mul2 [] ys
mul2 (_:xs) [] = 0 : mul2 xs []

main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result