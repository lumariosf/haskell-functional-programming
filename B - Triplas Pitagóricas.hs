pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(x, y, z) | x <- [1..n], y <- [x + 1..n], z <- [y + 1..n], x*x + y*y == z*z]

main = do
    s <- getLine
    let result = pitagoras (read s)
    print result