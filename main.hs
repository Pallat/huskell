sum' :: Int -> Int
sum' 0 = 0
sum' n = n + sum' (n-1)

fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

text "" = "Hi"
text s = "Hi " ++ s ++ " krub"
