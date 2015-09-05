data Customer = Customer String String deriving (Show)
firstName (Customer fname _) = fname
lastName (Customer _ lname) = lname

firstNames :: [Customer] -> [String]
firstNames [Customer fname _] = [fname]
firstNames (xs) = map firstName xs
