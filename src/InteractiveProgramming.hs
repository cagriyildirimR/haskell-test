
act :: IO (Char, Char)
act = do x <- getChar
         y <- getChar
         return (x,y)

strLen :: IO ()
strLen = do putStr "Enter a string: "
            xs <- getLine
            putStr (show ([1..10]))
            putStr "The string has "
            putStr (show (length xs))
            putStr " characters \n"
