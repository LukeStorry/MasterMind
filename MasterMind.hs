
--Checks whether a string consists of digits.
digits :: String -> Bool
digits [x:s] -- splits inputted string into first char and a string
    | elems x [0..9]    = digits s --checks char, then recursively calls all others
        | otherwise         = false --if any char not in 0-9, then false returned

--checks whether the input is valid
valid :: Int -> String -> Bool
valid n guess 
    | digits guess && length guess == 4	    = true
    | otherwise				    = false



