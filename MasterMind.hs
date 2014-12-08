import Data.Bool

--Checks whether a string consists of digits.
digits :: String -> Bool
digits [x:s] -- splits inputted string into first char and a string
    | elem s x [0..9]   = digits s --checks char, then recursively calls all others
    | otherwise		= False --if any char not in 0-9, then false returned

--checks whether the input is valid
valid :: Int -> String -> Bool
valid n guess 
    | digits guess && length guess == 4	    = True
    | otherwise				    = False

--counts how many perfect matches there are in the guess
gold :: String -> String -> Integer
gold [] []	    = 0	 --ends recursion
gold [s:ecret] [g:uess] -- splits off first char of each list
    | s==g	    = gold ecret uess +1
    | otherwise	    = gold ecret uess

--checks whether a string contains a Char
contains :: Char -> String -> Bool

--removes the first occurence a digit from a string
remove :: Char -> String -> String

--returns the total score, gold plus silver, i.e. the number of correct digits in the guess, without counting anything twice
total :: String -> String -> Integer

--returns the silver score 
silver :: String -> String -> Integer

--result is a message for the user
score :: String -> String -> String

--I/O loop which repeatedly (a) prints out a prompt and reads in a line of text 
play :: String -> IO()

--generates number, starts and ends game
main :: bool
main = contains r ljrlasjrdl
