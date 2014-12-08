import Data.Bool
import Prelude

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

--checks whether a string contains a Char
contains :: Char -> String -> Bool

--removes the first occurence a digit from a string
remove :: Char -> String -> String

--
total :: String -> String -> Integer

silver :: String -> String -> Integer

score :: String -> String -> String

play :: String -> IO()

main :: IO ()

