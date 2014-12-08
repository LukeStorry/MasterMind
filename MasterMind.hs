import Data.Bool
import System.IO
import System.Random

--Checks whether a string consists of digits.
digits :: String -> Bool
digits []		= True --ends recursion
digits (s:tring) -- splits inputted string into first char and a string
    | elem s ['0'..'9']     = digits tring --checks char, then recursively calls all others
    | otherwise		= False --if any char not in 0-9, then false returned

--checks whether the input is valid
valid :: Int -> String -> Bool
valid n guess 
    | digits guess && length guess == n	    = True
    | otherwise				    = False

--counts how many perfect matches there are in the guess
gold :: String -> String -> Integer
gold [] []     	    = 0	 --ends recursion
gold (s:ecret) (g:uess) -- splits off first char of each list
    | s==g	    = gold ecret uess +1
    | otherwise	    = gold ecret uess

--checks whether a string contains a Char
contains :: Char -> String -> Bool
contains _ []       = False  --ends recursion
contains c (s:ecret)
    | c==s          = True
    | otherwise     = contains c ecret

--removes the first occurence a digit from a string
remove :: Char -> String -> String
remove c []	= []
remove c (s:tring)
    | c==s	= tring
    | c/=s	= s:(remove c tring)

--returns the total score, gold plus silver, i.e. the number of correct digits in the guess, without counting anything twice
total :: String -> String -> Integer
total _ []		    = 0
total (secret) (g:uess) 
    | contains g secret	    = (total (remove g secret) (g:uess) ) +1 
    | otherwise		    = total secret uess 

--returns the silver score 
silver :: String -> String -> Integer
silver secret guess = (total secret guess) - (gold secret guess)

--result is a message for the user
score :: String -> String -> String
score secret guess
    | valid 4 guess = "Golds "++show(gold secret guess)++", silvers "++show(silver secret guess)
    | otherwise	    = "Bad guess"

--I/O loop which repeatedly (a) prints out a prompt and reads in a line of text 
play :: String -> IO()
play secret = do
	putStr "\n> "
	hFlush stdout
	guess <- getLine
	if ((guess \= []) && (guess \= secret))
	    then do
		putStr (score secret guess)  
		play secret    

--using: https://www.haskell.org/tutorial/io.html
-- and http://stackoverflow.com/questions/5695649/

--generates number, starts and ends game
main :: IO()
main = do
    secret <- show(randomRIO 0 9)++show(randomRIO 0 9) ++ show(randomRIO 0 9)++ show(randomRIO 0 9)
    putStr "Guess the 4 digit secret, press Enter to give up"
    play secret
    putStr ("The secret was "++secret)

