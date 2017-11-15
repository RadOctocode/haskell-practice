import Control.Monad  
import Data.Char  
check :: String -> String ->String
check "SCISSOR" x
    | x=="PAPER" = "WIN"
    | x=="SCISSOR" = "TIE"
    | x=="ROCK" = "LOSE"

check "PAPER" x
    | x=="PAPER" = "TIE"
    | x=="SCISSOR" = "LOSE"
    | x=="ROCK" = "WIN"


check "ROCK" x
    | x=="PAPER" = "LOSE"
    | x=="SCISSOR" = "WIN"
    | x=="ROCK" = "TIE"

check y x = "THIS IS BAD INPUT HOW DARE YOU"

--play :: String -> maybe bool
--play x
--    | x=="YES" = True
--    | x=="NO" = False
--    otherwise nothing
      
main = forever $ do  
    l <- getLine 
    g <- getLine
    putStrLn(check l g)

