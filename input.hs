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



      
main = forever $ do  
    l <- getLine 
    g <- getLine
    let strl = show(l)
    let strg = show(g)
    --putStrLn l
    --putStrLn ("" ++ show (g)) 
    putStrLn(check strl strg)
    print (strl=="ROCK")
