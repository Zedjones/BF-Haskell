import Data.Char

data Tape = Tape [Char] Char [Char] deriving (Show)

emptyTape :: Tape 
emptyTape = Tape zero (chr 0) zero 
    where zero = repeat (chr 0)

printTape :: Tape -> Char 
printTape (Tape ls m rs) = m

moveRight :: Tape -> Tape
moveRight(Tape ls m (r:rs)) = Tape (ls ++ [m]) r rs

moveLeft :: Tape -> Tape 
moveLeft(Tape (l:ls) m rs) = Tape ls l ([m] ++ rs)

doFunc (Tape ls m rs) func = 
    case func of 
        '+' -> Left $ Tape ls (chr $ (ord m) + 1) rs
        '-' -> Left $ Tape ls (chr $ (ord m) - 1) rs
        '<' -> Left $ moveLeft $ Tape ls m rs 
        '>' -> Left $ moveRight $ Tape ls m rs 
        '.' -> Right $ putChar m 
        ',' -> Left $ Tape ls (chr $ ord (getChar)) rs

main = do 
    let myTape = emptyTape
    print $ printTape myTape