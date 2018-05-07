import Data.Char

data Tape a = Tape [a] a [a] deriving (Show)

emptyDataTape :: Tape Char
emptyDataTape = Tape [] (chr 0) zero 
    where zero = repeat (chr 0)

makeInstructionTape :: String -> Tape Char 
makeInstructionTape string = Tape [] (head string) (tail string)

printTape :: Tape Char -> Char 
printTape (Tape _ m _) = m

moveRight :: Tape a -> Tape a
moveRight (Tape ls m (r:rs)) = Tape (ls ++ [m]) r rs

moveRightInst :: Tape Char -> Tape Char 
moveRightInst (Tape ls m []) = Tape (ls ++ [m]) '0' []
moveRightInst tape@(Tape ls m (r:rs)) = moveRight tape 

moveLeft :: Tape a -> Tape a
moveLeft (Tape [] m rs) = error "Went too far to the left of the tape"
moveLeft (Tape ls m rs) = Tape (init ls) (last ls) (m:rs)

seekLeft inst@(Tape ls m _)
    | last ls == '[' = inst
    | otherwise = seekLeft (moveLeft inst)

seekRight inst@(Tape ls m _)
    | last ls == ']' = inst 
    | otherwise = seekRight (moveRightInst inst)

doFunc dataTape@(Tape _ m _) inst@(Tape _ '[' _)
    | m == chr 0 = doFunc dataTape (seekRight inst) 
    | otherwise = doFunc dataTape (moveRightInst inst)

doFunc dataTape@(Tape _ m _) inst@(Tape _ ']' _) 
    | m /= chr 0 = doFunc dataTape (seekLeft inst)
    | otherwise = doFunc dataTape (moveRightInst inst)

--End condition
doFunc dataTape inst@(Tape _ '0' _) = return ()

--We need to perform the + operation
doFunc dataTape@(Tape ls m rs) inst@(Tape _ '+' _) = 
    doFunc (Tape ls (succ m) rs) (moveRightInst inst)

--We need to perform the - operation
doFunc dataTape@(Tape ls m rs) inst@(Tape _ '-' _) = 
    doFunc (Tape ls (pred m) rs) (moveRightInst inst)

doFunc dataTape inst@(Tape _ '<' _) = 
    doFunc (moveLeft dataTape) (moveRightInst inst)

doFunc dataTape inst@(Tape _ '>' _) = 
    doFunc (moveRight dataTape) (moveRightInst inst) 

doFunc dataTape@(Tape _ m _) inst@(Tape _ '.' _) = do 
    putChar(m)
    doFunc dataTape (moveRightInst inst) 

doFunc dataTape@(Tape ls _ rs) inst@(Tape _ ',' _) = do 
    m <- getChar 
    doFunc (Tape ls m rs) (moveRightInst inst)

main = do
    let myTape = emptyDataTape
    print $ printTape myTape