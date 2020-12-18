import Control.Monad.State

-- Question 4 Begin

quickSort [] = []
quickSort (x:xs) =
    if xs==[] then [x]
    else
        let
            partitioned = partition (init $ x:xs) [] [last $ x:xs]
        in (quickSort $ fst partitioned) ++ (quickSort $ snd partitioned)
    where
        partition [] ls rs = (ls, rs)
        partition (x:xs) ls rs =
            if x>(head rs) then partition xs ls (rs++[x])
            else partition xs (ls++[x]) rs

isPermutation xs ys = isPermutation' (quickSort xs) (quickSort ys)
    where
        isPermutation' [] [] = True
        isPermutation' [] (h:hs) = False
        isPermutation' (h:hs) [] = False
        isPermutation' (h:hs) (t:ts) =
            if h==t then isPermutation' hs ts
            else False

-- Question 4 End


-- Question 5 begin

genPermutations [] = [[]]
genPermutations ys = [a:x | a <- ys, x <- (genPermutations $ filter (\y -> y /= a) ys)]


-- Question 5 end


-- Question 6 begin

fisrtn fname n = do
    str <- readFile fname 
    putStr $ unlines $ take n (lines str)

-- Question 6 end


-- Question 7 begin


data Rank = Numbered Int | Jack | Queen | King | Ace deriving (Show,Eq)
data Suit = Hearts|Clubs|Diamonds|Spades deriving (Show,Eq)
data Card = Card Rank Suit | Joker deriving (Show,Eq)

score x = 
    case x of
        Card (Numbered x) s ->
            [x]
        Card Jack s ->
            [11]
        Card Queen s ->
            [12]
        Card King s ->
            [13]
        Card Ace s ->
            [20]
        Joker -> [11,12,13,20]


scores :: [Card] -> [Int]
scores [] = [0]
scores (x:xs) = 
    let s1 = score x
        s2 = scores xs
    in 
        [(\y -> y+s3) | s3 <- s1] <*> s2

-- Question 7 end


-- Question 8 begin
data Next a = Item [a] | Empty String
begin :: State ([a], Int) () 
begin = do
    (ns, n) <- get
    put (ns,0)
    return ()

isOver :: State ([a], Int) Bool 
isOver = do
    (ns,n) <- get
    if n >= length ns then return True else return False 

peek :: State ([a], Int) a
peek  = do
    (ns, n) <- get
    put (ns,n)
    return (ns!!n)

next :: State ([a], Int) (Next a)
next  = do
    (ns,n) <- get 
    let ret = if n < length ns then Item [ns!!n] else Empty "Nothing"
    put (ns,n+1)
    return ret

instance Show a => Show(Next a) where
    show (Item [str]) = show str
    show (Empty str) = show str

main = do
    putStrLn $ show $ flip evalState (["my", "name", "is", "Archies"], 0) (do
        begin
        next
        next
        isOver)

-- Question 8 end