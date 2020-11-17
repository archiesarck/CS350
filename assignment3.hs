-- Question 1

-- quickSort :: (Ord a) => [a]->[a]
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


-- Question 2

-- uniq :: (Eq a) => [a] -> [a]
uniq xs = uniq' xs []
    where
        uniq' [] ys = ys
        uniq' (x:xs) ys =
            if (findx x ys) then uniq' xs ys
            else uniq' xs (ys ++ [x])
            where
                findx key [] = False
                findx key (l:ls) =
                    if l==key then True
                    else findx key ls


-- Question 3

-- neighbors :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> [(a1, a2)]
neighbors i j = quickSort $ remove [(i-1, j-1), (i-1, j), (i-1, j+1), (i, j-1), (i, j+1), (i+1, j-1), (i+1, j), (i+1, j+1)] []
    where
        remove [] ys = ys
        remove (x:xs) ys =
            if ((fst x)<0 || (fst x)>8) then remove xs ys
            else if ((snd x)<0 || (snd x)>8) then remove xs ys
            else remove xs (ys++[x])


-- Question 4

-- noOfWords :: String -> Int
noOfWords s = length $ words s


-- Question 5

-- compose_multiple :: [b -> b] -> b -> b
compose_multiple [] val = val
compose_multiple (f:fs) val = compose_multiple (init $ f:fs) $ (last $ f:fs) val


-- Question 6

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Nil deriving Show

-- maptree :: (a->b) -> BinaryTree a -> BinaryTree b
maptree fn Nil = Nil
maptree fn (Node x left right) = Node (fn x) (maptree fn left) (maptree fn right)

-- foldTree :: (a->b->b->b) -> b -> BinaryTree a -> b
foldTree fn identity Nil = identity
foldTree fn identity (Node x left right) = fn x (foldTree fn identity left) (foldTree fn identity right)