-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse


-- Problem 2
myButLast :: [a] -> a
myButLast xs
    | len < 2 = error "Too few elements!"
    | otherwise = xs !! (len - 2)
    where len = length xs


-- Problem 3
elementAt :: [a] -> Int -> a
elementAt list index
    | index < 1 = error "Index must be a positive number!"
    | length list < index = error "List index out of range!"
    | otherwise = myLast $ take index list
    -- But this doesn't work for infinite lists!

elementAt' :: [a] -> Int -> a
elementAt' list index 
    | index < 1 = error "Index must be a positive number!"
    | null elementAtHead = error "List index out of range!"
    | otherwise = head elementAtHead
    where elementAtHead = drop (index - 1) list
    -- This works fine for infinite lists


-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse list = (myLast list):(myReverse $ init list)


-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = myReverse list == list


-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = [] 
flatten (List (x:xs)) = (flatten x) ++ (flatten $ List xs)


-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs
                  then compress xs 
                  else x:(compress xs)


-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : (takeWhile (== x) xs)) : pack (dropWhile (== x) xs)


-- Problem 10
encode :: Eq a => [a]-> [(Int, a)]
encode = map (\l -> (length l, head l)) . pack
