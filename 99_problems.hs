-- 1. Implement `last`
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse


-- 2. Write a function that returns the penultimate item in a list
myButLast :: [a] -> a
myButLast xs
    | len < 2 = error "Too few elements!"
    | otherwise = xs !! (len - 2)
    where len = length xs


-- 3. Implement `!!`
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


-- 4. Implement `length`
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


-- 5. Implement `reverse`
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse list = (myLast list):(myReverse $ init list)


-- 6. Write a function to determine whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = myReverse list == list


-- 7. Flatten an arbitrarily-nested list
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = [] 
flatten (List (x:xs)) = (flatten x) ++ (flatten $ List xs)


-- 8. Collapse adjacent identical list elements to a single element
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs
                  then compress xs 
                  else x:(compress xs)


-- 9. Separate runs of identical elements into sublists
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : (takeWhile (== x) xs)) : pack (dropWhile (== x) xs)


-- 10. Encode runs of identical elements into tuples: 
--     (<number of elements in run>, <element>)
encode :: Eq a => [a]-> [(Int, a)]
encode = map (\l -> (length l, head l)) . pack
