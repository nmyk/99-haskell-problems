import System.Random (randomRIO)
import Control.Monad (replicateM)

-- 1. Implement `last`
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse


-- 2. Write a function that returns the penultimate item in a list
myButLast :: [a] -> a
myButLast list
    | len < 2 = error "Too few elements!"
    | otherwise = list !! (len - 2)
    where len = length list


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

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [x] = True
isPalindrome' list = head list == last list
                  && isPalindrome' (init $ tail list)


-- 7. Flatten an arbitrarily-nested list
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ (flatten $ List xs)


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

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x : fst run) : (pack $ snd run)
    where run = span (== x) xs


-- 10. Encode runs of identical elements into tuples:
--     (<number of elements in run>, <element>)
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\l -> (length l, head l)) . pack


-- 11. Encode runs of identical elements into custom data type
data RunOf a = Multiple Int a | Single a deriving Show

encodeModified :: Eq a => [a] -> [RunOf a]
encodeModified = map (\l -> if length l == 1
                            then Single $ head l
                            else Multiple (length l) (head l)) . pack


-- 12. Decode a run-length encoded list
decodeModified :: Eq a => [RunOf a] -> [a]
decodeModified [] = []
decodeModified [Single x] = [x]
decodeModified [Multiple n x] = take n $ repeat x
decodeModified (x:xs) = (decodeModified [x]) ++ (decodeModified xs)


-- 13. Directly run-length encode a list without creating sublists
encodeDirect :: Eq a => [a] -> [RunOf a]
encodeDirect [] = []
encodeDirect (x:xs) =
    let run = span (== x) xs
        pkg = \l -> if length l == 1
                    then Single $ head l
                    else Multiple (length l) (head l)
    in pkg (x : fst run) : (encodeDirect $ snd run)


-- 14. Duplicate the elements of a list; e.g. "abc" -> "aabbcc"
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)


-- 15. As above, but replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli l n = concat [take n $ repeat x | x <- l]


-- 16. Drop every n'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery list n = [fst x | x <- zip list [1..], snd x `mod` n /= 0]


-- 17. Split a list into two parts, given the length of the first part
split :: [a] -> Int -> ([a], [a])
split list n = (leftSplit enumerated, rightSplit enumerated)
    where enumerated = zip list [1..]
          leftSplit  = map fst . takeWhile ((<= n) . snd)
          rightSplit = map fst . dropWhile ((<= n) . snd)


-- 18. Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice list i j = [fst x | x <- zip list [1..], snd x >= i, snd x <= j]


-- 19. Rotate a list n places to the left
rotate :: [a] -> Int -> [a]
rotate l n
    | n' > 0 = rotate (tail l ++ [head l]) $ n' - 1
    | n' < 0 = rotate (last l : init l) $ n' + 1
    | otherwise = l
    where n' = n `mod` length l -- optimize a bit in case of large `n`


-- 20. Remove the n'th element from a list
removeAt :: Int -> [a] -> [a] -- doesn't return the deleted element
removeAt n l
    | (n < 1) || (n > length l) = error "Invalid index"
    | otherwise = map fst . filter ((/= n) . snd) $ enumerated
    where enumerated = zip l [1..]

removeAt' :: Int -> [a] -> (a, [a]) -- returns deleted element and residual list
removeAt' n l
    | (n < 1) || (n > length l) = error "Invalid index"
    | otherwise =
        let enumeratedPartition = span ((/= n) . snd) $ zip l [1..]
            left = map fst $ fst enumeratedPartition
            removed = fst . head $ snd enumeratedPartition
            right = map fst $ tail . snd $ enumeratedPartition
        in (removed, left ++ right)


-- 21. Insert an element at a given position into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt e l n
    | (n < 1) || (n > length l) = error "Invalid index"
    | otherwise = take (n - 1) l ++ e : drop (n - 1) l


-- 22. Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range i j
    | i > j = reverse $ range j i
    | i == j = [i]
    | otherwise = i:range (i+1) j


-- 23. Extract a given number of randomly selected elements from a list
rndSelect :: [a] -> Int -> IO [a] -- This solution allows duplicates
rndSelect [] _ = return []
rndSelect l n = do
    indices <- replicateM n $ randomRIO (0, (length l - 1))
    return [l!!i | i <- indices]

rndSelect' :: [a] -> Int -> IO [a] -- This solution is without replacement
rndSelect' l n
    | n > length l = error "Trying to select too many items"
    | null l = return []
    | n == 0 = return []
    | otherwise = do
        index <- randomRIO (1, length l)
        let (selected, rest) = removeAt' index l
        do
            let x = selected
            xs <- rndSelect' rest (n - 1)
            return (x:xs)


-- 24. Draw k different random numbers from the set [1..n]
diffSelect :: Int -> Int -> IO [Int]
diffSelect k n = rndSelect' [1..n] k


-- 25. Generate a random permutation of the elements of a list
rndPermu :: [a] -> IO [a]
rndPermu l =  rndSelect' l (length l)
