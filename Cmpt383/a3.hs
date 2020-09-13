-- A3
-- Jia Ming Wu
-- 301278354

-- Q1
snoc :: a -> [a] -> [a]
snoc x []        = [x] 
snoc x (y:ys)    = y : (snoc x (ys))


-- Q2
myappend :: [a] -> [a] -> [a]
myappend x []     = x 
myappend x (y:ys) = myappend (snoc y x) ys


-- Q3
myreverse :: [a] -> [a]
myreverse []       = []
myreverse (x:xs)   = snoc x (myreverse xs)


-- Q4
-- smallest_divisor and is_prime is from the 383 course websites
smallest_divisor :: Integer -> Integer
smallest_divisor n
    | n < 0     = error "n must be >= 0"
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = head (dropWhile (\x -> n `mod` x /= 0) [2..n])

is_prime :: Integer -> Bool
is_prime n | n < 2     = False
           | otherwise = (smallest_divisor n) == n

-- reverse int idea is from https://stackoverflow.com/questions/19725292/how-to-reverse-an-integer-in-haskell
--
reverseInt :: Integer -> Integer
reverseInt x 
    |x == 0    = 0
    |x > 0     = read . myreverse . show $ x 
    |otherwise = error "Please enter a positive int"



count_emirps :: Integer -> Integer
count_emirps n
    | n < 13                                                        = 0
    | is_prime n && is_prime (reverseInt n) && n /= (reverseInt n)  = 1 + count_emirps (n - 1)
    | otherwise                                                     = count_emirps (n - 1)


 
--Q5

biggest_sum :: [[Int]] -> [Int]
biggest_sum [(x:xs)] = (x:xs)
biggest_sum (x:xs)
    | xs == []                     = x
    | sum x > sum (biggest_sum xs) = x
    | otherwise                    = biggest_sum xs



--Q6
greatest :: (a -> Int) -> [a] -> a
greatest f (x:xs)
    | null xs = x
    | (f x) >= (f (greatest f xs)) = x
    | otherwise = greatest f xs



--Q7
is_bit :: Int -> Bool
is_bit x 
    | x == 0         = True
    | x == 1         = True
    | otherwise      = False


--Q8
flip_bit :: Int -> Int
flip_bit x
    | x == 0         = 1
    | x == 1         = 0
    | otherwise      = error "Input is not a bit"


--Q9.a
is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 seq
    | null seq            = True
    | is_bit (head seq)   = is_bit_seq1 (tail seq)
    | otherwise           = False

--Q9.b
is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 []    = True
is_bit_seq2 seq   = if is_bit(head seq) then is_bit_seq2(tail seq) else False

--Q9.c
is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 []          = True
is_bit_seq3 seq         = all is_bit seq


--Q10.a
invert_bits1 :: [Int] -> [Int]
invert_bits1 []     = []
invert_bits1 (x:xs)
    | is_bit x      = flip_bit x : invert_bits1 xs
    | otherwise     = error "Invalid bit sequence"

--Q10.b
invert_bits2 :: [Int] -> [Int]
invert_bits2 lst = map flip_bit lst

--Q10.c
invert_bits3 :: [Int] -> [Int]
invert_bits3 lst = [flip_bit x | x <- lst]


--Q11
bit_count :: [Int] -> (Int, Int)
bit_count seq 
 | is_bit_seq1 seq = ((length (filter (==0) seq)),(length (filter (==1) seq)))
 | otherwise = error "Input is not a bit sequence"
 


--Q12
all_basic_bit_seqs :: Int -> [[Int]]
-- idea from http://zvon.org/other/haskell/Outputprelude/replicate_f.html
-- replicate Input: replicate 3 5 gives Output: [5,5,5]
all_basic_bit_seqs n = if n<=0 then [] else sequence (replicate n [1,0])



--Q13
data List a = Empty | Cons a (List a)
    deriving Show

toList :: [a] -> List a
toList lst
    | null lst    = Empty
    | otherwise   = Cons (head lst) (toList (tail lst))


--Q14
toHaskellList :: List a -> [a]
toHaskellList Empty        = []
toHaskellList (Cons x xs)  = x : (toHaskellList xs)


--Q15
append :: List a -> List a -> List a
append Empty x        = x
append (Cons y ys) x  = (Cons y (append ys x))


--Q16 
removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty = Empty
removeAll f (Cons x xs) 
    | f x         = removeAll f xs
    | otherwise   = Cons x (removeAll f xs)

--Q17                    
sort :: Ord a => List a -> List a
sort Empty         = Empty
sort (Cons x xs)   = append (sort(removeAll (\n -> n > x) xs)) (Cons x (sort(removeAll (\n -> n <= x) xs)))
