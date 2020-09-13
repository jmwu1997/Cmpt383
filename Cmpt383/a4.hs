--a4
--jiaming wu
--301278354

data Token = Num Double | Op String
    deriving (Eq, Read, Show)
    
--check input is a operations
isOp :: [Char] -> Bool 
isOp fn 
-- binary operators
    | fn  == "+" = True
    | fn  == "-" = True
    | fn  == "*" = True
    | fn  == "/" = True
    | fn  == "+all" = True
    | fn  == "*all" = True
--unary operators
    | fn  == "inc" = True
    | fn  == "dec" = True
    | fn  == "sqrt" = True
    | fn  == "sin" = True
    | fn  == "cos" = True
    | fn  == "inv" = True
-- manipulate operators
    | fn  == "dup" = True
    | fn  == "pop" = True
    | fn  == "clear" = True
    | fn == "swap" = True
    | otherwise = False

--code isDouble references from https://rosettacode.org/wiki/Determine_if_a_string_is_numeric
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

--convert from string to token
convert_token :: String -> Token
convert_token x
    | isOp x   = Op x
    | isDouble x = Num (read x::Double)
    | otherwise = error "Invalid Token"

--check if input is a token
f_token :: String -> Bool
f_token x
    | isOp x   = True
    | isDouble x = True
    | otherwise = False

--showtoken
showToken :: Token -> String
showToken (Num x) = show x
showToken (Op x) = x

--check stack is good input
checkStack :: [Char] -> Bool
checkStack str = all f_token (words str)

--checkstack if is a valid input Num or Op
calcStack :: String -> String
calcStack str 
    |checkStack str = str
    |otherwise = error "Input is invalid"

--check add
checkadd :: String -> Bool
checkadd str 
    | words str !! 1 == "+" = True
    | otherwise = False

--check multiply
checkmul :: String -> Bool
checkmul str 
    | words str !! 1 == "*" = True
    | otherwise = False

--check divide
checkdiv :: String -> Bool
checkdiv str 
    | words str !! 1 == "/" = True
    | otherwise = False

--check subtractions
checksub :: String -> Bool
checksub str 
    | words str !! 1 == "-" = True
    | otherwise = False

--check string empty
checkNull :: String -> Bool
checkNull str
    | cal str == [] = True 
    | otherwise = False

--check string only have 1 word and its not a token
checklen :: String -> Bool
checklen str 
    | (length (words str) == 1)&& not(f_token str) = True
    | otherwise = False

--check string only have 1 word and its a number
checklen1 :: String -> Bool
checklen1 str 
    | (length (words str) == 1)&& isDouble str = True
    | otherwise = False

--check if string input is a op// similar to isop but string check
checkop :: String -> Bool
checkop fn 
    | fn  == "inc" = True
    | fn  == "dec" = True
    | fn  == "sqrt" = True
    | fn  == "sin" = True
    | fn  == "cos" = True
    | fn  == "inv" = True
    | fn  == "dup" = True
    | fn  == "pop" = True
    | fn  == "clear" = True
    | fn == "swap" = True
    | fn  == "+all" = True
    | fn  == "*all" = True
    | otherwise = False


--applying operations on the numbers
applyop :: [Double] -> String -> [Double]  
-- "4 5 + 1 2" => "4 + 5 1 2"
applyop (x:y:xs) fn    
 | fn == "+" = (x + y):xs
 | fn == "-" = (x - y):xs
 | fn == "*" = (x * y):xs
 | fn == "/" = (y / x):xs
 | fn == "swap" = y:x:xs
 | fn == "clear" = []

applyop (x:xs) fn 
 | fn == "sqrt" = (sqrt x):xs 
 | fn == "dup" =  x:x:xs
 | fn == "sin" = (sin x):xs
 | fn == "cos" = (cos x):xs
 | fn == "inc" = (x+1):xs
 | fn == "dec" = (x-1):xs
 | fn == "inv" = (1 / x):xs
 | fn == "pop" = xs
 | fn == "+all" = [foldl (+) (0) (x:xs)]
 | fn == "*all" = [foldl (*) (1) (x:xs)]

applyop x y
 | checkStack y = read y:x
 | otherwise = error "Invalid operations"

-- using foldl to apply operations 1 by 1
cal :: String -> [Double] 
cal str 
 |checkStack str = foldl applyop [] (words str)
 |otherwise = error "Invalid Input"


-- check all invalid input before outputting
calc :: String -> String
calc str
    | checklen str = "Invalid input"
    | checklen1 str = "Not enough args"
    | checkop str = str ++ ": empty stack"
    | checkadd str = "+: not enough args"
    | checksub str = "-: not enough args"
    | checkmul str = "*: not enough args"
    | checkdiv str = "/: not enough args"
    | checkNull str = "empty stack"
    | checkStack str = show . head $ cal str




