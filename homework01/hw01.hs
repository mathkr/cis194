-- Homework 01

-- Returns a list containing each digit of a positive Integer in the correct
-- order.
toDigits :: Integer -> [Integer]
toDigits n
        | n <= 0    = []
        | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]

-- Returns a list containing each digit of a positive Integer in the reversed
-- order.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
        | n <= 0    = []
        | otherwise = (n `mod` 10) : toDigits (n `div` 10)

-- Doubles every second number in a list of Integers, beginning from the right.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOther' $ reverse xs
        where doubleEveryOther' []        = []
              doubleEveryOther' (x:[])    = [x]
              doubleEveryOther' (x:y:ys)  = x:(y * 2):(doubleEveryOther' ys)

-- Sums up all individual digits of all Integers in a list.
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (sum $ toDigits x) + sumDigits xs

-- Checks whether an Integer is a valid credit card number
validate :: Integer -> Bool
validate 0 = False
validate n = divisibleBy10 . sumDigits . doubleEveryOther $ toDigits n
        where divisibleBy10 n = n `mod` 10 == 0

--
-- Towers of Hanoi
--

type Peg = String
type Move = (Peg, Peg)

-- Gives a list of moves to solve the towers of hanoi with n stones and three
-- pegs.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to _ = [(from, to)]
hanoi n a b c     = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a
