-- multi clause function
sumTill :: Int -> Int
sumTill 1 = 1
sumTill n = n + sumTill (n - 1)

-- guard patterns / guards
sumTill' :: Int -> Int
sumTill' n
 | n == 1 = 1
 | otherwise = n + sumTill' (n - 1)

-- case
sumTill'' n = case n of
  1 -> 1
  _ -> n + sumTill'' (n - 1)

-- you can mix them
func :: Int -> Int -> Int
func x 1
 | x == 1 = 0
func x y = 999

rot13char :: Char -> Char
rot13char c
 | isLower = undefined
 | isUpper = undefined
 | otherwise = error "ERROR"
 where
   isUpper = 'A' <= c && c <= 'Z'
   isLower = 'a' <= c && c <= 'z'

-- top-bottom style
addOneSquare x = y * y
  where y = x+1

-- bottom-top style
addOneSquare' x =  let y = x + 1
                    in y * y
