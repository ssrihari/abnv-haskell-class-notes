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

data User = NormalUser {  userName :: String
                        , userAge :: Int}
          | Admin {  userName :: String
	           , userAge :: Int
		   , adminRole :: String}
	  deriving (Show)

-- showUser :: User -> String
-- showUser :: (NormalUser name age) =
--   ("name is: " ++ name ++ " and age is:" ++ show age)
-- showUser :: (Admin name age role) =
--   ("name is: " ++ name ++ " and age is:" ++ (show age) ++ " role is: " ++ role)



-- data IntList = EmptyList | List Int IntList
-- let myl = List 1 (List 2 EmptyList)
-- iLength IntList l = case l of {EmptyList -> 0; List i rest -> 1 + iLength rest}

data BST = EmptyNode | Node {left::BST, value::Int, right::BST} deriving (Show)

find :: BST -> Int -> Bool
find EmptyNode _ = False
find (Node l v r) x
 | x == v = True
 | x < v = find l x
 | x > v = find r x

insert :: BST -> Int -> BST
insert EmptyNode x = Node EmptyNode x EmptyNode
insert n@(Node l v r) x
 | x == v = n
 | x < v = Node (insert l x) v r
 | x > v = Node l v (insert r x)

fromList :: [Int] -> BST
fromList [] = EmptyNode
fromList (f:r) = insert (fromList r) f

isBST :: BST -> Bool
isBst EmptyNode = True
isBST (Node l v r) = value l < v && value r > v

-- test #1
-- quickCheck (\list val -> let tree = fromList list in find (insert tree val) val)
-- quickCheck (\list -> Data.List.sort list == Data.List.nub (inOrder (fromList list)))
-- quickCheck (\list -> isBST (fromList list))