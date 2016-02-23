-- #1 creating a rot 13 cipher
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Char as Char
import Data.Char (Char)

-- using zip and Map.!
let rot13 x = Map.fromList (zip ['a'..'z'] (drop 13 (take 39 (cycle ['a'..'z'])))) Map.! x
map rot13 "haskell"
-- > "unfxryy"

-- not using Map.! (because it throws exceptions)
let rot13lookup x = Map.lookup x (Map.fromList(zip ['a'..'z'] (drop 13 (take 39 (cycle ['a'..'z'])))))
map rot13lookup "haskell"
-- > [Just 'u',Just 'n',Just 'f',Just 'x',Just 'r',Just 'y',Just 'y']

-- using Data.char
let rot13char x = Char.chr ((Char.ord x) + 13)
map rot13char "haskell"
-- > "un\128xryy"
