data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir
testCar  = Car Mini

isCar :: Vehicle -> Bool
isCar = (\v -> case v of Plane _ -> False; Car _ _ -> True)

isPlane :: Vehicle -> Bool
isPlane = (\v -> case v of Plane _ -> True; Car _ _ -> False)

areCars :: [Vehicle] -> [Bool]
areCars a = map (\v -> case v of Plane _ -> False; Car _ _ -> True) a

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

isCar' :: Vehicle -> Bool
isCar' (Car _ _) = True
isCar' _         = False

isPlane' :: Vehicle -> Bool
isPlane' = not . isCar'

areCars' :: [Vehicle] -> [Bool]
areCars' = map isCar'

data Person =
  Person { name :: String
         , age :: Int}
         deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16

data List a = Nil | Cons a (List a)

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' v Leaf = Node Leaf v Leaf
insert' v (Node left v' right)
  | v < v'  = Node (insert' v left) v' right
  | v > v'  = Node left v' (insert' v right)
  | v == v' = Node left v right
-- recursively inserts until it finds an empty slot,
-- at which it inserts the base case (Node Leaf v Leaf) into the tree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
  1
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
  2
       (Node Leaf 5 Leaf)

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
       (Node Leaf 3 Leaf)

bstSearch :: (Eq a, Ord a) => a -> BinaryTree a -> Bool
bstSearch v Leaf = False
bstSearch v (Node left a right)
  | v < a  = bstSearch v left
  | v > a  = bstSearch v right
  | v == a = True