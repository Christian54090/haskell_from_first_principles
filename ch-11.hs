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
isCar' (Plane _) = False

isPlane' :: Vehicle -> Bool
isPlane' = not . isCar'
