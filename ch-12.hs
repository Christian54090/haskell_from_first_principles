ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n+2) else Nothing

deconMaybe :: Maybe a -> a
deconMaybe Nothing  = error "Tried to extract Nothing"
deconMaybe (Just a) = a

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 =
      Just $ Person name age
  | otherwise = Nothing

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

-- "Either" returns either the left value (PersonInvalid), or the right (Person)
--mkPerson' :: Name -> Age -> Either PersonInvalid Person

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a     = Just a

replaceThe :: String -> String
replaceThe "" = ""
replaceThe a =
  word ++ " " ++ replaceThe tail'
  where
    word' = head $ words a
    word  = (case (notThe word') of Nothing -> "a"; (Just a) -> a)
    tail' = drop ((length word')+1) a

countVowels :: String -> Integer
countVowels "" = 0
countVowels a
  | c `elem` ["a","e","i","o","u"] = 1 + countVowels tail'
  | otherwise                      = 0 + countVowels tail'
  where
    c = take 1 a
    tail' = drop 1 a
