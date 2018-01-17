{- | A haskell script to practice record syntax for data types
 - Module:        Human
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 13
 -}

data Human = Human String String Int Float String String deriving (Show)

firstName :: Human -> String
firstName (Human fname _ _ _ _ _) = fname

lastName :: Human -> String
lastName (Human _ lname _ _ _ _) = lname

age :: Human -> Int
age (Human _ _ age _ _ _) = age

height :: Human -> Float
height (Human _ _ _ height _ _) = height

phoneNumber :: Human -> String
phoneNumber (Human _ _ _ _ number _) = number

flavor :: Human -> String
flavor (Human _ _ _ _ _ flavor) = flavor

data Person = Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    , height :: Float
    , phoneNumber :: String
    , flavor :: String
    } deriving (Show)

data Car = Car {company :: !String, model :: !String, year :: !Int} deriving (Show)

buildCar :: String -> String -> Int -> Car
buildCar co mo yr = Car {company=co, model=mo, year=yr}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
