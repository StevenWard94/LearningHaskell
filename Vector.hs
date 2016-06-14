{- | A haskell script to practice parameterized data types
 - Module:        Vector
 - Author:        Steven Ward <stevenward94@gmail.com>
 - URL:           https://github.com/StevenWard94/LearningHaskell
 - Last Change:   2016 June 12
 -}

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

scalarMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `scalarMult` m = Vector (i * m) (j * m) (k * m)

vectMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `vectMult` (Vector l m n) = (i * l) + (j * m) + (k * n)
