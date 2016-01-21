module Data.List.Zipper where

data Z a = Z [a] a [a]

zipper :: [a] -> Maybe (Z a)
zipper [] = Nothing
zipper (x:xs) = Just (Z [] x xs)

zpeek :: Z a -> a
zpeek (Z _ x _) = x
