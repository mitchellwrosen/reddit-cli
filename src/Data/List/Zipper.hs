module Data.List.Zipper where

data Z a = Z [a] a [a]
  deriving Show

zipper :: [a] -> Maybe (Z a)
zipper [] = Nothing
zipper (x:xs) = Just (Z [] x xs)

zllen :: Z a -> Int
zllen (Z as _ _) = length as

zrlen :: Z a -> Int
zrlen (Z _ _ cs) = length cs

zpeek :: Z a -> a
zpeek (Z _ x _) = x

zdown :: Z a -> Z a
zdown (Z as b (c:cs)) = Z (b:as) c cs
zdown z = z

zup :: Z a -> Z a
zup (Z (a:as) b cs) = Z as a (b:cs)
zup z = z
