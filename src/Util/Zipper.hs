module Util.Zipper
  ( Zipper()
  , listToZipper
  , zipperToList
  , left
  , curs
  , right
  , next
  , prev
  , push
  , replace
  , trunc
  , first
  ) where

data Zipper a = Zipper [a] [a]
              deriving (Show)

listToZipper []    = Zipper [] []
listToZipper (c:l) = Zipper [c] l

zipperToList :: Zipper a -> [a]
zipperToList (Zipper a b) = reverse $ reverse b ++ a

left  (Zipper (c:l) _) = l
left  (Zipper []    _) = []
curs  (Zipper (c:_) _) = c
right (Zipper _     r) = r

next (Zipper l (n:r)) = Zipper (n:l) r
next z                = z

prev (Zipper (c:a:l) r) = Zipper (a:l) (c:r)
prev z                  = z

push c (Zipper l r) = Zipper (c:l) r

replace c (Zipper    [] r) = Zipper (c:[]) r
replace c (Zipper (_:l) r) = Zipper (c:l)  r

trunc (Zipper l _) = Zipper l []

first :: Zipper a -> Zipper a
first z@(Zipper (c:[]) _) = z
first z@(Zipper []     _) = z
first z = first $ prev z
