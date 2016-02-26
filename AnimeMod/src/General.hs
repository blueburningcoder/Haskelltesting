module General where


-- | adds an element to the end of the given list
addToEnd :: [a] -> a -> [a]
addToEnd (l:li) a = l:(addToEnd li a)
addToEnd []     a = [a]


-- | basically filters the first n elements of the list after the predicate
getClosest :: [a] -> Int -> (a -> Bool) -> [a]
getClosest []     _ _ = []
getClosest  _     0 _ = []
getClosest (l:li) n p = if p l then l:(getClosest li (n - 1) p) else getClosest li n p



