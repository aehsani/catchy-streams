-- Boyer Moore majority Vote Algorithm
-- Given a list or stream of elements where the majority are the same,
-- this algorithm will detect that majority element.

majority :: (Eq a) => [a] -> Maybe a
majority [] = Nothing
majority xs = voting xs 0 Nothing

voting :: (Eq a) => [a] -> Int -> Maybe a -> Maybe a
voting []   n v = v
voting (x:xs) 0 v = voting xs 1 (Just x)
voting (x:xs) n v
    | v == Just x = voting xs (n+1) v
    | otherwise   = voting xs (n-1) v
