mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    let sortedBegining = mergeSort b
        sortedEnd      = mergeSort e
    in merge b e
    where
        (b, e) = splitAt( (length xs) `div` 2 ) xs
        merge [] a = a
        merge a [] = a
        merge (a:as) (b:bs)
            | a < b     = a : (merge as (b:bs) )
            | otherwise = b : (merge bs (a:as) )
