quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let small = [i | i <- xs, i <= x]
        large = [i | i <- xs, i > x]
    in (quickSort small) ++ [x] ++ (quickSort large)

