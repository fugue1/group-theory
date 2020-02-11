
import Data.List (sortOn)
import Data.Array
import System.Environment


----------------------------remove a from list
remove a [] = []
remove a (x:xs) | a == x = xs
                | otherwise = x : remove a xs
--------------------------------------------------------fast reverse
fastRev [] ys = ys
fastRev (x:xs) ys = fastRev xs (x:ys)
revast xs = fastRev xs []
----------------------------------------------------------------
biMerge f [] = []
biMerge f [x] = [x]

biMerge f (x:y:xs) = (f x y) : biMerge f xs

--------------------------------------general button up merge with f
mergeUp f [] = []

mergeUp f [x] = x

mergeUp f xs = mergeUp f merged
    where merged = biMerge f xs

----------------------merge returning unique elements
u_merge [] ys = ys
u_merge xs [] = xs
u_merge (x:xs) (y:ys) | x < y = x : u_merge xs (y:ys)
                      | y < x = y : u_merge (x:xs) ys
                      | otherwise = x : u_merge xs ys 

umerge xs = mergeUp u_merge [[x] | x <- xs]

-------------------------------difference lists
diffList xs = (xs++)
appDiff xs ys = xs . ys
sameList xs = xs []
diffMap f xs = diffList (map f (sameList xs))   ---project difference list xs to a list, map over with f, liftM back up

---------------------------------------------------------- intrinsic permutation composition through sorting

compTwist xs ys = [a | (a,b) <- Data.List.sortOn snd $ zip xs ys]

permInvert xs = compTwist [1..] xs

compPerm xs ys = compTwist xs (permInvert ys) 


----------------------------------------------------------------------permutation composition with indexed arrays (faster)
comp_Perm n xs ys = array (1,n) [(i, xs ! (ys ! i)) | i <- [1..n]]
    

toArray n xs = array (1,n) (zip [1..] xs)
--------------------------------------------------------------------search for latin squares that are groups whose order signature is increasing

truncPot n = [ remove k [1..n] | k <- [2..n]]

branchGroup n = fixRow 2 2 [] [1] [2] [] [] (truncPot n)
    where

        fixRow i j es ks zs ys xs ((1:cs):rs) | (j > i) && (not (elem 1 zs)) = appDiff (fixRow i (j+1) ((i,j):es) ks (1:zs) ((xs ++ cs):ys) [] rs) (fixRow i j es ks zs ys (1:xs) (cs:rs))
                                              | (i > j) = if (elem (j,i) es) then fixRow i (j+1) es ks (1:zs) ((xs ++ cs):ys) [] rs else fixRow i j es ks zs ys (1:xs) (cs:rs)
                                              | i == j = appDiff (fixRow i (j+1) es ks (1:zs) ((xs ++ cs):ys) [] rs) (fixRow i j es ks zs ys (1:xs) (cs:rs))


        fixRow i j es ks zs ys xs ((c:cs):rs) | not (elem c zs) = appDiff (fixRow i (j+1) es ks (c:zs) ((xs ++ cs):ys) [] rs) (fixRow i j es ks zs ys (c:xs) (cs:rs))
                                              | otherwise = fixRow i j es ks zs ys (c:xs) (cs:rs)  

        fixRow i j es ks zs ([]:ys) xs [] | (head ks <= ordz) && (0 == remaind) = diffList [(revast (ordz:ks), [revz])]
                                          | otherwise = diffList []
            where revz = toArray n (revast zs)
                  ordz = orderPermMax 2 revz revz
                  remaind = rem n ordz

        fixRow i j es ks zs ys xs [] | (head ks <= ordz) && (0 == remaind) = diffMap (muteSec (revz:)) (fixRow (newr) 2 es (ordz:ks) [newr] [] [] (revast ys))
                                     | otherwise = diffList []
            where revz = toArray n (revast zs)
                  ordz = orderPermMax 2 revz revz
                  remaind = rem n ordz
                  newr = i+1
               


        fixRow i j es ks zs ys xs ([]:rs) = diffList []

        orderPermMax m rz hs | (m > n) || (intvl == next) = m
                             | otherwise = orderPermMax (m+1) rz next

            where next = comp_Perm n rz hs

        intvl = toArray n [1..n]


fixedSquares n = sameList $ diffMap (muteSec (toArray n . (toArray n [1..n]:))) $ branchGroup n

fastGroup n = map (muteSec ((map elems) . elems)) $ filter ((closedArray n nSqr) . snd) $ fixedSquares n
    where nSqr = [(i,j) | i <-[2..n], j <- [2..n]]


-----------here we check that nSqr is a left regular representation of a group
-----------we probably don't need to check all pairs (i,j)
closedArray n nSqr arr = all (\(i,j) -> ((comp_Perm n (arr ! i) (arr ! j)) == arr ! (arr ! i ! j))) nSqr


--------------------------------apply f to second element only
muteSec f (os, ps) = (os, f ps)



main = do 
    [n,m] <- getArgs
    let x = read n :: Int 
        y = read m :: Int 
    print (umerge $ map fst $ take x $ fastGroup y)