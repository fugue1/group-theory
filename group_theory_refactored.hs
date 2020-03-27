import Data.Array
import System.Environment


deleteWithOne [] = []
deleteWithOne [x] = [(x, [])]
deleteWithOne (x:xs) = (x, xs) : map (\(z, ys) -> (z, x:ys)) (deleteWithOne xs)

filter_collapse (xs:yss) = [ (x, ns):as | (x, ns) <- xs, as <- filter_collapse [genRemove (\(a, _) -> a == x) ys | ys <- yss]]
filter_collapse [] = [[]]

filter_collapse_id (row,col) ids [] = [([],[])]
filter_collapse_id (row,col) ids (xs:yss) | row <= col = [ (ids, as) | as <- filter_collapse (xs:yss)]
                                          | bool = [ (newIds, head xs :as) | as <- filtered]
                                          | otherwise = [ (is, (x, ns):as) | (x, ns) <- removeOne, (is, as) <- filter_collapse_id (row,1+col) ids (map (genRemove (\(a, _) -> a == x)) yss)]
    where (bool, newIds) = elemRem (row, col) ids
          filtered = filter_collapse [genRemove (\(a, _) -> a == 1) ys | ys <- yss]
          removeOne = genRemove (\(a,_) -> a == 1) xs


splitter [] = ([],[])
splitter ((z,ys):wss) = (z:zs, ys:yss)
    where (zs, yss) = splitter wss

idFinder c ((z,ys):wss) | z == 1 = (c, (z:zss, ys:xss ))
                        | otherwise = (t, (z:hss, ys:kss))
    where (zss, xss) = splitter wss
          (t, (hss, kss)) = idFinder (c+1) wss

idSplit = idFinder 1

genRemove f [] = []
genRemove f (x:xs) | f x = xs 
                   | otherwise = x : genRemove f xs

elemRem x [] = (False, [])
elemRem x (y:ys) | x /= y = (bool, y:zs)
                 | otherwise = (True, ys)
    where (bool, zs) = elemRem x ys
 

filterCollapseNorm row ids zss = [ (is, (row, snd $ head hs):as) | (is, as) <- filter_collapse_id (row,2) ids (map (genRemove (\(a,_) -> a==row)) (hs:gss)) ]                      
    where (hs:gss) = tail $ map deleteWithOne zss

deleteOne [x] = [[]]
deleteOne (x:xs) = xs : map (x:) (deleteOne xs)

haskLatin n = map (intArray:) $ concatMap (treeBuild 2) $ filterCollapseNorm 2 [] (deleteOne intvl)
    where
        intvl = [1..n]
        castArray = listArray (1,n)
        intArray = castArray intvl
        
        treeBuild row (ids, ((a,[]):zss)) | 0 == remaind = [[ arr ]]
                                     | otherwise = []
            where ordz = orderPermMax 2 arr arr
                  remaind = rem n ordz
                  arr = castArray $ a: map fst zss 

        treeBuild row (ids, xss) | 0 == remaind = map (arr:) $ (concatMap (treeBuild (1+row)) filtercollapsed)
                            | otherwise = []
            where (col,(firsts, seconds)) = idSplit xss
                  arr = castArray firsts
                  ordz = orderPermMax 2 arr arr
                  remaind = rem n ordz
                  newIds | row >= col = ids
                         | otherwise = (col,row):ids
                  filtercollapsed = filterCollapseNorm (1+row) newIds seconds

        orderPermMax m rz hs | (m > n) || (intArray == next) = m
                             | otherwise = orderPermMax (m+1) rz next
            where next = comp_perm rz hs

        comp_perm as bs = array (1,n) [(i, as ! (bs ! i)) | i <- intvl]

haskGroup 1 = [[[1]]]

haskGroup n = map (map elems) $ filter (weakArray . castArray) $ haskLatin n
    where nSqr = [(i,j) | i <- [2..n], j <- [2..n]]
          castArray = listArray (1,n)
          weakArray arr = all (\(i,j) -> let comp = comped i j in comp == arr ! (comp ! 1)) nSqr
              where comped i j = comp_perm (arr ! i) (arr ! j)
                    
          comp_perm as bs = array (1,n) [(i, as ! (bs ! i)) | i <- intvl]
          intvl = [1..n]
--------------------------
main = do 
    [n,m] <- getArgs
    let x = read n :: Int 
        y = read m :: Int 
    print $ take x $ haskGroup y
