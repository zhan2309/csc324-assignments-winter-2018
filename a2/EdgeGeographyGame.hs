module EdgeGeographyGame where

-- You may import useful modules here.
import Debug.Trace
-- trace ("answer: " ++ show (x,u))
-- data Tree = Leaf Int | Node Int [Tree] deriving (Show)
-- Node 10 [Leaf 100, Leaf 10]

{- The input is a list of adjacency lists, e.g.,
   [(0,[1,2]), (1, [3,4]), (2,[0]), (3,[4]), (4,[])]
   means 0->1, 0->2, 1->0, 2->1, 3 has no outgoing edges.

   goodFirstVertices takes this input and computes the choices for the first
   vertex so the first player is destined to win.
-}
goodFirstVertices :: [(Int, [Int])] -> [Int]
goodFirstVertices lst = getAllGoodVertices ([0..(length lst)- 1]) (convertToTuples lst) 

getAllGoodVertices :: [Int] -> [(Int, Int)] -> [Int]
getAllGoodVertices [] lst = []
getAllGoodVertices (x:xs) lst 
    | (isGoodVertice 0 x lst) = [x] ++ (getAllGoodVertices xs lst)
    | otherwise = getAllGoodVertices xs lst

convertToTuple :: (Int, [Int]) -> [(Int, Int)]
convertToTuple (x, []) = []
convertToTuple (x,(u:us)) = (x,u) : convertToTuple (x, us)

convertToTuples :: [(Int, [Int])] -> [(Int, Int)]
convertToTuples [] = []
convertToTuples (x:xs) = convertToTuple x ++ convertToTuples xs

delete :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
delete [] a = []
delete (x:xs) a 
    | x == a = xs
    | otherwise =  [x] ++ (delete xs a) 

findAll :: Int -> [(Int, Int)] -> [(Int, Int)]
findAll a [] = []
findAll a (x:xs) = (findOne a x) ++ (findAll a xs)

findOne :: Int -> (Int, Int) -> [(Int, Int)]
findOne a x 
    | (fst x) == a = [x]
    | otherwise = []

isGoodVertice :: Int -> Int -> [(Int, Int)] -> Bool
isGoodVertice 0 a [] = True
isGoodVertice 1 a [] = False
isGoodVertice 0 a xs = (showNextMoveResult 0 (findAll a xs) xs)
isGoodVertice 1 a xs = (showNextMoveResult 1 (findAll a xs) xs)

showNextMoveResult :: Int -> [(Int, Int)] -> [(Int, Int)] -> Bool
showNextMoveResult 0 [] lst = True
showNextMoveResult 1 [] lst = False
showNextMoveResult 0 (x_a@(u,v):x_as) lst = if (isGoodVertice 1 v (delete lst x_a)) == False
                                                then False
                                                else (isGoodVertice 1 v (delete lst x_a)) && (showNextMoveResult 0 x_as lst)
showNextMoveResult 1 (x_a@(u,v):x_as) lst = if (isGoodVertice 0 v (delete lst x_a)) == True
                                                then True
                                                else (isGoodVertice 0 v (delete lst x_a)) || (showNextMoveResult 1 x_as lst)


    