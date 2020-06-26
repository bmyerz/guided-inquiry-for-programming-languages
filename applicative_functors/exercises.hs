import Control.Applicative

-- Exercise: use Maybe as an Applicative
data Tree a = Leaf a | Node a (Tree a) (Tree a) 

maybeTreeSum :: Tree (Maybe Int) -> Maybe Int
maybeTreeSum = undefined

-- test cases
mtsTest1 = maybeTreeSum (Node (Just 10)
                              (Node (Just 20)
                                    (Node Nothing
                                          (Leaf (Just 30))
                                          (Leaf (Just 40))
                                     )
                                     (Leaf (Just 7))
                              )
                              (Leaf (Just 1))
                         )
-- expects Nothing

mtsTest2 = maybeTreeSum (Node (Just 10)
                              (Node (Just 20)
                                    (Leaf (Just 30))
                                    (Leaf (Just 1))
                              )
                              (Leaf (Just 40))
                         )
-- expects (Just 101)


-- Exercise: use List as an Applicative 

-- you may use this boolean function in your rightTriangle definition
isRightTri :: Int -> Int -> Int -> Bool
isRightTri sx sy hyp = sx ^ 2 + sy ^ 2 == hyp ^ 2

rightTriangles :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
rightTriangles = undefined

-- test cases
rtTest1 = rightTriangles [3..6] [4..12] [5..13]
-- expects [(3,4,5),(5,12,13),(6,8,10)]

rtTest2 = rightTriangles [1..13] [1..13] [1..13]
-- expects [(3,4,5),(4,3,5),(5,12,13),(6,8,10),(8,6,10),(12,5,13)]


-- Exercise: use IO as an Applicative 

getLines' :: Int -> IO [String]
getLines' = undefined

glTest1 = getLines' 2
-- after entering two lines on console, returns list of those strings

glTest2 = getLines' 5
-- after enteering five lines on console, returns list of those strings


-- Exercise: use ZipList as an Applicative 

totalEachColumn :: (Int -> Int -> Int) -> [[Int]] -> [Int]
totalEachColumn fn s = getZipList (totalEachColumnHelper fn s) 

-- complete this helper function
totalEachColumnHelper :: (Int -> Int -> Int) -> [[Int]] -> ZipList Int 
totalEachColumnHelper = undefined

-- take the product of elements in each column
tecTest1 = totalEachColumn (*) [[1,2,3],
                            [10,20,30],
                            [100,200,300]]
-- expects [1000, 8000, 27000]

-- take the sum of elements in each column
tecTest2 = totalEachColumn (+) [[18, 3, 19, 2],
                            [21,16,4,10],
                            [12,8,25,24],
                            [21,9,2,22],
                            [16,2,21,7]]
-- expects [88, 38, 71, 65]
