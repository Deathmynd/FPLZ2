data BinaryTree = EmptyTree | Node Integer BinaryTree BinaryTree -- deriving (Eq)
singleton :: Integer -> BinaryTree    
singleton x = Node x EmptyTree EmptyTree    
    
insert :: BinaryTree -> Integer -> BinaryTree    
insert EmptyTree x = singleton x    
insert (Node a left right) x     
    | x == a = Node x left right    
    | x < a  = Node a (insert left x) right    
    | x > a  = Node a left (insert right x)  

minValue :: BinaryTree -> Integer
minValue EmptyTree = error "Tree is empty"
minValue (Node a EmptyTree _) = a
minValue (Node a left _) = minValue left

remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree a = emptyTree
remove (Node a EmptyTree EmptyTree) x | (a == x) = EmptyTree
    | otherwise = (Node a EmptyTree EmptyTree)
remove (Node a left EmptyTree) x | (a == x) = left
    | (a > x) = Node a (remove left x) EmptyTree
    | (a < x) = Node a left EmptyTree

remove (Node a EmptyTree right) x | (a == x) = right
    | (a > x) = Node a EmptyTree right
    | (a < x) = Node a EmptyTree (remove right x)
remove (Node a left right) x | (x < a) = Node a (remove left x) right
    | (x > a) = Node a left (remove right x)
    | (x == a) = let rightMinimum = minValue right in
        Node rightMinimum left (remove right rightMinimum)

emptyTree :: BinaryTree
emptyTree = EmptyTree

containsElement :: BinaryTree -> Integer -> Bool  
containsElement EmptyTree x = False  
containsElement (Node a left right) x   
    | x == a = True  --error "dsadsa"
    | x < a  = containsElement left x  
    | x > a  = containsElement right x 

maxValue :: BinaryTree -> Integer
maxValue EmptyTree = error "Tree is empty"
maxValue (Node a _ EmptyTree) = a
maxValue (Node a _ right) = maxValue right

nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree x = error "Tree is empty"
nearestGE tree x = 
    if (maxValue tree < x) 
    then error "value is more than the maximum value in a tree"
    else nearestGEmain tree x

nearestGEmain :: BinaryTree -> Integer -> Integer
nearestGEmain EmptyTree x = x-1
nearestGEmain (Node a left right) x | (a == x) = a
    | (x < a) = let newX = nearestGEmain left x in
        if (newX < x) then a
            else newX
    | (x > a) = nearestGEmain right x

convertTreeToList :: BinaryTree -> [Integer] -> [Integer]
convertTreeToList EmptyTree list = list
convertTreeToList (Node a left right) list = let listWithA = list ++ [a] in
    let listWithAWithLeft = convertTreeToList left listWithA in
    convertTreeToList right listWithAWithLeft

convertListToTree :: [Integer] -> BinaryTree -> BinaryTree
convertListToTree [] tree = tree
convertListToTree (h:t) tree = let newTree = insert tree h in
    convertListToTree t newTree   

listFromTree :: BinaryTree -> [Integer]
listFromTree tree = convertTreeToList tree []

treeFromList :: [Integer] -> BinaryTree
treeFromList list = convertListToTree list EmptyTree 

checkTree :: String -> String
checkTree str = 

    let x = emptyTree `insert` 8 `insert` 4 `insert` 2 `insert` 6 `insert` 12 `insert` 10 `insert` 14 in
    --let x = [2, 4, 3, 5, 7] in
    --let tree = treeFromList x in

    show $ nearestGE x 9

main = interact checkTree