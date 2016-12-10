foldrNew :: (a -> b -> b) -> b -> [a] -> b
foldrNew f z []     = z 
foldrNew f z (h:t) = f h (foldrNew f z t) 
 
foldlNew :: (b -> a -> b) -> b -> [a] -> b
foldlNew f z []     = z                  
foldlNew f z (h:t) = foldlNew f (f z h) t

mapNew :: (a -> b) -> [a] -> [b]
mapNew _ [] = []
mapNew f list   = foldrNew (\h t -> (f h):t) [] list

flatMapNew :: (a -> [b]) -> [a] -> [b]
flatMapNew _ [] = []
flatMapNew f list = foldrNew (\h t -> (f h) ++ t) [] list 

concatNew :: [a] -> [a] -> [a]
concatNew [] [] = []
concatNew list1 list2 = foldrNew (\l1 l2 -> l1:l2) list2 list1

filterNew :: (a -> Bool) -> [a] -> [a]
filterNew _ [] = []
filterNew f list = foldrNew (\h t -> if (f h) then h:t else t) [] list

maxByNew :: (a -> Integer) -> [a] -> a
maxByNew f (h:t) =
				foldlNew (\max h -> if (f max) > (f h) then max else h) h t

minByNew :: (a -> Integer) -> [a] -> a
minByNew f (h:t) = 
				foldlNew (\min h -> if (f min) < (f h) then min else h) h t

reverseNew :: [a] -> [a]
reverseNew [] = []
reverseNew list = foldrNew (\h t -> t ++ [h]) [] list

elementAtNew :: Integer -> [a] -> a
elementAtNew _ [] = error "list is empty"
elementAtNew x list |	((length list) <= (fromIntegral x)) = error "no element at list"
					|	otherwise = res
								where (res, _) =
										foldlNew (\(res,x) h -> if (x == 0) then (h, x-1) else (res, x-1)) (head list, x) list

indexOfNew :: String -> [String] -> Integer
indexOfNew _ [] = error "list is empty"
indexOfNew x list = if (res == -1) then error "element not found" else res
					where (res, _, _) =
						foldlNew (\(res, count, x) h -> if (h == x) then (count, count+1, x) else (res, count+1, x)) (-1, 0, x) list

-- main
test :: String -> String
test str =
	--let x = mapNew (\x -> if (x > 1) then True else False) [1,2] in
	--let x = flatMapNew (\x -> [x, x]) [1, 2] in
	--let x = concatNew [1, 2] [3, 4] in	
	--let x = filterNew (\x -> if (x > 1) then True else False) [1,2] in
	--let x = maxByNew (\x -> -x) [1,2] in	
	--let x = minByNew (\x -> -x) [1,2] in
	--let x = reverseNew [1,2,3] in
	--let x = elementAtNew 3 [1,2,3] in
	let x = indexOfNew "we" ["qwe", "asd", "zxc"] in
	show $ x
main = interact test