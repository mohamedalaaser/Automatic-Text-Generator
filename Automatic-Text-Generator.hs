import DataFile

---------------------------wordToken------------------------------------------------

wordToken :: [Char] -> [[Char]]
wordToken (a:at)= wordTokenHelp (spacePunc(a:at))

spacePunc :: [Char] -> [Char]
spacePunc [] = []
spacePunc (a:at) 
	| elem a punct = (" " ++[a]) ++ spacePunc at
	| otherwise =[a]++spacePunc at

wordTokenHelp :: [Char] -> [[Char]]
wordTokenHelp [] = []
wordTokenHelp (a:at) 
	| a==' ' = wordTokenHelp at
	| otherwise = x:wordTokenHelp(skip (length x) at)      where x = wordTokenString (a:at)

wordTokenString :: [Char] -> [Char]
wordTokenString [] = []
wordTokenString (a:at)
	| a==' ' = []
	| otherwise = a:wordTokenString at

skip :: Int-> [Char] -> [Char]
skip _ [] = []
skip a (b:bt) 
	| a==0 = (b:bt)
	|otherwise = skip (a-1) bt

wordTokenList :: [String] -> [String]
wordTokenList [] = []
wordTokenList (a:at) = (wordToken a) ++ (wordTokenList at)

------------------------------------------------------------------------------------
--------------------------------uniqueBigrams---------------------------------------
removeDuplicates [] = []
removeDuplicates (x:xs) 
	| elem x xs = removeDuplicates xs
	| otherwise = x : removeDuplicates xs


uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams [] = []
uniqueBigrams [x] = [(x,"")]
uniqueBigrams x = removeDuplicates (uniqueBigramsH x)
uniqueBigramsH (a:b:xs)= (a,b) : uniqueBigramsH (b:xs)
uniqueBigramsH [x] = []



------------------------------------------------------------------------------------
--------------------------------uniqueTrigrams--------------------------------------
removeDuplicates2 [] = []
removeDuplicates2 (x:xs)
	| elem x xs = removeDuplicates2 xs
	| otherwise = x : removeDuplicates2 xs


uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams [] = []
uniqueTrigrams [x] = [(x," ","")]
uniqueTrigrams [x,y] = [(x,y,"")]
uniqueTrigrams x = removeDuplicates2 (uniqueTrigramsH x)
uniqueTrigramsH (a:b:c:xs)= (a,b,c) : uniqueTrigramsH (b:c:xs)
uniqueTrigramsH [x,y] = []

------------------------------------------------------------------------------------
--------------------------------bigramsFreq-----------------------------------------
bigramsFreq :: Num a => [String]-> [((String,String),a)]
bigramsFreq x 
	| x == [] = [(("",""),0)]
	| length x == 1 = [((head x,""),1)]
	| otherwise = bigrams (uniqueBigrams x) (uniqueBigramsH x) 

bigrams :: Num a => [(String,String)]->[(String,String)]->[((String,String),a)]
bigrams [] _ = []
bigrams (element:xs) list = (element,count element list): bigrams xs list


count :: Num a => (String,String) -> [(String,String)] -> a
count x [] = 0
count x (y:ys) 
	| x==y = 1 + count x ys
        | otherwise = count x ys



------------------------------------------------------------------------------------
--------------------------------TrigramsFreq----------------------------------------
trigramsFreq :: Num a => [String]-> [((String,String,String),a)]
trigramsFreq x 
	| x == [] = [(("","",""),0)]
	| length x == 1 = [((head x,"",""),1)]
	| length x == 2 = [((head x, last x,""),1)]
	| otherwise = trigrams (uniqueTrigrams x) (uniqueTrigramsH x) 

trigrams :: Num a => [(String,String,String)]->[(String,String,String)]->[((String,String,String),a)]
trigrams [] _ = []
trigrams (element:xs) list = (element,count2 element list): trigrams xs list


count2 :: Num a => (String,String,String) -> [(String,String,String)] -> a
count2 x [] = 0
count2 x (y:ys) 
	| x==y = 1 + count2 x ys
        | otherwise = count2 x ys

------------------------------------------------------------------------------------
--------------------------------getfreq---------------------------------------------

getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
getFreq item [] = error "item not in the list"
getFreq item ((a,b):t) = if( item==a ) then b
			else getFreq item t

------------------------------------------------------------------------------------
--------------------------------generateOneProb ------------------------------------

generateOneProb :: Fractional a => ((String,String,String),a) -> [((String,String),a)] -> a
generateOneProb ((a,b,c),countoftriple) [] = error "item not in the list #2"
generateOneProb ((a,b,c),countoftriple) (((d,e),countofpair):tail)
				 | a==d && b==e = countoftriple/countofpair
			         | otherwise = generateOneProb ((a,b,c),countoftriple) tail

------------------------------------------------------------------------------------
--------------------------------genProbPairs----------------------------------------

genProbPairs :: Fractional a => [((String,String,String),a)] -> [((String,String),a)] -> [((String,String,String),a)]

genProbPairs [] pairs = []
genProbPairs (((a,b,c),count):ttriples) pairs = ((a,b,c),(generateOneProb ((a,b,c),count) pairs)):(genProbPairs ttriples pairs)

------------------------------------------------------------------------------------
--------------------------------generateNextWord------------------------------------
generateNextWord :: (Ord a, Fractional a) => [String] ->[((String,String,String),a)] -> String
generateNextWord _ [] = error "Sorry, it is not possible to infer from current database"
generateNextWord [x1,x2] list
	|e1==x1 && e2==x2 && count>0.03 = e3
	|otherwise = generateNextWord [x1,x2] (removeElement ((e1,e2,e3),count) list)
	where ((e1,e2,e3),count) = list !! (randomZeroToX ((length list)-1))


removeElement _ [] = []
removeElement x (y:ys)
	| x == y = removeElement x ys
	| otherwise = y : removeElement x ys

------------------------------------------------------------------------------------
--------------------------------generateText----------------------------------------

generateText :: String -> Int -> String
generateText [] _ = []
generateText str num 
	|length (wordToken newStr) /= num = error "Sorry, it is not possible to infer from current database"
	|otherwise = str ++ newStr
	where newStr = generatetexthelper (wordToken str) num

generatetexthelper :: [String] -> Int -> String
generatetexthelper [x,x1] num
	| num == 0 = ""
	| elem (head newStr) punct = newStr ++ generatetexthelper [x1, newStr] (num-1)
	| otherwise = " " ++ newStr ++ generatetexthelper [x1, newStr] (num-1)
	where newStr = generateNextWord [x,x1] (genProbPairs (trigramsFreq (wordTokenList docs)) (bigramsFreq (wordTokenList docs)))

------------------------------------------------------------------------------------




--evaluation

updateGenerateOneProb :: Fractional a => ((String,String),a) -> [(String,a)] -> a
updateGenerateOneProb ((a,b),countofdouble) [] = error "item not in the list"
updateGenerateOneProb ((a,b),countofdouble) ((d,countofone):tail)
				 | a==d = countofdouble/countofone
			         | otherwise = updateGenerateOneProb ((a,b),countofdouble) tail



updatedGenProbPairs:: Fractional a => [((String,String),a)] -> [(String,a)] -> [((String,String),a)]
updatedGenProbPairs [] singles = []
updatedGenProbPairs (((a,b),count):tdoubles) singles = ((a,b),(updateGenerateOneProb ((a,b),count) singles)):(updatedGenProbPairs  tdoubles singles)



updatedGenProbPairs1 [] _ = []
updatedGenProbPairs1 (((a,b),prob):xs) list = ((a,b),prob/x) : updatedGenProbPairs1 xs list where x = getFreq a list
