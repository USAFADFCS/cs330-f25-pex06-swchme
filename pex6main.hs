-- pex6.hs 
-- unKnot Haskell

-- name: Sweta Chandra Mohan

{- 
DOCUMENTATION:
https://www.referencecollection.com/references/haskell_reference.hs.html
Used Gavin Smith's test cases for my code. 
https://en.wikibooks.org/wiki/Haskell/Pattern_matching used this to learn mroe about pattern matchig 

-}

-- this funciton takes trip code and will apply type one and two moves on it until there are more
-- more moves possible 
-- it is tangled until there are no more moves or it is a knot. 
unKnot :: [(Char, Char)] -> String
unKnot tripCode

   -- base case menas there are no crossses left. 
   | null tripCode = "not a knot"

   -- Type i where there is no wrap 
   | typeOneMoveExistsInThisTripCode tripCode = unKnot (makeTypeOneMoveInThisTripCode tripCode)

   -- type i with a wrap 
   | typeOneMoveExistsInThisTripCode (wrapThisTripCode tripCode) = unKnot (makeTypeOneMoveInThisTripCode (wrapThisTripCode tripCode))

   -- Type ii without wrap 
   | typeTwoMoveExistsInThisTripCode tripCode = unKnot (makeTypeTwoMoveInThisTripCode tripCode)

   -- Type ii with a wrap 
   | typeTwoMoveExistsInThisTripCode (wrapThisTripCode tripCode) = unKnot (makeTypeTwoMoveInThisTripCode (wrapThisTripCode tripCode))

   -- no moves left
   | otherwise =
        "tangle - resulting trip code: " ++ show tripCode


--implimenting the wrapping 
--

wrapThisTripCode :: [(Char, Char)] -> [(Char, Char)]
wrapThisTripCode trip = wrap trip

-- moving the first pair to the end of the list

wrap :: [(Char, Char)] -> [(Char, Char)]
wrap [] = [] -- empty list stays that way 
wrap (first:rest) = rest ++ [first] -- move first elemnt to the end 


--detection of a type I crosss

typeOneMoveExistsInThisTripCode :: [(Char, Char)] -> Bool
typeOneMoveExistsInThisTripCode trip = typeOneMoveExists trip

--sees if there are any pair of crossings that are right 
-- near each other with the same cross name 

typeOneMoveExists :: [(Char, Char)] -> Bool
typeOneMoveExists [] = False
typeOneMoveExists [x] = False
typeOneMoveExists ((c1,t1):(c2,t2):others)
-- if they share the same name, type 1 is possible 
   | c1 == c2  = True
   -- otherwise,keep cheking and more forward one elemnt. 
   | otherwise = typeOneMoveExists ((c2,t2):others)


--executing the type I cross 
makeTypeOneMoveInThisTripCode :: [(Char, Char)] -> [(Char, Char)]
makeTypeOneMoveInThisTripCode trip = makeTypeOneMove trip


--will rmeove the pair of crossings that have the same name 

makeTypeOneMove :: [(Char, Char)] -> [(Char,  Char)]
makeTypeOneMove [] = [] -- base case 
makeTypeOneMove [p] = [p] -- incase theres only one elemt 
makeTypeOneMove ((c1,t1):(c2,t2):others)
-- if theres a matching pair, both have to be dropped 
   | c1 == c2  = others
   | otherwise = (c1,t1) : makeTypeOneMove ((c2,t2):others)


--detection of a type two cross in the code but a idfferent tpye later in the cod e

typeTwoMoveExistsInThisTripCode :: [(Char, Char)] -> Bool 
typeTwoMoveExistsInThisTripCode trip =  typeTwoMoveExists trip

--
typeTwoMoveExists :: [(Char, Char)] -> Bool
typeTwoMoveExists [] = False
typeTwoMoveExists [x] = False
typeTwoMoveExists ((c1,t1):(c2,t2):others)
   | t1 == t2 && secondTypeTwoPairExists c1 c2 t1 others = True
   | otherwise = typeTwoMoveExists ((c2,t2):others)


-- creating helper 

secondTypeTwoPairExists :: Char -> Char -> Char -> [(Char,Char)] -> Bool
secondTypeTwoPairExists _ _ _ [] = False
secondTypeTwoPairExists _ _ _ [x] = False
secondTypeTwoPairExists a b firstType ((x1,t1):(x2,t2):rest)
   | t1 == t2 && firstType /= t1  && ((a == x1 && b == x2) || (a == x2 && b == x1)) = True
   | otherwise = secondTypeTwoPairExists a b firstType ((x2,t2):rest)


--executing the type 2 cross using th ehelepr 

makeTypeTwoMoveInThisTripCode :: [(Char, Char)] -> [(Char, Char)]
makeTypeTwoMoveInThisTripCode trip = makeTypeTwoMove trip

makeTypeTwoMove :: [(Char, Char)] -> [(Char, Char)]
makeTypeTwoMove [] = []
makeTypeTwoMove [x] = [x]
makeTypeTwoMove ((c1,t1):(c2,t2):others)
--drop the pair if it is found later as well 
   | t1 == t2 && secondTypeTwoPairExists c1 c2 t1 others = removeSecondTypeTwoPair c1 c2 t1 others
   | otherwise = (c1,t1) : makeTypeTwoMove ((c2,t2):others)

--will remove the second pair that matches the type ii pattern 
removeSecondTypeTwoPair :: Char -> Char -> Char -> [(Char,Char)] -> [(Char,Char)]
removeSecondTypeTwoPair _ _ _ [] = []
removeSecondTypeTwoPair _ _ _ [p] = [p] -- no pair single 
removeSecondTypeTwoPair a b firstType ((x1,t1):(x2,t2):rest)
--drop if it completes the type two formation 
   | t1 == t2 && firstType /= t1 && ((a == x1 && b == x2) || (a == x2 && b == x1)) = rest
   -- keep the first crossing and continue recursing on the rest  
   | otherwise = (x1,t1) : removeSecondTypeTwoPair a b firstType ((x2,t2):rest)
-- The Test cases given to me by Gavin Smith 

{-

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print ("test case t01: " ++ show t01)
   print ("result: " ++ unKnot t01)
   
   -}
   
main :: IO ()
main = do
    let t01 = [('a','o'),('a','u')]
    print("   test case t01 - tripcode: " )
    print(t01)
    print("   result:" ++ unKnot t01) -- "Unknot"
    
    let t02 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
    print("   test case t02 - tripcode: " )
    print(t02)
    print("   result:" ++ unKnot t02) -- "Unknot"
 
    let t03 = [('a','u'),('b','u'),('a','o'),('b','o')]
    print("   test case t03 - tripcode: " )
    print(t03)
    print("   result:" ++ unKnot t03) -- "Unknot"
 
    let t04 = [('a','o'),('b','u'),('a','u'),('b','o')]
    print("   test case t04 - tripcode: " )
    print(t04)
    print("   result:" ++ unKnot t04) -- "Unknot"
 
    let t05 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
    print("   test case t05 - tripcode: " )
    print(t05)
    print("   result:" ++ unKnot t05) -- "Unknot"
     
    let t06 = [('a','o'),('q','u'),('a','u')]
    print("   test case t06 - tripcode: " )
    print(t06)
    print("   result:" ++ unKnot t06) -- [('q','u')]
     
    let t07 = [('a','o'),('a','u'),('q','u')]
    print("   test case t07 - tripcode: " )
    print(t07)
    print("   result:" ++ unKnot t07) -- [('q','u')]
     
    let t08 = [('a','o'),('b','o'),('a','u'),('b','u'),('q','u')]
    print("   test case t08 - tripcode: " )
    print(t08)
    print("   result:" ++ unKnot t08) -- [('q','u')]
     
    let t09 = [('a','u'),('b','o'),('a','o'),('b','u'),('q','u')]
    print("   test case t09 - tripcode: " )
    print(t09)
    print("   result:" ++ unKnot t09) -- [('a','o'),('b','o'), ('a','u'), ('b','u') ('q','u')]
     
    let t10 = [('a','u'),('b','o'),('a','o'),('b','u'),('q','u'),('c','o'),('c','u')]
    print("   test case t10 - tripcode: " )
    print(t10)
    print("   result:" ++ unKnot t10) -- [('a','o'),('b','o'), ('a','u'), ('b','u') ('q','u')]
     
    let t11 = [('a','u'),('b','o'),('a','o'),('q','u'),('b','u'),('c','o'),('c','u')]
    print("   test case t11 - tripcode: " )
    print(t11)
    print("   result:" ++ unKnot t11) -- [('q','u')]
     
    let t12 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('q','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
    print("   test case t12 - tripcode: " )
    print(t12)
    print("   result:" ++ unKnot t12) -- [('q','u')]
            

