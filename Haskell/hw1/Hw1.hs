module Hw1 where

type Mapping = [(String, String, String)]
data AST = EmptyAST | ASTNode String AST AST deriving (Show, Read)

writeExpression :: (AST, Mapping) -> String
evaluateAST :: (AST, Mapping) -> (AST, String)
-- DO NOT MODIFY OR DELETE THE LINES ABOVE -- 
-- IMPLEMENT writeExpression and evaluateAST FUNCTION ACCORDING TO GIVEN SIGNATURES -- 


pls aa bb =  "(" ++ aa ++ "+" ++ bb ++ ")"
tms aa bb = "(" ++ aa ++ "*" ++ bb ++ ")"
neg aa = "(-" ++ aa ++ ")"
cnt aa bb =  "(" ++ aa ++ "++" ++ bb ++ ")"
le aa = "(" ++"length " ++ aa ++ ")"

letExp (st1,st2,st3) 
    | st2 == "num" = st1++"="++st3
    | st2 == "str" =   st1++"="++ "\"" ++ st3 ++"\""
    | otherwise =  st1++"="++st3
mapletexp  aa = if aa == [] then "" else "let " ++ putter (map letExp aa) ++ " in "

putter (x:[]) = x 
putter [] = ""
putter (x:xs) =  x ++ ";"  ++ (putter xs) 

getvar (ASTNode kl lefAST rigAST ) 
    | kl == "num" = getvar lefAST
    | kl == "str" =  "\"" ++ (getvar lefAST) ++"\""
    | kl == "plus" =  pls (getvar lefAST) (getvar rigAST)
    | kl == "times" =  tms (getvar lefAST) (getvar rigAST)
    | kl == "negate" =  neg (getvar lefAST) 
    | kl == "cat" = cnt (getvar lefAST) (getvar rigAST)
    | kl == "len" = le (getvar lefAST)
    | otherwise = kl

writeExpression (ASTNode kl lefAST rigAST , kos) 
    | kl == "num" =   (getvar lefAST)
    | kl == "str" =    (getvar lefAST)
    | kl == "plus" = letto ++ (pls (getvar lefAST) (getvar rigAST))
    | kl == "times" = letto ++ (tms (getvar lefAST) (getvar rigAST))
    | kl == "negate" = letto ++ (neg (getvar lefAST) )
    | kl == "cat" = letto ++ (cnt (getvar lefAST) (getvar rigAST))
    | kl == "len" = letto ++ (le (getvar lefAST))
    | otherwise = letto ++ kl
    where letto =  (mapletexp kos)
    
evaluateAST (ASTNode kl lefAST rigAST , variables ) =    allres (ASTNode kl lefAST rigAST , variables ) 

allres :: (AST, Mapping) -> (AST, String)
allres (ASTNode kl lefAST rigAST , variables )
    | kl == "num" =   ( (runner (ASTNode kl lefAST rigAST  ) variables), value)
    | kl == "str" =    ( (runner (ASTNode kl lefAST rigAST  ) variables),  (calculate (runner (ASTNode kl lefAST rigAST  ) variables)))
    | kl == "plus" = ( (runner (ASTNode kl lefAST rigAST  ) variables), value)
    | kl == "times" = ( (runner (ASTNode kl lefAST rigAST  ) variables), value)
    | kl == "negate" = ( (runner (ASTNode kl lefAST rigAST  ) variables), value)
    | kl == "cat" = ( (runner (ASTNode kl lefAST rigAST  ) variables),  (calculate (runner (ASTNode kl lefAST rigAST  ) variables)))
    | kl == "len" = ( (runner (ASTNode kl lefAST rigAST  ) variables),  (calculate (runner (ASTNode kl lefAST rigAST  ) variables)))
    | otherwise = ( (runner (ASTNode kl lefAST rigAST  ) variables), value)
    where value = show (calculatenu ((runner (ASTNode kl lefAST rigAST  ) variables)))


runner :: AST -> Mapping -> AST
runner ast (x:[]) = (search (ast,[x]))  
runner ast [] = ast

runner ast (x:xs) =  runner (search (ast,[x])) xs



valnum aa = read aa :: Int 
add aa bb = aa+bb


calculate :: AST -> String
calculate (ASTNode kl lefAST rigAST ) 
    | kl == "str" =  calculate lefAST
    | kl == "cat" =  (calculate lefAST) ++ (calculate rigAST)
    | kl == "len" = show (length (calculate lefAST)) 
    | otherwise = kl

calculatenu :: AST -> Int
calculatenu (ASTNode kl lefAST rigAST ) 
    | kl == "num" = calculatenu lefAST
    | kl == "plus" =    ((calculatenu lefAST) + (calculatenu rigAST))
    | kl == "times" =   ((calculatenu lefAST) * (calculatenu rigAST))
    | kl == "negate" =  (-1) * (calculatenu lefAST) 
    | kl == "len" = len' ((calculate lefAST)) 
    | otherwise =  valnum kl 

len' :: [a] -> Int
len' [] = 0
len' (_:xs) = 1 + len' xs




search :: (AST,Mapping) -> AST
search (ASTNode kl lefAST rigAST , [(st1,st2,st3)]) 
    |( kl == st1)   =  (ASTNode st2 (ASTNode st3 EmptyAST EmptyAST) EmptyAST)
    | kl == "num" =   (ASTNode kl (search (lefAST,[(st1,st2,st3)])) EmptyAST)
    | kl == "str" =    (ASTNode kl (search (lefAST,[(st1,st2,st3)])) EmptyAST)
    | kl == "plus" = (ASTNode kl (search (lefAST,[(st1,st2,st3)])) (search (rigAST,[(st1,st2,st3)])))
    | kl == "times" = (ASTNode kl (search (lefAST,[(st1,st2,st3)])) (search (rigAST,[(st1,st2,st3)])))
    | kl == "negate" = (ASTNode kl (search (lefAST,[(st1,st2,st3)])) EmptyAST)
    | kl == "cat" = (ASTNode kl (search (lefAST,[(st1,st2,st3)])) (search (rigAST,[(st1,st2,st3)])))
    | kl == "len" = (ASTNode kl (search (lefAST,[(st1,st2,st3)])) EmptyAST)
    | otherwise = (ASTNode kl lefAST rigAST)

