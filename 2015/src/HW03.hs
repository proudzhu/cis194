module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st name val x = if x == name then val else st x

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------
-- | Convert Bool to Int
--
-- >>> boolToInt False
-- 0
--
-- >>> boolToInt True
-- 1
boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

-- | Eval expression
--
-- >>> evalE empty (Val 5)
-- 5
--
-- >>> evalE empty (Op (Val 1) Eql (Val 2))
-- 0
evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE st (Val i) = i
evalE st (Op lhs op rhs) =
  case op of
    Plus -> x + y
    Minus -> x - y
    Times -> x * y
    Divide -> x `div` y
    Gt     -> boolToInt (x > y)
    Ge     -> boolToInt (x >= y)
    Lt     -> boolToInt (x < y)
    Le     -> boolToInt (x <= y)
    Eql    -> boolToInt (x == y)
  where x = evalE st lhs
        y = evalE st rhs


-- Exercise 3 -----------------------------------------
-- | desugar
--
-- >>> desugar (Incr "A")
-- DAssign "A" (Op (Var "A") Plus (Val 1))
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e) = DAssign s e
desugar (Incr e)     = DAssign e (Op (Var e) Plus (Val 1))
desugar (If e s1 s2) = DIf e (desugar s1) (desugar s2)
desugar (While e s) = DWhile e (desugar s)
desugar (For s1 e s2 s3) = DSequence (desugar s1) (DWhile e (DSequence (desugar s3) (desugar s2)))
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar Skip = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s e) = extend st s (evalE st e)
evalSimple st (DIf e s1 s2) = if evalE st e == 1 then evalSimple st s1 else evalSimple st s2
evalSimple st w@(DWhile e x)  = if evalE st e == 1 then evalSimple (evalSimple st x) w else st
evalSimple st (DSequence x y) = evalSimple (evalSimple st x) y
evalSimple st DSkip = st

run :: State -> Statement -> State
run st s = evalSimple st $ desugar s

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
