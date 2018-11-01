-- definition of expression
data Exp = Var String          -- variable
         | Apply Exp Exp         -- application
         | Lambda String Exp      -- abstraction
         | N   Int             -- integer
         | Op  String Exp Exp  -- operator (a special form of function that takes two params)
	   | Lc  String Exp Exp -- logic connective
	   | Not Exp -- Not
	   | B   Bool -- Boolean
	   | Let String Exp Exp -- Let expression
	   | If Bool Exp Exp -- If expression
	   | Apply1 Exp Exp -- application such as (\x. x b)
	   | Str String -- String
	   | Lt Exp Exp -- Less than
	   | Lte Exp Exp -- Less than or Equal
         deriving (Eq, Show)

-- value definition
data Val = VN   Int                        -- integer
         | CLambda String Exp [(String, Val)] -- closure of lambda and env
	   | BN   Bool -- Bool
	   | SN   String -- String
         deriving (Eq, Show)

-- user interface, create an empty env as initial env and do renaming.
interpE :: Exp -> Maybe Val
interpE exp = do
	a1 <- interp [] (rename exp)
	case a1 of 
		(CLambda x y z) -> do
			a2 <- interp z y
			case a2 of
				(SN s) -> Just (SN ("Lambda " ++ x ++ "." ++ s))	
		_ -> Just a1 

-- rename
rename :: Exp -> Exp
rename (Var x) = Var (x ++ "0")
rename (N x) = N x
rename (B x) = B x
rename (Str x) = Str x
rename (Lambda x y) = Lambda (x ++ "0") (rename y)
rename (Apply x y) = Apply (rename x) (rename y)
rename (Op x y z) = Op x (rename y) (rename z)
rename (Lc x y z) = Lc x (rename y) (rename z)
rename (Not x) = Not (rename x)
rename (Let x y z) = Let (x ++ "0") (rename y) (rename z)
rename (Apply1 x y) = Apply1 (rename x) (rename y)
rename (If x y z) = If x (rename y) (rename z)
rename (Lt x y) = Lt (rename x) (rename y)
rename (Lte x y) = Lte (rename x) (rename y)

-- evalation function
interp :: [(String, Val)] -> Exp -> Maybe Val

-- deal with variable
interp []           (Var x) = Just (SN x)
interp ((x',v):env) (Var x) = if x == x' then Just v else interp env (Var x)

-- deal with value
interp env (N x) = Just (VN x)
interp env (B x) = Just (BN x)
interp env (Str x) = Just (SN x)

-- deal with lambda expression
interp env (Lambda x body) = Just (CLambda x body env)

-- deal with bool expression
interp env (Lc lc x y) = do
	a1 <- interp env x
	a2 <- interp env y
	applyLc lc a1 a2
                                 
-- deal with Not expression
interp env (Not x) = do
	a1 <- interp env x
	applyNot a1

-- deal with If
interp env (If x y z) = do	
	if x then 
	 interp env y
	else
	 interp env z

-- deal with Less than
interp env (Lt x y) = do	
	a1 <- interp env x
	a2 <- interp env y
	lt a1 a2

-- deal with Less than or equal
interp env (Lte x y) = do	
	a1 <- interp env x
	a2 <- interp env y
	lte a1 a2

-- deal with arithmetic expression
interp env (Op op x y) = do
	a1 <- interp env x
	a2 <- interp env y
	applyOp op a1 a2                                 

-- deal with let
interp env (Let x y z) = do
	a1 <- interp env y
	interp ((x,a1):env) z

-- deal with application1
interp env (Apply1 exp1 exp2) = do
	e1 <- interp env exp1
	e2 <- interp env exp2
	apply1 e1 e2

-- deal with application
interp env (Apply f arg) = do
	e1 <- interp env f
	e2 <- interp env arg
	applyLambda e1 e2

-- arithmetic
applyOp :: String -> Val -> Val -> Maybe Val
applyOp "+" (VN x) (VN y) = Just (VN (x + y))
applyOp "-" (VN x) (VN y) = Just (VN (x - y))
applyOp "*" (VN x) (VN y) = Just (VN (x * y))
applyOp "/" (VN x) (VN y) = Just (VN (x `div` y))

-- logic
applyLc :: String -> Val -> Val -> Maybe Val
applyLc "and" (BN x) (BN y) = Just (BN (x && y))
applyLc "or" (BN x) (BN y) = Just (BN (x || y))

-- not
applyNot :: Val -> Maybe Val
applyNot (BN x) = Just (BN (not x))

-- less than
lt :: Val -> Val -> Maybe Val
lt (VN x) (VN y) = Just (BN (x < y))

-- less than or equal
lte :: Val -> Val -> Maybe Val
lte (VN x) (VN y) = Just (BN (x <= y))

-- application, extend env.
applyLambda :: Val -> Val -> Maybe Val
applyLambda (CLambda x body env) arg = interp ((x,arg):env) body

-- application such as \x.x b
apply1 :: Val -> Val -> Maybe Val
apply1 (SN x) (SN y) = Just (SN (x ++ " " ++ y))


-- test cases. "interpE test1"
-- (\x.(x+3))2
test1 = (Apply (Lambda "x" (Op "+" (N 3) (Var "x"))) (N 2))

-- ((\x.\y.(x-y))3)2
test2 = (Apply (Apply ((Lambda "x" (Lambda "y" (Op "-" (Var "y") (Var "x"))))) (N 3)) (N 2))

-- ((\x.\y.(x-y))3)test1
test3 = (Apply (Apply ((Lambda "x" (Lambda "y" (Op "-" (Var "y") (Var "x"))))) (N 3)) test1)

-- (\x.(true and x))false
test4 = (Apply (Lambda "x" (Lc "and" (B True) (Var "x"))) (B False))

-- let x=5 in 3+x
test5 = (Let "x" (N 5) (Op "+" (N 3) (Var "x")))

-- (\x.\y. y x)y
test6 = (Apply (Lambda "x" (Lambda "y" (Apply1 (Var "y") (Var "x")))) (Str "y"))

-- ((Let x=3 in (x+5)) <= 8)
test7 = (Lte (Let "x" (N 3) (Op "+" (N 5) (Var "x"))) (N 8)) 
                    