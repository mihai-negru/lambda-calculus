{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lambda where

import           Data.List
import           Expr


{- |
  Generates a list of free variable names of a Lambda Expression

  A free variable is a variable that is not bound to any abstraction parameters

  Function generates the list of bounded variables and when matches a Variable checks
  if the name is contained in the list of bounded variables, if not it is 'cons'-ed to
  the list of the free variables
-}
free_vars :: Expr -> [String]
free_vars expr = freeVarsAux expr []
  where
    freeVarsAux (Variable x) bounded        = [x | x `notElem` bounded]
    freeVarsAux (Function x body) bounded   = freeVarsAux body (x : bounded)
    freeVarsAux (Application e1 e2) bounded = nub (freeVarsAux e1 bounded ++ freeVarsAux e2 bounded)


{- |
  Checks if a Lambda Expression contains a variable of the input name

  The function checks recursively the entire expression syntax tree until
  found a match

  Returns True if the input name was found in the expression or False otherwise
-}
hasVar :: String -> Expr -> Bool
hasVar var = hasVarAux
  where
    hasVarAux (Variable x)        = var == x
    hasVarAux (Function x e)      = var == x || hasVarAux e
    hasVarAux (Application e1 e2) = hasVarAux e1 || hasVarAux e2


{- |
  Generates a new variable name based on a variable stream
  used for textual substitution

  The generated name will have the form "z$", where '$' can
  be any natural number from [1..inf] range

  The generated name will not be found in the input Lambda Expression

  __Examples:__

  @
  λx.x -> generates z1
  λz1.z1 -> generates z2
  (λz3.z3 λz2.z2) -> generates z1 for first and z1 for second as well
  @
-}
genVar :: Expr -> String
genVar e = genVarAux 0
  where
    genVarAux c
      | hasVar (varNames !! c) e = genVarAux (c+1)
      | otherwise = varNames !! c

    varNames = ["z" ++ show x| x <- [1..]]


{- |
  Reduces a redex taking in consideration name colissions

  The reduction is propagated recursevily in the entire expression

  __Examples:__

  @
  (λx.body e) -> reduce -> body[e\x]
  @
-}
reduce :: Expr -> String -> Expr -> Expr
reduce expr1 x expr2 = reduce_aux expr1
  where
    reduce_aux var@(Variable y)
      | y == x                        = expr2
      | otherwise                     = var
    reduce_aux (Application e1 e2)    = Application (reduce_aux e1) (reduce_aux e2)
    reduce_aux func@(Function y body)
      | y == x                        = func
      | y `notElem` free_vars expr2   = Function y $ reduce_aux body
      | otherwise                     = reduce_aux (Function newVar (reduce body y (Variable newVar)))
      where
        newVar = genVar func
    reduce_aux (Macro m)              = error "Cannot redex reduce a macro."


{- |
  Checks if a Lambda Expression is in betta normal form

  If a Lambda Expression is in lambda form than it cannot
  be reduces to any other expression

  __Examples:__

  @
  if e1 ->* e2 and e2 cannot be reduceed anymore then isBettaNormalForm e2 == True
  (->*) means reduces to
  @
-}
isBettaNormalForm :: Expr -> Bool
isBettaNormalForm (Variable _)                   = True
isBettaNormalForm (Application (Function _ _) _) = False
isBettaNormalForm (Application e1 e2)            = isBettaNormalForm e1 && isBettaNormalForm e2
isBettaNormalForm (Function _ body)              = isBettaNormalForm body
isBettaNormalForm (Macro m)                      = error "Cannot check betta normal for macro."


{- |
  Evaluates one step of the Lambda Expression using Normal Order
-}
stepN :: Expr -> Expr
stepN var@(Variable x)                    = var
stepN (Function x body)                   = Function x (stepN body)
stepN (Application (Function x body) e2)  = reduce body x e2
stepN (Application e1 e2)
  | not $ isBettaNormalForm e1            = Application (stepN e1) e2
  | otherwise                             = Application e1 (stepN e2)
stepN (Macro m)                           = error "Cannot evaluate a macro in normal order."


{- |
  Evaluates a Lambda Expression using Normal Order
-}
reduceN :: Expr -> Expr
reduceN expr
  | isBettaNormalForm expr  = expr
  | otherwise               = reduceN $ stepN expr


{- |
  Evaluates a Lambda Expression using Normal Order
  and return a List of Expressions of each step of the reduction
-}
reduceAllN :: Expr -> [Expr]
reduceAllN expr
  | isBettaNormalForm expr  = [expr]
  | otherwise               = expr : reduceAllN (stepN expr)


{- |
  Evaluates one step of the Lambda Expression using Applicative Order
-}
stepA :: Expr -> Expr
stepA var@(Variable _)                      = var
stepA (Function x body)                     = Function x (stepA body)
stepA (Application e1@(Function x body) e2)
  | not $ isBettaNormalForm e2              = Application e1 (stepA e2)
  | otherwise                               = reduce body x e2
stepA (Application e1 e2)
  | not $ isBettaNormalForm e1              = Application (stepA e1) e2
  | otherwise                               = Application e1 (stepA e2)
stepA (Macro m)                             = error "Cannot evaluate a macro in applicative order."


{- |
  Evaluates a Lambda Expression using Applicative Order
-}
reduceA :: Expr -> Expr
reduceA expr
  | isBettaNormalForm expr  = expr
  | otherwise               = reduceA $ stepA expr


{- |
  Evaluates a Lambda Expression using Applicative Order
  and return a List of Expressions of each step of the reduction
-}
reduceAllA :: Expr -> [Expr]
reduceAllA expr
  | isBettaNormalForm expr  = [expr]
  | otherwise               = expr : reduceAllA (stepA expr)


{- |
  Evaluates a Lambda Expression that may contain macros

  The dictionary should map all the macro names to valid lambda expressions
  that will be substituted into a new Lambda Expression syntax tree

  Throws an error if the macro name is not pressent in the current dictionary
-}
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros dict = evalMacrosAux
  where
    evalMacrosAux e@(Variable x)      = e
    evalMacrosAux (Function x body)   = Function x (evalMacrosAux body)
    evalMacrosAux (Application e1 e2) = Application (evalMacrosAux e1) (evalMacrosAux e2)
    evalMacrosAux (Macro m)           = case lookup m dict of
                                          Nothing -> error "Could not evaluate macros."
                                          Just e  -> evalMacrosAux e


{- |
  Evaluates a Code expression and applies a function over the evaluated lambdas

  If the line of code is an assignment than a new entry will be inserted into dictionary
  else the code will be evaluated based on the generated dictionary over the function iteration
-}
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode f codes = f <$> evalCodeAux codes []
  where
    evalCodeAux [] _ = []
    evalCodeAux (x:xs) dict = case x of
      Assign s e -> evalCodeAux xs ((s, e):dict)
      Evaluate e -> evalMacros dict e : evalCodeAux xs dict
