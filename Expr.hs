module Expr where

{- |
  Lambda expression syntax tree.
-}
data Expr =
  Variable String         -- ^ Variable constructor expressed as a String
  | Function String Expr  -- ^ Function constructor containing a parameter name and a body
  | Application Expr Expr -- ^ Application constructor containing two lambda expressions
  | Macro String          -- ^ Macro constructor to define an expression encapsulated in a String


{- |
  Line of code syntax tree.
-}
data Code =
  Evaluate Expr           -- ^ Evaluate constructor which takes and expression and evalutes it
  | Assign String Expr    -- ^ Assign constructor to assign an expression to a String 'macro'
  deriving (Eq, Show)


-- | Shorthand definition for Variable Constructor
v = Variable

-- | Shorthand definition for Function Constructor
f = Function

-- | Shorthand definition for Application Constructor
a = Application

-- | Shorthand definition for Macro Constructor
macro = Macro


{- |
  Instance the Show typeclass for Lambda Expressions
  in order to print is a user friendly format, using
  lambda annotations
-}
instance Show Expr where
    show :: Expr -> String
    show (Variable x) = x
    show (Function x (Application e1 e2)) = ('λ' : x) ++ ".(" ++ show e ++ ")"
        where e = Application e1 e2
    show (Function x e) = ('λ' : x) ++ ('.' : show e)
    show (Application e1 (Application u v)) = show e1 ++ " (" ++ show e2 ++ ")"
        where e2 = Application u v
    show (Application e1 e2) = show e1 ++ (' ': show e2)
    show (Macro m) = m


{- |
  Instance the Eq typeclass for Lambda Expressions
  in order to evaluate an expression, which has different
  variable or abstraction names, however form the same structure

  __Examples:__

  @
  λx.x is equal with λy.y
  (x p) is equal with (m n)
  y is equal with z
  @
-}
instance Eq Expr where
    (==) :: Expr -> Expr -> Bool
    (==) e1 e2 = equal e1 e2 []
      where
        equal :: Expr -> Expr -> [(String, String)] -> Bool
        equal (Variable x) (Variable y) env = case lookup x env of
                                                (Just xv) -> xv == y
                                                Nothing   -> x == y

        equal (Function x e1) (Function y e2) env = equal e1 e2 ((x,y):env)
        equal (Application e1 e2) (Application e3 e4) env = equal e1 e3 env && equal e2 e4 env
        equal (Macro m1) (Macro m2) _ = m1 == m2
        equal _ _ _ = False
