{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parser (parse_expr, parse_code) where

import           Control.Applicative
import           Control.Monad
import           Expr

{- |
  Parser data type definition

  Convention:
    Parser returns a Maybe with the syntax tree parsed and
    the rest of the String that need to be parser or Nothing
    in case some error occured

  Convetion Lambda Expressions:
    If the Parser returns nothing an error occured
    If the Parser returns a Just and the remaining String
    is not empty that an error occured, because an expression
    needs to be parsed fully
    If the Parser returns a Just and the remaining String
    is empty that the expression was parssed successfully

  Convention Code Expressions:
    Same as convention for lambda expressions
-}
newtype Parser a = Parser {
  parse :: String -> Maybe(a, String)
}

--- Define Monadic Behavior for the Parser datatypes ---

instance Monad Parser where
  (>>=) mp f = Parser $ \s -> case parse mp s of
    Nothing      -> Nothing
    Just (x, s') -> parse (f x) s'

  return x = Parser $ \s -> Just (x, s)

{- |
 Instance the Applicative typeclass in order to chain Parsers
-}
instance Applicative Parser where
  pure x = return x

  pf <*> px =
    do
      f <- pf
      x <- px
      return $ f x


{- |
  Instance Functor typeclass for structure-preserving transformations for Parser
-}
instance Functor Parser where
  fmap f px =
    do
      x <- px
      return $ f x


{- |
  Instance Alternative typeclass in order to pick another Parser in case ones fails
  and to be able to use 'some' and 'many' functions
-}
instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> case parse p1 s of
    Nothing -> parse p2 s
    x       -> x


--- Utils function for parsing Chars and Strings ---


{- |
  Checks if a Char is a small letter
-}
isSmallLetter :: Char -> Bool
isSmallLetter c = c `elem` ['a'..'z']


{- |
  Parser for only one character
-}
charac :: Char -> Parser Char
charac c = Parser charP
  where
    charP [] = Nothing
    charP (x:xs)
      | x == c = Just (c, xs)
      | otherwise = Nothing


{- |
  Parser that satisfy a boolean expression over for one character
-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser predP
  where
    predP [] = Nothing
    predP (x:xs)
      | p x = Just (x,xs)
      | otherwise = Nothing


{- |
  Parser for a String containing just one small letter
-}
strchar :: Parser String
strchar = (:[]) <$> satisfy isSmallLetter


{- |
  Parser for a String containing one or many small letters
-}
string :: Parser String
string = some $ satisfy isSmallLetter


{- |
  Parser for a String containing zero or some spaces
-}
spaces :: Parser String
spaces = many $ charac ' '


--- Utils for parsing Lambda Expressions ---


{- |
  Parses a String into a valid Lambda Expression syntax tree

  If parser fails than an error is thrown
-}
parse_expr :: String -> Expr
parse_expr s = case parse expression s of
  Nothing     -> error "Could not parse lambda at all."
  Just(e, []) -> e
  _           -> error "Parsed just a part of the lambda."


{- |
  Parser for a Lambda Expression

  An expression can either be an Application, Function or an Item

  First the function tries to parse an application then a function
  in order not to permit to a function to consume all the expressions
  from the right side
-}
expression :: Parser Expr
expression = application
          <|> function
          <|> item


{- |
  Parser for a Lambda Application

  An Application is a chain of items

  Because Application have left associativity the chain of items
  are folded to left

  An application can reduce also to one item if no more items can be found
-}
application :: Parser Expr
application = foldl1 Application
           <$> some item


{- |
  Parser for a Lambda Function

  A function can have zero or some spaces, then a separator of '\'
  which tells that the expression is a function, then it should be
  followed by a variable string containing one letter, which can be
  arounded by zero or some spaces, then followed by a separator '.',
  which tells that the following expression will be the body of the function

  A body of the function can be just an item
-}
function :: Parser Expr
function = Function
        <$> (spaces *> charac '\\' *> spaces *> strchar <* spaces <* charac '.')
        <*> item


{- |
  Parser for a Lambda Item

  An Item can be a variable, a macro, a expression arounded by paranthesis or a function

  From an item can come out an application, if and only if the grouping reduces to an application
-}
item :: Parser Expr
item = variable
    <|> macros
    <|> grouping
    <|> function


{- |
  Parser for a Lambda Variable

  A Variable can start with zero or some spaces and should be evaluated
  at a String containing just one small letter
-}
variable :: Parser Expr
variable = Variable <$> (spaces *> strchar)


{- |
  Parser for a Lambda Macro

  A Macro can start with zero or some spaces and should be separated with the '$'
  sign, which tells that the macro name begins, the macro name is a simple String containing
  just small letters
-}
macros :: Parser Expr
macros = Macro <$> (spaces *> charac '$' *> string)


{- |
  Parser for a Lambda Group (or a paranthesised expression)

  A Group is a Lambda expression arounded with paranthesises and spaces
-}
grouping :: Parser Expr
grouping = spaces *> charac '(' *> expression <* spaces <* charac ')'


--- Utils for parsing Code Expressions ---


{- |
  Parses a String into a valid Code Expression syntax tree

  If parser fails than an error is thrown
-}
parse_code :: String -> Code
parse_code s = case parse code s of
  Nothing     -> error "Could not parse code at all."
  Just(e, []) -> e
  _           -> error "Parsed just a part of the code."


{- |
  Parser for a Code Expression

  A Code can be either an assignment or an evaluation
-}
code :: Parser Code
code = assignment
    <|> evaluation


{- |
  Parser for a Code Assignment

  An Assignment can start with zero or some spaces then continues with a string
  name contianing just small letters then separated by '=' which can be arounded by
  spaces and then a Lambda Expression
-}
assignment :: Parser Code
assignment = Assign
          <$> (spaces *> string <* spaces <* charac '=')
          <*> expression


{- |
  Parser for a Code Evaluation

  An Evaluation is just a simple Lambda expression
-}
evaluation :: Parser Code
evaluation = Evaluate
          <$> expression
