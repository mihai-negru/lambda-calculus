import           Expr
import           Lambda
import           Parser
import           System.IO
import           Tests.Examples

-- | Default dictionary for macros
defaultCtx :: [(String, Expr)]
defaultCtx = [ ("true", ltrue)
              , ("false", lfalse)
              , ("and", land)
              , ("or", lor)
              , ("not", lnot)
              , ("zero", zero)
              , ("succ", nsucc)
              , ("add", add)
              , ("mult", mult)
              , ("iszero", iszero)
              , ("m", m)
              , ("i", i)
              , ("k", k)
              , ("ki", ki)
              , ("c", c)
              , ("b", b)
              , ("y", y)
              ]


{- |
    Takes one line of code and evaluates it to a betta-normal
    lambda expression or updated the macro dictionary

    If a error is thrown the program crashes
-}
lambda :: [(String, Expr)] -> IO ()
lambda ctx = do
    putStr "λ> "
    hFlush stdout
    input <- getLine
    case input of
        "exit" -> return ()
        _ -> case parse_code input of
                (Evaluate e) -> do
                    print (reduceN (evalMacros ctx e))
                    lambda ctx
                (Assign s e) -> do
                    lambda ((s, evalMacros ctx e):ctx)


{- |
    Main function of the program, calls the lambda looper
    with the default macro dictionary
-}
main :: IO ()
main = lambda defaultCtx
