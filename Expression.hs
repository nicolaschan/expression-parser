import Utils
import Multivector
import Data.Typeable

data Primitive = Primitive String
data Token = 
    Function (Expression Token -> Either Error Token) | 
    NamedFunction String (Expression Token -> Either Error Token) |
    Number Double |
    Boolean Bool |
    Multivector4 (Multivector4 Double) |
    Impossible |
    Str String |
    List [Token] |
    Unknown String
data Expression a = Value a | Expression (Expression a) (Expression a) 

data Error = Error String [String]

displayToken :: String -> String -> [String] -> String
displayToken name value [] = "(" ++ name ++ ": " ++ value ++ ")"
displayToken name value meta = "(" ++ name ++ ": " ++ value ++ " " ++ show meta ++ ")"

instance Show Error where
    show (Error msg stack) = "Error: " ++ msg ++ "\n" ++ indent 2 (unlines $ map ("in " ++) stack)
instance Show Primitive where
    show (Primitive str) = str
instance Show Token where
    show (Function f) = displayToken "Partial" "a -> a" [] 
    show (NamedFunction sym f) = displayToken "NamedFunction" sym []
    show (Number num) = displayToken "Number" (show num) [] 
    show (Boolean bool) = displayToken "Boolean" (show bool) []
    show (Multivector4 mv) = displayToken "Multivector4" (show mv) []
    show (Str s) = s
    show (Unknown u) = displayToken "Unknown" u [] 
instance Show a => Show (Expression a) where
    show (Value x) = show x
    show (Expression e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")" 

operator :: String -> String 
operator str = 
    let op = splitParens str in
    if length op > 1 then addParens . unwords . init $ op else head op

argument :: String -> String
argument str = 
    let args = splitParens str in
    if length args >= 2 then last args else ""

parse :: String -> Expression Primitive
parse str
    | (length (splitParens str) > 1) || (count '(' str > 0) = 
        let op = operator str in
        let arg = argument str in
        if length arg == 0 
            then parse . removeParens $ op
            else Expression (parse (operator str)) (parse (argument str))
    | otherwise = Value (Primitive str)

instance Num Token where
    (Number x) + (Number y) = Number (x + y)
    (Number x) - (Number y) = Number (x - y)
    (Number x) * (Number y) = Number (x * y)
    abs (Number x) = Number (abs x) 
    signum (Number x) = Number (signum x)
    fromInteger = Number . fromInteger 

tokenNumber :: String -> Bool
tokenNumber str = subset "0123456789.-" str

numberConstructor :: Expression Token -> Either Error Token
numberConstructor expr = evaluate expr >>= \token -> case token of
    (Number n) -> Right $ Number n
    (Boolean True) -> Right $ Number 1
    (Boolean False) -> Right $ Number 0
    (Multivector4 (Scalar s)) -> Right $ Number s
    _ -> Left $ Error ("Cannot convert to number: " ++ show token) [show expr]

booleanConstructor :: Expression Token -> Either Error Token
booleanConstructor expr = numberConstructor expr >>= \(Number n) -> Right $ Boolean (n /= 0) 

{-
lexer :: String -> Token 
lexer "+" = symbolize (\[a,b] -> a + b) "+" 2 
lexer "*" = symbolize (\[a,b] -> a * b) "*" 2 
lexer "/\\" = NamedFunction "/\\" (\(Multivector4 v1) -> Function (\(Multivector4 v2) -> Multivector4 $ v1 /\ v2))
lexer "dot" = NamedFunction "dot" (\(Multivector4 v1) -> Function (\(Multivector4 v2) -> Multivector4 $ dot v1 v2))
lexer "crash" = undefined 
lexer "id" = NamedFunction "id" (\x -> x) 
lexer "if" = NamedFunction "if" (\cond -> 
    let (Boolean c) = booleanConstructor cond in
    Function (\true -> Function (\false -> if c then true else false))) 
lexer "Scalar" = NamedFunction "Scalar" (\(Number x) -> Multivector4 (Scalar x))
lexer "Vector" = NamedFunction "Vector" (\(Number x) -> Function (\(Number y) -> Function (\(Number z) -> Function (\(Number w) -> Multivector4 (Vector [x,y,z,w])))))
lexer "Bivector" = NamedFunction "Bivector" (\(Number a) -> Function (\(Number b) -> Function (\(Number c) -> Function (\(Number d) -> Function (\(Number e) -> Function (\(Number f) -> Multivector4 (Bivector [a,b,c,d,e,f])))))))
lexer "Antivector" = NamedFunction "Antivector" (\(Number x) -> Function (\(Number y) -> Function (\(Number z) -> Function (\(Number w) -> Multivector4 (Antivector [x,y,z,w])))))
lexer "Antiscalar" = NamedFunction "Antiscalar" (\(Number x) -> Multivector4 (Antiscalar x))
lexer str
    | tokenNumber str = Number (read str) 
    | otherwise = Unknown str
-}

lexer :: String -> Token
lexer "crash" = undefined
lexer "True" = Boolean True
lexer "False" = Boolean False
lexer "Number" = symbolize (\[a] -> numberConstructor a) "Number" 0 
lexer "Boolean" = symbolize (\[a] -> booleanConstructor a) "Boolean" 0 
lexer "Scalar" = symbolize (\[a] -> do
    (Number x) <- numberConstructor a
    Right $ Multivector4 (Scalar x)) "Scalar" 1
lexer "Vector" = symbolize (\[a,b,c,d] -> do
    (Number x) <- numberConstructor a
    (Number y) <- numberConstructor b
    (Number z) <- numberConstructor c
    (Number w) <- numberConstructor d
    Right $ Multivector4 (Vector [x,y,z,w])) "Vector" 4
lexer "Bivector" = symbolize (\[a,b,c,d,e,f] -> do
    (Number x) <- numberConstructor a
    (Number y) <- numberConstructor b
    (Number z) <- numberConstructor c
    (Number w) <- numberConstructor d
    (Number v) <- numberConstructor e
    (Number u) <- numberConstructor f
    Right $ Multivector4 (Bivector [x,y,z,w,v,u])) "Bivector" 6
lexer "Antivector" = symbolize (\[a,b,c,d] -> do
    (Number x) <- numberConstructor a
    (Number y) <- numberConstructor b
    (Number z) <- numberConstructor c
    (Number w) <- numberConstructor d
    Right $ Multivector4 (Antivector [x,y,z,w])) "Antivector" 4
lexer "Antiscalar" = symbolize (\[a] -> do
    (Number x) <- numberConstructor a
    Right $ Multivector4 (Antiscalar x)) "Antiscalar" 1
lexer "if" = symbolize (\[cond,true,false] -> booleanConstructor cond >>= (\(Boolean b) -> if b then evaluate true else evaluate false)) "if" 3 
lexer "wrap" = symbolize (\[x,_] -> evaluate x) "wrap" 2
lexer "latex" = NamedFunction "latex" (\expr -> Right $ Str (show expr))
lexer "left" = NamedFunction "left" (\left -> Right $ Function (\_ -> evaluate left))
lexer "+" = symbolize (\[a,b] -> do
    x <- evaluate a
    y <- evaluate b
    Right $ x + y) "+" 2
lexer "-" = symbolize (\[a,b] -> do
    x <- evaluate a
    y <- evaluate b
    Right $ x - y) "-" 2
lexer "*" = symbolize (\[a,b] -> do
    x <- evaluate a
    y <- evaluate b
    Right $ x * y) "*" 2
lexer str
    | tokenNumber str = Number (read str)
    | otherwise = Unknown str

symbolize' :: ([Expression Token] -> Either Error Token) -> [Expression Token] -> Int -> Either Error Token
symbolize' f xs 0 = f xs
symbolize' f xs arity = Right $ Function (\x -> symbolize' f (xs ++ [x]) (pred arity))

symbolize :: ([Expression Token] -> Either Error Token) -> String -> Int -> Token 
symbolize f sym arity = NamedFunction sym (\x -> symbolize' f [x] (pred arity))

tokenize :: Expression Primitive -> Expression Token 
tokenize (Value (Primitive p)) = Value (lexer p)
tokenize (Expression e1 e2) = Expression (tokenize e1) (tokenize e2) 

apply :: Token -> Expression Token -> Either Error Token
apply (NamedFunction _ f) x = f x
apply (Function f) x = f x 

appendStacktrace :: Error -> String -> Error
appendStacktrace (Error msg stack) line = Error msg (stack ++ [line])

appendStacktraceIfError :: Either Error Token -> String -> Either Error Token
appendStacktraceIfError (Left err) line = Left $ appendStacktrace err line 
appendStacktraceIfError token _ = token

evaluate :: Expression Token -> Either Error Token 
evaluate (Value token) = Right token
evaluate full@(Expression (Value sym@(Function _)) expr) = appendStacktraceIfError (apply sym expr) (show full)
evaluate full@(Expression (Value sym@(NamedFunction _ _)) expr) = appendStacktraceIfError (apply sym expr) (show full)
evaluate full@(Expression (Value token) _) = appendStacktraceIfError (Left (Error ("Not an operator: " ++ show token) [])) (show full)
evaluate expr@(Expression e1 e2) = appendStacktraceIfError (case evaluate e1 of
            (Left err) -> Left (appendStacktrace err (show e1)) 
            (Right val) -> evaluate $ Expression (Value val) e2) (show expr)
    
    --case res of
        --(Left err) -> Left (appendStacktrace err (show expr))
        --(Right token) -> (Right token)
    

eval :: String -> Either Error Token
eval = evaluate . tokenize . parse

main :: IO ()
main = interact process 

showEither :: (Show a, Show b) => Either a b -> String
showEither (Left v) = show v
showEither (Right v) = show v

process :: String -> String
process str = unlines [ showEither $ eval line | line <- lines str ]
