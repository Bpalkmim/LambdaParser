-- Bernardo Pinto de Alkmim
-- 1712658
-- Exercícios em Haskell para INF2811 - Programação Funcional
-- Professor Roberto Ierusalimschy
-- Parser de λ-expressões em estilo PEG.

-- TODO bugs conhecidos: múltiplos comentários seguidos não são aceitos

import Data.Char
import LambdaCalculus
import Control.Monad    (liftM)

data Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) = f

instance Functor Parser where
        fmap = liftM

instance Applicative Parser where
        pure x = Parser (\s-> [(x, s)])
        Parser f <*> Parser x = do
                g <- Parser f
                y <- Parser x
                pure (g y)

instance Monad Parser where
        return = pure
        x >>= f = Parser aux where
                aux s = concatMap aux' (parse x s)
                aux' (v, r) = parse (f v) r

-- Tipo para a AST
data AST = Identifier Lambda
        | Abstraction (AST, AST)
        | Application [AST]
        | Varlist [AST]
        | Root AST
        | Error String -- Para mensagens de erro

instance Show AST where
        show a = show' a 0

item :: Parser Char
item = Parser temp where
        temp [] = []
        temp (x:xs) = [(x, xs)]

-- Falha ao parsear
pfail :: Parser a
pfail = Parser (\_-> [])

-- Negative lookahead
--pnot :: Parser a -> Parser ()
--pnot x = Parser (\s-> case parse x s of
--        [] -> [((), s)]
--        _ -> [])

-- Or não determinístico
--por :: Parser a -> Parser a -> Parser a
--por (Parser f) (Parser g) = Parser (\s-> f s ++ g s)

-- Or determinístico
por1 :: Parser a -> Parser a -> Parser a
por1 (Parser f) (Parser g) = Parser (\s-> case f s of
        [] -> g s
        x -> x)

-- Verifica se o caracter satisfaz a condição
sat :: (Char -> Bool) -> Parser Char
sat p = do
        c <- item
        if p c then
                return c
        else
                pfail

-- Fecho de Kleene ('*' de gramáticas)
many :: Parser a -> Parser [a]
many p = many1 p `por1` return []

-- '+' de gramáticas
many1 :: Parser a -> Parser [a]
many1 p = do
        x <- p
        xs <- many p
        return (x:xs)

-- Pelo menos 2 ocorrências (para aplicação)
many2 :: Parser a -> Parser [a]
many2 p = do
        x <- p
        xs <- many1 p
        return (x:xs)

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do
        char x
        string xs

comment :: Parser String
comment = do
        string "--"
        many (sat (/= '\n'))
        many space

space :: Parser Char
space = do
        c <- sat isSpace
        return c

spaces :: Parser String
spaces = do
        comment `por1` many space

-- Ignora espaços
token :: Parser a -> Parser a
token p = do
        x <- p
        spaces
        return x

symbol :: String -> Parser ()
symbol = token . string

digit :: Parser Char
digit = do
        c <- sat isDigit
        return c

lower :: Parser Char
lower = do
        c <- sat isLower
        return c

upper :: Parser Char
upper = do
        c <- sat isUpper
        return c

alpha :: Parser Char
alpha = do
        c <- lower `por1` upper
        return c

alphanum :: Parser Char
alphanum = do
        c <- alpha `por1` digit
        return c

identifier :: Parser AST
identifier = do
        c <- alpha
        cs <- many alphanum
        spaces
        return (Identifier (LVar (c:cs)))

-- Símbolos não terminais mais gerais da gramática.
parenthesized :: Parser AST
parenthesized = do
        symbol "("
        e <- expression
        symbol ")"
        return e

varlist :: Parser AST
varlist = do
        vars <- many2 identifier
        return (Varlist vars)

abstraction :: Parser AST
abstraction = do
        symbol "\\"
        var <- varlist `por1` identifier
        symbol "->"
        e <- expression
        return (Abstraction (var, e))

application :: Parser AST
application = do
        aps <- many2 simple
        return (Application aps)

simple :: Parser AST
simple = do
        e <-    parenthesized
                `por1` identifier
        return e

expression :: Parser AST
expression = do
        e <-    abstraction
                `por1` application
                `por1` simple
        spaces
        return e

root :: Parser AST
root = do
        spaces
        p <- expression
        return (Root p)

-- Função que roda o parser.
runParser :: String -> AST
runParser s =
        case s of
                []      -> Error "Entrada vazia."
                _       -> case parse root s of
                        [(x, [])]       -> x
                        [(_, (_:_))]    -> Error "Nao aceitou a entrada."
                        _               -> Error "Erro no parser."

-- O inteiro passado é a quantidade de indentações para a impressão das camadas da AST.
show' :: AST -> Int -> String
show' (Root node) n = showTabs n ++ "[ Root ]\n" ++ showTabs n ++ "{\n" ++ show' node (n + 1) ++ showTabs n ++ "}\n"
show' (Abstraction (v, e)) n = showTabs n ++ "[ Abstraction ]\n" ++ showTabs n ++ "{\n" ++ show' v (n + 1) ++ show' e (n + 1) ++ showTabs n ++ "}\n"
show' (Identifier v) n = showTabs n ++ "[ Identifier ] { " ++ show v ++ " }\n"
show' (Varlist vs) n = showTabs n ++ "[ VarList ]\n" ++ showTabs n ++ "{\n" ++ showVarList vs (n + 1) ++ showTabs n ++ "}\n"
        where   showVarList [] _ = ""
                showVarList (v:vars) m = show' v m ++ showVarList vars m
show' (Application es) n = foldl' concatish "" es (n + length es)
        where   foldl' _ z [] _ = z
                foldl' op z (x:xs) m = foldl' op (op z x (m - 1)) xs (m - 1)
                concatish "" x m = show' x m
                concatish y x m = showTabs m ++ "[ Application ]\n" ++ showTabs m ++ "{\n" ++ y ++ show' x (m + 1) ++ showTabs m ++ "}\n"
show' (Error s) _ = s ++ "\n"

showTabs :: Int -> String
showTabs 0 = ""
showTabs n = "\t" ++ showTabs (n - 1)

main :: IO ()
main = do 
        print (runParser "")
        putStrLn "\nTeste de variavel.\n"
        print (runParser "x")
        putStrLn "\nTestes com comentarios.\n"
        print (runParser "-- ola sou um cometario\nx   \n\t")
        print (runParser "-- Comentario \n          x")
        print (runParser "\n\t     x--ois ")
        print (runParser "x       --comment\n--comentario\n") -- Dá erro!!!!
        print (runParser "x     --comment\n--comentario\ny") -- Dá erro!!!!
        print (runParser "x     --comment\n z--comentario\ny") -- Dá erro!!!!
        print (runParser "--olaaaaa\nx ")
        putStrLn "Nao deve aceitar."
        print (runParser "--olaaaa\n")
        putStrLn "\nTestes com balanceamento de parenteses.\n"
        print (runParser "    ( (  (   x   )     ) )      ")
        putStrLn "Nao deve aceitar."
        print (runParser "((x)")
        putStrLn "\nTestes de abstracoes.\n"
        print (runParser "\\x->x")
        print (runParser "(\\x->\\y-> (x))")
        print (runParser "\\x->x x")
        putStrLn "\nTestes de aplicacoes.\n"
        print (runParser "x y z")
        print (runParser "(x y) z")
        print (runParser "x (y z)")
        print (runParser "((x y) z) w")
        print (runParser "(x y) (z w)")
        print (runParser "x (y (z w))")
        print (runParser "(x y) (z w x1)")
        putStrLn "\nTestes misturando abstracoes e aplicacoes.\n"
        print (runParser "x \\z-> y") -- Ver se isso aqui é permitido
        print (runParser "x (\\z-> z y)")
        print (runParser "\\z x y-> x z y")
        print (runParser "(x) (y) (z) (w)")
        print (runParser "x y z w")