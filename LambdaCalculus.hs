-- Bernardo Pinto de Alkmim
-- 1712658
-- Exercícios em Haskell para INF2811 - Programação Funcional
-- Professor Roberto Ierusalimschy
-- λ-expressões: reduções, variáveis livres, e substituições.

module LambdaCalculus (
        Lambda(LVar, LApp, LAbs),
        Name,
        noRepeat,
        freeVar,
        newVar,
        subst
) where

-- Uma λ-expressão pode ser uma variável, uma aplicação, ou uma abstração.
data Lambda = LVar Name
        | LApp Lambda Lambda
        | LAbs Lambda Lambda
        deriving (Eq, Show)

type Name = String

-- Evita repetições em uma lista
noRepeat :: (Eq a) => [a] -> [a]
noRepeat [] = []
noRepeat (x:xs) = x:(filter (/= x) (noRepeat xs))

-- Mostra as variáveis livres de uma λ-expressão.
freeVar :: Lambda -> [Name]
freeVar (LVar x) = [x]
freeVar (LApp e1 e2) = noRepeat ((freeVar e1) ++ (freeVar e2))
freeVar (LAbs (LVar x) e) = filter (/= x) (freeVar e)

-- Gera sempre que possível uma variável da forma "x" concatenado com n "'".
-- n, no caso, é o menor número de "'" necessário para que a variável nova seja livre
-- na nova λ-expressão gerada.
-- Pior caso: complexidade O(n^2) de tempo, O(n) espaço.
-- TODO melhorar?
newVar :: [Name] -> Name
newVar [] = "x"
newVar (y:ys) = case filter (== y++"'") ys of
                        [] -> y++"'"
                        _ -> newVar ((y++"'"):ys)

-- Importante casar os valores das variáveis!
subst :: Lambda -> Name -> Lambda -> Lambda
subst (LVar y) x e = case x of
                        _       | x == y -> e
                                | otherwise -> LVar y
subst (LApp e' e'') x e = LApp (subst e' x e) (subst e'' x e)
subst (LAbs (LVar y) e') x e = case x of
                        _       | x == y -> LAbs (LVar x) e'
                                | otherwise -> case (filter (== y) (freeVar e)) of
                                        [] -> LAbs (LVar y) (subst e' x e)
                                        _ -> LAbs (LVar z) (subst (subst e' y (LVar z)) x e)
                                                where z = newVar (x:((freeVar e) ++ (freeVar e')))

-- Testes
main :: IO ()
main = do
        -- (x)[x → (z z)] ≡ z z
        print (subst (LVar "x") "x" (LApp (LVar "z") (LVar "z")))
        print (freeVar (subst (LVar "x") "x" (LApp (LVar "z") (LVar "z"))))
        -- (y)[x → (z z)] ≡ y
        print (subst (LVar "y") "x" (LApp (LVar "z") (LVar "z")))
        print (freeVar (subst (LVar "y") "x" (LApp (LVar "z") (LVar "z"))))

        -- ((x x) (y x))[x → (z z)] ≡ ((z z) (z z)) (y (z z))
        print (subst (LApp (LApp (LVar "x") (LVar "x")) (LApp (LVar "y") (LVar "x"))) "x" (LApp (LVar "z") (LVar "z")))
        print (freeVar (subst (LApp (LApp (LVar "x") (LVar "x")) (LApp (LVar "y") (LVar "x"))) "x" (LApp (LVar "z") (LVar "z"))))

        -- (λx.(z z))[x → (λy.w)] ≡ λx.(z z)
        print (subst (LAbs (LVar "x") (LApp (LVar "z") (LVar "z"))) "x" (LAbs (LVar "y") (LVar "w")))
        print (freeVar (subst (LAbs (LVar "x") (LApp (LVar "z") (LVar "z"))) "x" (LAbs (LVar "y") (LVar "w"))))
        -- (λx'.(z z))[x → (λy.w)] ≡ λx.(z z)
        print (subst (LAbs (LVar "x'") (LApp (LVar "z") (LVar "z"))) "x" (LAbs (LVar "y") (LVar "w")))
        print (freeVar (subst (LAbs (LVar "x'") (LApp (LVar "z") (LVar "z"))) "x" (LAbs (LVar "y") (LVar "w"))))
        -- (λw.(z z))[x → (λy.w)] ≡ λx'.(z z)
        print (subst (LAbs (LVar "w") (LApp (LVar "z") (LVar "z"))) "x" (LAbs (LVar "y") (LVar "w")))
        print (freeVar (subst (LAbs (LVar "w") (LApp (LVar "z") (LVar "z"))) "x" (LAbs (LVar "y") (LVar "w"))))
        -- (λx'.(z z))[x → (λy.x')] ≡ λx''.(z z)
        print (subst (LAbs (LVar "x'") (LApp (LVar "z") (LVar "z"))) "x" (LAbs (LVar "y") (LVar "x'")))
        print (freeVar (subst (LAbs (LVar "x'") (LApp (LVar "z") (LVar "z"))) "x" (LAbs (LVar "y") (LVar "x'"))))
        -- (λy.(z z))[y → (λy.x')] ≡ λy.(z z)
        print (subst (LAbs (LVar "y") (LApp (LVar "z") (LVar "z"))) "y" (LAbs (LVar "y") (LVar "x'")))
        print (freeVar (subst (LAbs (LVar "y") (LApp (LVar "z") (LVar "z"))) "y" (LAbs (LVar "y") (LVar "x'"))))
        -- (λx.(z z))[z → (λy.w)] ≡ λx.((λy.w) (λy.w))
        print (subst (LAbs (LVar "x") (LApp (LVar "z") (LVar "z"))) "z" (LAbs (LVar "y") (LVar "w")))
        print (freeVar (subst (LAbs (LVar "x") (LApp (LVar "z") (LVar "z"))) "z" (LAbs (LVar "y") (LVar "w"))))