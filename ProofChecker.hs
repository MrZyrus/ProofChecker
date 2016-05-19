-- 05-38040 Jose Cipagauta
-- 06-39883 Nicolas Mañan
-- Lab. de Lenguajes Proyecto 1

{-# LANGUAGE FlexibleInstances #-}

module ProofChecker where

import Theorems
import Terms

data Sust = Su Term Term

(=:) :: Term -> Term -> Sust
a =: (Var b) = Su (Var b) a

infixl 5 =:

class Sustitution a where
    sust :: Term -> a -> Term

instance Sustitution Sust where
    sust (Var a) (Su (Var b) c)  
        |    a == b = c
        |    otherwise = Var a
    sust a b = sust' a b

instance Sustitution (Term, Sust, Term) where
    sust (Var a) (b, (Su (Var c) d), Var e)
        |    a == c = b
        |    a == e = d
        |    otherwise = Var a
    sust a b = sust' a b

instance Sustitution (Term, Term, Sust, Term, Term) where
    sust (Var a) (b, c, (Su (Var d) e), Var f, Var g)
        |    a == d = b
        |    a == f = c
        |    a == g = e
        |    otherwise = Var a
    sust a b = sust' a b

sust' :: Sustitution a => Term -> a -> Term
sust' (Const a) _ = Const a
sust' (Neg a) b = Neg (sust a b)
sust' (Or a b) c = Or (sust a c) (sust b c)
sust' (And a b) c = And (sust a c) (sust b c)
sust' (Then a b) c = Then (sust a c) (sust b c)
sust' (Equiv a b) c = Equiv (sust a c) (sust b c)
sust' (NEquiv a b) c = NEquiv (sust a c) (sust b c)

instance Show Sust where
    show (Su a b) = (show b) ++ " =: " ++ (show a)

instantiate :: Sustitution a => Equation -> a -> Equation
instantiate (Eq a b) s = Eq (sust a s) (sust b s)

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Eq a b) e (Var z) = Eq (sust e (Su (Var z) a)) (sust e (Su (Var z) b))

infer :: Sustitution a => Float -> a -> Term -> Term -> Equation
infer f s (Var z) e = leibniz (instantiate (prop f) s) e (Var z)

step :: Sustitution a => Term -> Float -> a -> Term -> Term -> Term
step t f s (Var z) e
    |    t == (extractFirst (infer f s (Var z) e)) = extractSecond (infer f s (Var z) e)
    |    t == (extractSecond (infer f s (Var z) e)) = extractFirst (infer f s (Var z) e)
    |    otherwise = error "invalid inference rule"

extractFirst :: Equation -> Term
extractFirst (Eq a _) = a

extractSecond :: Equation -> Term
extractSecond (Eq _ a) = a

class Statement a where
    statement :: Float -> () -> a -> () -> () -> Term -> Term -> Term -> IO Term

instance Statement Sust where
    statement f _ s _ _ (Var z) e t = do
        putStrLn ("=== <statement " ++ (show f) ++ " with (" ++ (show s) ++ ") using lambda " ++ (show (Var z)) ++ " (" ++ (show e) ++ ")>")
        putStrLn (show (step t f s (Var z) e))
        return $ step t f s (Var z) e

instance Statement (Term, Sust, Term) where
    statement f _ s _ _ (Var z) e t = do
        putStrLn ("=== <statement " ++ (show f) ++ " with " ++ (show s) ++ " using lambda " ++ (show (Var z)) ++ " (" ++ (show e) ++ ")>")
        putStrLn (show (step t f s (Var z) e))
        return $ step t f s (Var z) e

instance Statement (Term, Term, Sust, Term, Term) where
    statement f _ s _ _ (Var z) e t = do
        putStrLn ("=== <statement " ++ (show f) ++ " with " ++ (show s) ++ " using lambda " ++ (show (Var z)) ++ " (" ++ (show e) ++ ")>")
        putStrLn (show (step t f s (Var z) e))
        return $ step t f s (Var z) e

proof :: Equation -> IO Term
proof e = do
    putStrLn ("proving " ++ (show e))
    putStrLn ("\n" ++ (show (extractFirst e)))
    return $ extractFirst e

done :: Equation -> Term -> IO ()
done e t 
    | extractSecond e == t = putStrLn "proof successful"
    | otherwise = putStrLn "proof unsuccessful"

with :: ()
with = undefined

using :: ()
using = undefined

lambda :: ()
lambda = undefined