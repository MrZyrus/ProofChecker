-- 05-38040 Jose Cipagauta
-- 06-39883 Nicolas Mañan
-- Lab. de Lenguajes Proyecto 1

module Terms where

data Term = Const String
        |   Var Char
        |   Neg Term
        |   Or Term Term
        |   And Term Term
        |   Then Term Term
        |   Equiv Term Term
        |   NEquiv Term Term
        deriving (Eq)

data Equation = Eq Term Term

show' :: String -> Term -> Term -> String
show' s a b = case a of
    (Var _) -> case b of
        (Var _) -> (show a) ++ s ++ (show b)
        (Const _) -> (show a) ++ s ++ (show b)
        otherwise -> (show a) ++ s ++ "(" ++ (show b) ++ ")"
    (Const _) -> case b of
        (Var _) -> (show a) ++ s ++ (show b)
        (Const _) -> (show a) ++ s ++ (show b)
        otherwise -> (show a) ++ s ++ "(" ++ (show b) ++ ")"
    otherwise -> case b of
        (Var _) -> "(" ++ (show a) ++ ")" ++ s ++ (show b)
        (Const _) -> "(" ++ (show a) ++ ")" ++ s ++ (show b)
        otherwise -> "(" ++ (show a) ++ ")" ++ s ++ "(" ++ (show b) ++ ")"

instance Show Term where
    show x = case x of
        (Const s) -> s
        (Var c) -> [c]
        (Neg t) -> case t of
            (Var _) -> "¬" ++ (show t)
            (Const _) -> "¬" ++ (show t)
            otherwise -> "¬" ++ "(" ++ (show t) ++ ")"
        (Or a b) -> show' " \\/ " a b 
        (And a b) -> show' " /\\ " a b
        (Then a b) -> show' " ==> " a b
        (Equiv a b) -> show' " <==> " a b
        (NEquiv a b) -> show' " !<==> " a b

instance Show Equation where
    show (Eq a b) = (show a) ++ " === " ++ (show b)
                
(\/) :: Term -> Term -> Term
a \/ b = Or a b

(/\) :: Term -> Term -> Term
a /\ b = And a b

(==>) :: Term -> Term -> Term
a ==> b = Then a b

(<==>) :: Term -> Term -> Term
a <==> b = Equiv a b

(!<==>) :: Term -> Term -> Term
a !<==> b = NEquiv a b

(===) :: Term -> Term -> Equation
a === b = Eq a b

neg :: Term -> Term
neg a = Neg a

infixl 8 \/, /\
infixr 7 ==>
infixl 6 <==>, !<==>
infixl 5 ===

a :: Term
a = Var 'a'

b :: Term
b = Var 'b'

c :: Term
c = Var 'c'

d :: Term
d = Var 'd'

e :: Term
e = Var 'e'

f :: Term
f = Var 'f'

g :: Term
g = Var 'g'

h :: Term
h = Var 'h'

i :: Term
i = Var 'i'

j :: Term
j = Var 'j'

k :: Term
k = Var 'k'

l :: Term
l = Var 'l'

m :: Term
m = Var 'm'

n :: Term
n = Var 'n'

o :: Term
o = Var 'o'

p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

s :: Term
s = Var 's'

t :: Term
t = Var 't'

u :: Term
u = Var 'u'

v :: Term
v = Var 'v'

w :: Term
w = Var 'w'

x :: Term
x = Var 'x'

y :: Term
y = Var 'y'

z :: Term
z = Var 'z'

true :: Term
true = Const "true"

false :: Term
false = Const "false"