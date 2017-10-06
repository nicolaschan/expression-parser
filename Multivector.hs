module Multivector where

import Utils (replace)

data Multivector4 a = 
    Scalar a | Vector [a] | Bivector [a] | Antivector [a] | Antiscalar a
data Matrix a = Matrix [[a]]

showTupleAsVector :: String -> String
showTupleAsVector string = replace [('[','<'),(']','>')] string

instance Show a => Show (Multivector4 a) where
    show (Scalar s) = "Scalar " ++ show s
    show (Vector v) = "Vector " ++ (showTupleAsVector $ show v)
    show (Bivector b) = "Bivector " ++ (showTupleAsVector $ show b)
    show (Antivector av) = "Antivector " ++ (showTupleAsVector $ show av)
    show (Antiscalar as) = "Antiscalar " ++ show as

dot :: Num a => Multivector4 a -> Multivector4 a -> Multivector4 a
dot (Vector v1) (Vector v2) = Scalar . sum $ zipWith (*) v1 v2

(/\) :: Num a => Multivector4 a -> Multivector4 a -> Multivector4 a
(Scalar s1) /\ (Scalar s2) = Scalar (s1 * s2)
(Scalar s) /\ (Vector v) = Vector $ map (*s) v
(Vector v) /\ (Scalar s) = Vector $ map (*s) v
--(Vector v1) /\ (Vector v2) = Bivector $ 
