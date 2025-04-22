--Ejercicio 2

--Lo utilizamos para aramar una herramienta logica mas potente
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

{-
Necesitamos un universo tal que:
al definir una función determine el valor de una fórmula. Dicha función la
denotamos por (valorF u (iR,iF) a f), en la que u denota el universo, iR es la in-
terpretación de los símbolos de relación, iF es la interpretación de los símbolos de
función, a la asignación y f la fórmula.



-}

--Definimos un modelo de animales de tal manera que:
data Animal = Tigre | Leon | Cocodrilo deriving Show
tigre, leon, cocodrilo :: Animal
tigre = Tigre
leon = Leon
cocodrilo = Cocodrilo


--Se define el nombre de la variable
type Nombre = String
-- Se define el indice de dicha variable
type Indice = [Int]
--Se define el tipo de dato de la variable
data Variable = Variable Nombre Indice
     deriving (Eq, Ord, Generic, Show)

--Definicion de un tipo de universo
type Universo a = [a]

--Definir un tipo de asignacion
type Asignacion a = Variable -> a

implementacion :: a -> Animal
implementacion v = Leon 

--Sustitucion de un tipo de variable para una interpretacion
sustituye :: Asignacion a -> Variable -> a -> Asignacion a
sustituye s x d v | v == x = d
	      	  | otherwise = s v

--Definimos el tipo de interpretacion
type InterpretacionR a = String -> [a] -> Bool

--Funcion que calcula el valor de una formula en un universo cualquiera, con una interpretacion i y una  asignacion s
valor :: Eq a => Universo a -> InterpretacionR a -> Asignacion a -> Pred -> Bool
valor _ i s (Atom r vs) = i r (map s vs)
valor u i s (No f) = not (valor u i s f)
valor u i s (Imp f1 f2) = valor u i s f1 <= valor u i s f2
valor u i s (Conj f1 f2) = valor f1 && valor f2
valor u i s (Disy f1 f2) = valor f1 || valor f2   
valor u i s (Forallx v f) = and [valor u i (sustituye s v d) f
      	    	      	     	 | d <- u]
valor u i s (Existx v f) = or [valor u i (sustituye s v d) f
      	    	      	      | d <- u]
{-
Creo que al final esta ya no se ocupa

--Funcion para separar formulas a atomicas y asignarles (verdadero o falso dada la interpretacion) )un valor
asignacion:: Pred -> Bool
asignacion (Atom nombre exps) = nombre (map interpretacion exps)
asignacion (No p) =  not (asignacion p)
asignacion (Disy p q) = asignacion p || asignacion q   
asignacion (Conj p q) = asignacion p && asignacion q
asignacion (Imp p q) = not (asignacion p) || asignacion q 
asignacion (Forallx _ p) = asignacion p
asignacion (Existx _ p) = asignacion p
-}

--Tipo de dato de logica de primer orden
data Pred = Atom Nombre [Exp]
          | Disy Pred Pred
          | Conj Pred Pred
	  | Imp Pred Pred
	  | No Pred
          | Forallx Variable Pred
          | Existx Variable Pred
  deriving (Eq, Show)
  
--Funcion que evalua un argumento atomico
data Exp = Lit String -- Tomando un aliteral
     	 | Var String -- Tomando una variable
	 | Fun [Exp]  -- Tomando una funcion de var o lit
  deriving (Eq, Show)

--Funcion que devuleve un valor segun el tipo de variable

interpretacion :: Exp -> Bool
interpretacion (Lit "p") = True
interpretacion (Lit "q") = False
interpretacion (Var _) = False -- variables no asignadas por ahora
interpretacion (Fun _) = False -- funciones no evaluadas por ahora

