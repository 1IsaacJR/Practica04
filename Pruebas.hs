--Ejercicio 2

--Lo utilizamos para aramar una herramienta logica mas potente
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

--Definimos un modelo de animales de tal manera que:
data Animal = Tigre | Leon | Cocodrilo deriving (Show, Eq)

-- Definimos tipos de constantes respecto a el modelo elegido
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

--Definir un tipo de asignacion
type Asignacion a = Variable -> a

--Definimos una asignacion deel modelo "Animal" de manera constante
asignacion :: a -> Animal
asignacion v = Leon 


--Definicion de un tipo de universo
type Universo a = [a]

--Utilizamos el Tipo de dato de logica de primer orden definido anteriormente
--Solo que cambiamos el nombre del tipo de dato para mejor especificacion de "Pred" a "Formula"
data Formula = Atomo Nombre [Variable]
  	   | Conjuncion [Formula]
  	   | Disyuncion [Formula]
	   | Implicacion Formula Formula
	   | Negacion Formula
  	   | ParaTodo Variable Formula
  	   | Existe Variable Formula
   deriving (Eq,Ord)  

--Sustitucion de un tipo de variable para una interpretacion
sustitucion :: Asignacion a -> Variable -> a -> Asignacion a
sustitucion s x d v | v == x = d
	      	  | otherwise = s v

--Definimos el tipo de interpretacion
type Interpretacion a = String -> [a] -> Bool

--Funcion que calcula el valor de una formula en un universo cualquiera, con una interpretacion i y una  asignacion s
valor :: Eq a => Universo a -> Interpretacion a -> Asignacion a -> Formula -> Bool
valor _ i s (Atomo r vs) = i r (map s vs)
valor u i s (Conjuncion fs) = all (valor u i s) fs
valor u i s (Disyuncion fs) = any (valor u i s) fs
valor u i s (Implicacion f1 f2) = valor u i s f1 <= valor u i s f2
valor u i s (Negacion f) = not (valor u i s f)
-- La función or toma la lista de valores booleanos generados por 'map' y devuelve True si al menos uno es True.
valor u i s (ParaTodo v f) = and [valor u i (sustitucion s v d) f
      	    	      	     | d <- u]
valor u i s (Existe v f) = or [valor u i (sustitucion s v d) f
      	    	      	      | d <- u]

--Ejemplo con una formula dado nuestro modelo
--Definimos unos tipos de varaibles
x, y, z :: Variable
x = Variable "x" []
y = Variable "y" []
z = Variable "z" []
u = Variable "u" []

--Definimos una formula
formula1 :: Formula
formula1 = ParaTodo x (Disyuncion [Atomo "P" [x],Atomo "Q" [x]])

--Definimos una segunda formula de prueba
formula2 :: Formula
formula2 = ParaTodo x (Atomo "P" [x])

--Definimos una de las interpretaciones
interpretacion1 :: String -> [Animal] -> Bool
interpretacion1 "P" [x] = x == leon 
interpretacion1 "Q" [x] = x == cocodrilo 
interpretacion1 _ _ = True

--Definimos una de las segundas interpretaciones que me arroja False
interpretacion2 :: String -> [Animal] -> Bool
interpretacion2 "P" [x] = x == leon 
interpretacion2 "Q" [x] = x == tigre 
interpretacion2 _ _ = False

--Definimos una de las terceras interpretaciones que me pueda arrojar True
interpretacion3 :: String -> [Animal] -> Bool
interpretacion3 "P" [x] = x == Leon
interpretacion3 _ _ = False


--Definimos un dominio 
dominio :: [Animal]
dominio = [Tigre, Leon, Cocodrilo]

-- Main de ejecucion
main :: IO ()
main = do
  print (valor dominio interpretacion1 asignacion formula1)
  print (valor dominio interpretacion2 asignacion formula1)
  print (valor dominio interpretacion3 asignacion formula2)

