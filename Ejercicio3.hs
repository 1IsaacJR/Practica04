--Ejercicio 3
--Recuperamos los metodos definidos anteriormente

-- Tipos base
type Variable = String
type Nombre = String
type Asignacion a = Variable -> a
type Universo a = [a]

-- Lógica de primer orden
data Formula
  = Atomo Nombre [Variable]
  | Conjuncion [Formula]
  | Disyuncion [Formula]
  | Implicacion Formula Formula
  | Negacion Formula
  | ParaTodo Variable Formula
  | Existe Variable Formula
  deriving (Eq, Ord, Show)

-- Función de sustitución
sustitucion :: Asignacion a -> Variable -> a -> Asignacion a
sustitucion s x d v
  | v == x = d
  | otherwise = s v

-- Interpretación
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

-- Verificador de consecuencia lógica
esConsecuenciaLogica :: Eq a => Universo a -> Interpretacion a -> Asignacion a -> [Formula] -> Formula -> Bool
esConsecuenciaLogica u i s premisas conclusion =
  not (valor u i s (Conjuncion [Conjuncion premisas, Negacion conclusion]))

-- Definición de Animal
data Animal = Tigre | Leon | Cocodrilo deriving (Eq, Show)

--Definimos un tipo de variable a utilizar
-- Variable usada en fórmulas
x :: Variable
x = "x"

--Definimos una formula
formula1 :: Formula
formula1 = ParaTodo x (Disyuncion [Atomo "P" [x], Atomo "Q" [x]])

--Definimos una segunda formula de prueba
formula2 :: Formula
formula2 = ParaTodo x (Atomo "P" [x])

--Definimos una de las interpretaciones
interpretacion1 :: String -> [Animal] -> Bool
interpretacion1 "P" [a] = a == Leon
interpretacion1 "Q" [a] = a == Cocodrilo
interpretacion1 _ _ = False

--Definimos una de las segundas interpretaciones que me arroja 
interpretacion2 :: String -> [Animal] -> Bool
interpretacion2 "P" [a] = a == Leon
interpretacion2 "Q" [a] = a == Tigre
interpretacion2 _ _ = False

--Definimos una de las terceras interpretaciones que me pueda arrojar 
interpretacion3 :: String -> [Animal] -> Bool
interpretacion3 "P" [a] = a == Leon
interpretacion3 _ _ = False

--Definimos un dominio 
universo :: [Animal]
universo = [Tigre, Leon, Cocodrilo]

-- Asignación inicial
asignacion :: Variable -> Animal
asignacion _ = Leon -- valor arbitrario, será reemplazado por sustituciones

-- Main de prueba
main :: IO ()
main = do
  putStrLn "Interpretación 1:"
  print (esConsecuenciaLogica universo interpretacion1 asignacion [formula1] formula2)

  putStrLn "Interpretación 2:"
  print (esConsecuenciaLogica universo interpretacion2 asignacion [formula1] formula2)

  putStrLn "Interpretación 3:"
  print (esConsecuenciaLogica universo interpretacion3 asignacion [formula1] formula2)
