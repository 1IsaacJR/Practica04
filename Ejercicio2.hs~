--Ejercicio 2
{-
Necesitamos un universo tal que:
al definir una función determine el valor de una fórmula. Dicha función la
denotamos por (valorF u (iR,iF) a f), en la que u denota el universo, iR es la in-
terpretación de los símbolos de relación, iF es la interpretación de los símbolos de
función, a la asignación y f la fórmula.
-}

-- Declaramos un tipo de universo
data Universo = cualquiercosa

P:: Universo -> Bool
P x 
  | x == "_" = True
  | x == 0 = True
  | otherwise = False

{-
Pasar funcion con los valores de las variables, constantes o funciones
Pero como??
-}

--Funcion para recibir una interpretacion
recibirInterpretacion:: [Pred, Bool] -> Bool
--asignar de cada par ordenado de literales, el valor booleano para la formula ya descompuesta
recibirInterpretacion [xs:s] = asignacion s 


--Funcion para verificar si una interpretacion satisface una formula
intepretacion:: Pred -> Bool
intepretacion pred
	      | asignacion pred == True = True
	      | otherwise = False

--Funcion para separar formulas a atomicas y asignarles (verdadero o falso dada la interpretacion) )un valor
asignacion:: Pred -> Bool
asignacion (Atom v) = _ 
asignacion (No p) =  _
asignacion (p Disy q) = asignacion p ++ asignacion q   
asignacion (p Conj q) = asignacion p ++ asignacion q
asignacion (p Imp q) = asignacion p ++ asignacion q
asignacion (Forallx p) = asignacion p
asignacion (Existx p) = asignacion p

--Tipo de dato de logica de primer orden
data Pred = Atom (Exp) -- Probar con Atom Universo (Universo -> Bool)
          | Disy Pred Pred
          | Conj Pred Pred
	  | Imp Pred Pred
	  | No Pred
          | Forallx Pred
          | Existx Pred
  deriving (Eq, Show)
  
--Funcion que evalua un argumento atomico
data Exp = Lit String -- Tomando un aliteral
     	 | Var String -- Tomando una variable
	 | Fun [Exp]  -- Tomando una funcion de var o lit
  deriving (Eq, Show)

