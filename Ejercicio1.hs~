--Ejercicio 1

{-
-- Que se quiere hacer
-- estructura de los predicados (como árboles de expresiones),
--o su significado funcional (evaluación con funciones de verdad).?


Logica de primer orden
--Objetos
--Relaciones
--Propiedades
--Funciones
--φ ::= P(t1, . . . , tn) | ¬(φ) | (φ ∧ φ) | (φ ∨ φ) | (φ → φ) | (∀x φ) | (∃x φ)
-- Puede ser variable, constante o funcion
-- Declaramos un tipo de universo
data Universo = cualquiercosa

P:: Universo -> Bool
P x 
  | x == "_" = True
  | x == 0 = True
  | otherwise = False

-}

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
  

-- Ejemplo de términos
juan :: Exp
juan = Lit "Juan"

x :: Exp
x = Var "x"

y :: Exp
y = Var "y"

-- Predicados atómicos usando Fun
amigoDe :: Exp -> Exp -> Pred
amigoDe a b = Atom (Fun [Lit "amigoDe", a, b])

feliz :: Exp -> Pred
feliz a = Atom (Fun [Lit "feliz", a])

--Ejemplo que implica todos los predicados anteriores

ejemploPred :: Pred
ejemploPred = Existx (
                 Conj
                   (amigoDe x juan)
                   (Forallx (
                      Imp
                        (amigoDe y x)
                        (feliz y)
                   ))
               )

-- Función principal
main :: IO ()
main = do
    print ejemploPred  
    


