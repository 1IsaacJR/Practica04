--Ejercicio 1

{-
Conceptos a evaluar
Logica de primer orden
--Objetos
--Relaciones
--Propiedades
--Funciones
--φ ::= P(t1, . . . , tn) | ¬(φ) | (φ ∧ φ) | (φ ∨ φ) | (φ → φ) | (∀x φ) | (∃x φ)
-- Puede ser variable, constante o funcion
-- Declaramos un tipo de universo
data Universo = cualquiercosa

--Se define un tipo de universo que se ocupara despues 
P:: Universo -> Bool
P x 
  | x == "_" = True
  | x == 0 = True
  | otherwise = False

-}

--Tipo de dato de logica de primer orden
data Pred = Atomo (Exp)
  	   | Conjuncion Pred Pred 
  	   | Disyuncion Pred Pred
	   | Implicacion Pred Pred
	   | Negacion Pred
  	   | ParaTodo Pred 
  	   | Existe Pred
   deriving (Eq,Show)  
  
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
amigoDe a b = Atomo (Fun [Lit "amigoDe", a, b])

feliz :: Exp -> Pred
feliz a = Atomo (Fun [Lit "feliz", a])



--Ejemplo que implica todos los predicados anteriores
ejemploPred :: Pred
ejemploPred = Existe (Conjuncion(amigoDe x juan)(ParaTodo (Implicacion(amigoDe y x)(feliz y))))

-- Main de ejecucion
main :: IO ()
main = do
    print ejemploPred  
    


