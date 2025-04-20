--Ejercicio 5

{-
Regla 1. Si esUsuario(x) :& tieneLlave(x) entonces accesoPermitido(x).
Regla 2. Si esAdministrador(x), entonces tieneLlave(x).
-}

--Funcion que evalua un tipo de usuario
data Usuario = Usr String
  deriving (Eq, Show)
  
--Tipo de dato que cumple con las reglas
data Permisos = EsUsuario Usuario
     	      | TieneLlave Usuario
	      | EsAdministrador Usuario
	      | AccesoPermitido Usuario
  deriving (Eq, Show)
  
--Tipo de dato para distintos tipos de permisos 
data PermisosUsuario = PermisosUsuario
  { usuario :: Usuario
  , esUsuario :: Bool
  , tieneLlave :: Bool
  , esAdmin :: Bool
  }

--Reglas de acceso para los tipos de permisos
reglasAcceso1 :: PermisosUsuario -> Permisos
reglasAcceso1 p
  | esUsuario p && tieneLlave p = AccesoPermitido (usuario p)
  | esAdmin p = TieneLlave (usuario p)
  | otherwise = EsUsuario (usuario p)

-- Algunos usuarios de ejemplo
usuario1 = Usr "Juan"
usuario2 = Usr "Ana"
usuario3 = Usr "Carlos"

-- Permisos compuestos
perm1 = PermisosUsuario usuario1 True True False      -- usuario con llave
perm2 = PermisosUsuario usuario2 False False True     -- solo admin
perm3 = PermisosUsuario usuario3 True False False     -- solo usuario

-- Evaluaciones
main :: IO ()
main = do
  print $ reglasAcceso1 perm1  -- AccesoPermitido Juan
  print $ reglasAcceso1 perm2  -- TieneLlave Ana
  print $ reglasAcceso1 perm3  -- EsUsuario Carlos

{-

--Reglas de acceso
accesoCredenciales :: Permisos -> Bool
accesoCredenciales (EsAdministrador _) = True
accesoCredenciales (EsUsuario _)       = False
accesoCredenciales (TieneLlave _)      = False
accesoCredenciales (AccesoPermitido _) = True

concatenacionDeUsuarios:: Permisos -> Usuario -> String
concatenacionDeUsuarios n _
			| n ++ Lit 
data Persona =
     Persona { name:: String
     	     age:: Int
	     occupation:: String} deriving (Eq, Show)

main :: IO ()
main = do
  putStrLn $ "Nombre: " ++ name juan
  putStrLn $ "Edad: " ++ show (age juan)
  putStrLn $ "Ocupación: " ++ occupation juan

-}


{-

--Reglas a cumplir
(TieneLlave _) && (EsUsuario _) = (AccesoPermitido _)
(EsAdministrador _) = (TieneLlave _)


--Funcion para verificar si una formula es consecuencia logica de un conjunto de premisas
conjuntoDePremisas:: [Pred] -> Integer -> Bool
--Comprobar que se cumple para alguna interpretacion cada una de las premisas
conjuntoDePremisas:: (xs:s) n
		     | asignacion s == True = asignacion xs contador

		     where
		     contador n = n++  
-}

