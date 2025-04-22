-- Ejercicio 4

module Ejercicio4 (
  Usuario(..),
  Permisos(..),
) where


--Funcion que evalua un tipo de usuario
data Usuario = Usr String
  deriving (Eq, Show)
  
--Tipo de dato que cumple con las reglas
data Permisos = EsUsuario Usuario
     	      | TieneLlave Usuario
	      | EsAdministrador Usuario
	      | AccesoPermitido Usuario
  deriving (Eq, Show)

-- Función que evalúa si un usuario tiene acceso basado en el permiso
evaluarPermiso :: Permisos -> Bool
evaluarPermiso (EsAdministrador _)   = True
evaluarPermiso (TieneLlave _)        = True
evaluarPermiso (AccesoPermitido _)   = True
evaluarPermiso (EsUsuario _)         = False

--Main de ejecucion
main :: IO ()
main = do
    let usuario1 = Usr "Juan"
    let permiso1 = EsUsuario usuario1
    let permiso2 = TieneLlave usuario1
    let permiso3 = EsAdministrador usuario1
    let permiso4 = AccesoPermitido usuario1

    putStrLn $ "¿EsUsuario tiene acceso? " ++ show (evaluarPermiso permiso1)
    putStrLn $ "¿TieneLlave tiene acceso? " ++ show (evaluarPermiso permiso2)
    putStrLn $ "¿EsAdministrador tiene acceso? " ++ show (evaluarPermiso permiso3)
    putStrLn $ "¿AccesoPermitido tiene acceso? " ++ show (evaluarPermiso permiso4)
