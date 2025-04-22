--Ejercicio 5
import Ejercicio4


{-
Regla 1. Si esUsuario(x) :& tieneLlave(x) entonces accesoPermitido(x).
Regla 2. Si esAdministrador(x), entonces tieneLlave(x).
-}
  
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

-- Main de ejecucion
main :: IO ()
main = do
  print $ reglasAcceso1 perm1  -- AccesoPermitido Juan
  print $ reglasAcceso1 perm2  -- TieneLlave Ana
  print $ reglasAcceso1 perm3  -- EsUsuario Carlos


