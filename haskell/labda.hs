

type Requisito = Depto -> Bool
type Busqueda = [Requisito]
type Depto = (Int, Int, Int, String)
type Persona = (String, [Busqueda])

ambientes :: (a, b, c, d) -> a
ambientes (a, _,_,_) =a
superficie :: (a, b, c, d) -> b
superficie (_,m2,_,_) = m2
precio :: (a, b, c, d) -> c
precio (_,_,p,_)= p
barrio :: (a, b, c, d) -> d
barrio (_,_,_,b) = b

--type Persona = (String, [Busqueda])

mail :: (String, [Busqueda]) -> String
mail = fst
busquedas :: (String, [Busqueda]) -> [Busqueda]
busquedas = snd

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool
between x y z = x <= z && y >= z

--deptosDeEjemplo :: [(Integer, Integer, Integer, String)]
--deptosDeEjemplo = [(3,80,7500,"Palermo"), (1,45,3500,"Villa Urquiza"), (2,50,5000,"Palermo"), (1,45,5500,"Recoleta")]

-- Punto 1

mayor :: (Ord a) => (b -> a) -> b -> b -> Bool
mayor funcion valor1 valor2 = funcion valor1 > funcion valor2

menor :: (Ord a) => (b -> a) -> b -> b -> Bool
menor funcion valor1  = (funcion valor1 <) . funcion

-- Ejemplo:
-- ghci> mayor precio (3,80,7500,"Palermo") (1,45,3500,"Villa Urquiza")     
-- True
-- ghci> menor precio (3,80,7500,"Palermo") (1,45,3500,"Villa Urquiza")
-- False

-- Punto 2

ubicadoEn :: [String] -> Depto -> Bool
ubicadoEn barrios departamento = barrio departamento `elem` barrios

cumpleRango :: (Depto -> Int) -> Int -> Int -> Depto -> Bool
cumpleRango funcion valor1 valor2  = between valor1 valor2 . funcion

-- type Busqueda = [Requisito]
-- type Requisito = Depto -> Bool
-- type Depto = (Int, Int, Int, String)


-- Punto 3
cumpleBusqueda ::  Busqueda -> Depto -> Bool
cumpleBusqueda busqueda departamento = all ($ departamento) busqueda

cumpleBusqueda' :: Depto -> Busqueda ->  Bool
cumpleBusqueda'  departamento  = all ($ departamento)

busquedaOrdenada :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
busquedaOrdenada busqueda requisito  = ordenarSegun requisito . filter (cumpleBusqueda busqueda)

{-
ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs
-}

deptosDeEjemplo :: [Depto]
deptosDeEjemplo =
  [ (3, 80, 7500, "Palermo")
  , (1, 45, 3500, "Villa Urquiza")
  , (2, 50, 5000, "Palermo")
  , (1, 45, 5500, "Recoleta")
  , (2, 60, 6500, "Belgrano")
  , (4, 120, 10000, "Recoleta")
  ]

busqueda1 :: Busqueda
busqueda1 = [ubicadoEn ["Palermo", "Recoleta"], cumpleRango precio 4000 8000]

busqueda2 :: Busqueda
busqueda2 = [cumpleRango ambientes 1 3, cumpleRango superficie 40 60]

busqueda3 :: Busqueda
busqueda3 = [ubicadoEn ["Belgrano"], cumpleRango precio 5000 7000]


ordenPorPrecioAsc :: Depto -> Depto -> Bool
ordenPorPrecioAsc = menor precio

ordenPorSuperficieDesc :: Depto -> Depto -> Bool
ordenPorSuperficieDesc = mayor superficie

-- Punto 4

mailsDePersonasInteresadas :: Depto -> [Persona] -> [String]
mailsDePersonasInteresadas departamento = map mail . filter (cumpleAlgunaBusqueda departamento . busquedas)

cumpleAlgunaBusqueda :: Depto -> [Busqueda] -> Bool
cumpleAlgunaBusqueda departamento = any (cumpleBusqueda' departamento)

-- cumpleBusqueda' :: Depto -> Busqueda ->  Bool
-- mail :: (a,b) -> a
-- mail = fst
-- busquedas :: (a,b) -> b
-- busquedas = snd

personasDeEjemplo :: [Persona]
personasDeEjemplo =
  [ ("alice@example.com", [[ubicadoEn ["Palermo", "Recoleta"], cumpleRango precio 4000 8000]])
  , ("bob@example.com", [[cumpleRango ambientes 1 3, cumpleRango superficie 40 60]])
  , ("charlie@example.com", [[ubicadoEn ["Belgrano"], cumpleRango precio 5000 7000]])
  , ("dave@example.com", [[cumpleRango precio 8000 12000, ubicadoEn ["Recoleta"]]])
  ]
