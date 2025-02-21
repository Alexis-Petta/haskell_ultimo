import Text.Show.Functions ()

-- Punto 1 

data Chofer = UnChofer {
    chofer :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condiciones :: [Viaje -> Bool]
} deriving (Show)

data Viaje = UnViaje {
    fecha :: Int,
    cliente :: (String, String),
    costo :: Int
} deriving (Show, Eq)

nombre :: Cliente -> String
nombre = fst
direccion :: Cliente -> String
direccion = snd

type Cliente = (String, String)
type Condicion = Viaje -> Bool

-- Punto 2

cualquierViaje :: Condicion
cualquierViaje viaje = True

viajePorPrecio :: Condicion
viajePorPrecio   = (200 > ). costo

viajePorNombre :: Int -> Condicion
viajePorNombre letras  = (letras < ).length.nombre.cliente

viajeNoZona :: String -> Condicion
viajeNoZona zonaNoQuierePasar = (zonaNoQuierePasar /= ).direccion.cliente

-- Punto 3 
--el cliente “Lucas” que vive en Victoria
--el chofer “Daniel”, su auto tiene 23.500 kms., hizo un viaje con el cliente Lucas el 20/04/2017 cuyo costo fue $ 150, y toma los viajes donde el cliente no viva en “Olivos”.
--la chofer “Alejandra”, su auto tiene 180.000 kms, no hizo viajes y toma cualquier viaje.

lucas :: Cliente
lucas = ("Lucas", "Victoria")

daniel :: Chofer
daniel = UnChofer {
    chofer = "Daniel",
    kilometraje = 23500,
    viajes = [UnViaje{cliente = lucas, fecha = 20170420, costo = 150}],
    condiciones = [viajeNoZona "Olivos"]
    }

alejandra :: Chofer
alejandra = UnChofer{
    chofer = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condiciones = [cualquierViaje]
}

-- Punto 4

puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje viaje = all ($ viaje).condiciones

-- Punto 5 

liquidacion :: Chofer -> Int
liquidacion = sum.map costo.viajes

-- Punto 6

{-
Realizar un viaje: 
A- dado un viaje y una lista de choferes, se pide que filtre los choferes que toman ese viaje. 
Si ningún chofer está interesado, no se preocupen: el viaje no se puede realizar.

B- considerar el chofer que menos viaje tenga. Si hay más de un chofer elegir cualquiera.

C- efectuar el viaje: esto debe incorporar el viaje a la lista de viajes del chofer. ¿Cómo logra representar este cambio de estado?
-}

realizarUnViaje :: Viaje -> [Chofer] -> Chofer
realizarUnViaje viaje = agregarViaje viaje.choferConMenosViajes.filter (puedeTomarViaje viaje)

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (a:b:choferes)
 |(length.viajes) a < (length.viajes) b = choferConMenosViajes (a:choferes)
 |otherwise = choferConMenosViajes (b:choferes)

agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje viaje chofer = chofer {viajes = viajes chofer ++ [viaje]}

viajeOlivos :: Viaje
viajeOlivos =   UnViaje {  
    fecha = 0,
    cliente = ("Pedro", "Olivos"),
    costo = 100 
    }

{- Al infinito y más allá
Modelar al chofer “Nito Infy”, 
su auto tiene 70.000 kms., que el 11/03/2017 hizo infinitos viajes de $ 50 con Lucas y toma cualquier viaje donde el cliente tenga al menos 3 letras. 
-}

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

nitoInfy :: Chofer
nitoInfy = UnChofer{chofer = "Nito Infy", kilometraje = 70000, viajes = repetirViaje viajeInfinito, condiciones = [viajePorNombre 3]}

viajeInfinito :: Viaje
viajeInfinito = UnViaje {fecha=20170311, cliente=lucas, costo=50}

-- Punto 8
gongNeng ::(Ord c) => c->(c->Bool)->(a->c)->[a]->c
gongNeng arg1 arg2 arg3  = max arg1 . head . filter arg2 . map arg3 