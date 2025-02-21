import Text.Show.Functions

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


-- Punto 1

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {velocidad = 10, precision = precisionJugador habilidad * 2, altura = 0}

madera :: Palo
madera habilidad = UnTiro {velocidad = 100, precision = precisionJugador habilidad `div` 2, altura = 5}

hierro :: Int -> Palo
hierro n habilidad = UnTiro {velocidad = fuerzaJugador habilidad * n, precision = precisionJugador habilidad `div` n, altura = max 0 n-3 }

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1 .. 10]

-- Punto 2

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

{-Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. 
En principio necesitamos representar los siguientes obstáculos:

Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro. 
Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.

Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. 
Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.

Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.
-}


-- Punto 3


data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
  }

tunel :: Obstaculo
tunel = UnObstaculo puedeSuperarTunel efectoTunel

laguna :: Int -> Obstaculo
laguna largo = UnObstaculo puedeSuperarLaguna (efectoLaguna largo)

hoyo :: Obstaculo
hoyo = UnObstaculo puedeSuperarHoyo efectoHoyo

puedeSuperarTunel :: Tiro -> Bool
puedeSuperarTunel tiro = precision tiro > 90 && altura tiro == 0

efectoTunel :: Tiro -> Tiro
efectoTunel tiro = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}

puedeSuperarLaguna :: Tiro -> Bool
puedeSuperarLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largoLaguna tiro = UnTiro {velocidad = velocidad tiro, precision = precision tiro, altura = altura tiro `div` largoLaguna}

puedeSuperarHoyo :: Tiro -> Bool
puedeSuperarHoyo tiro = between  5 20 (velocidad tiro) && precision tiro > 95 && altura tiro == 0

efectoHoyo :: Tiro -> Tiro
efectoHoyo tiro = UnTiro {velocidad = 0, precision = 0, altura = 0}

intentaSuperarObstaculo :: Tiro -> Obstaculo -> Tiro
intentaSuperarObstaculo tiro  obstaculo
 |puedeSuperar obstaculo tiro = efectoLuegoDeSuperar obstaculo tiro
 |otherwise = tiro {velocidad = 0, precision = 0, altura = 0}

{-
Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.

Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo, el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.
BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.

Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.
-}

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

obstaculosSuperados :: Tiro -> [Obstaculo] -> Int
obstaculosSuperados _ [] = 0
obstaculosSuperados tiro (obstaculoActual:obstaculos)
 | puedeSuperar obstaculoActual tiro = 1 + obstaculosSuperados (intentaSuperarObstaculo tiro obstaculoActual) obstaculos
 | otherwise = 0

tiroDePrueba :: Tiro
tiroDePrueba = UnTiro{velocidad = 10, precision = 95, altura = 0}

--maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (flip obstaculosSuperados obstaculos.golpe jugador) palos


{-
-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b
-}

-- Punto 5 

{-
Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, 
se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.
-}

jugadorDeTorneo = fst
puntosGanados = snd

padresPerdedores :: [(Jugador, Puntos)]->[String]
padresPerdedores jugadores = (map (padre.jugadorDeTorneo). filter (not.gano jugadores)) jugadores

gano :: [(Jugador, Puntos)] -> (Jugador,Puntos) -> Bool
gano jugadores jugador = (all ((puntosGanados jugador >). puntosGanados) . filter (/= jugador)) jugadores
