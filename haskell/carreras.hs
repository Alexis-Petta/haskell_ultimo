data Auto = Auto{
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving(Show, Eq)

type Carrera = [Auto]

--Punto 1

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = abs (distanciaEntre auto1 auto2) < 10 && color auto1 /= color auto2

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto otrosAutos = (not.any (estaCerca auto)) otrosAutos && vaGanando auto otrosAutos

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto  = (1 ==).puesto auto

puesto :: Auto -> Carrera -> Int
puesto auto = length.filter (not.leVaGanando auto)

distanciaEntre :: Auto -> Auto -> Int
distanciaEntre auto1 auto2 = distancia auto1 - distancia auto2

leVaGanando :: Auto -> Auto ->Bool
leVaGanando autoGanador autoPerdedor = distanciaEntre autoGanador autoPerdedor > 0 && autoGanador/=autoPerdedor


--Punto 2 

correr :: Int ->  Auto -> Auto
correr  tiempo auto= auto {distancia = distancia auto + tiempo * velocidad auto}

type Modificador = Int -> Int

alterarVelocidad :: Modificador -> Auto -> Auto
alterarVelocidad modificadorVelocidad auto = auto{velocidad = modificadorVelocidad.velocidad $ auto}

bajarLaVelocidad ::  Int -> Auto -> Auto
bajarLaVelocidad velocidadAReducir = alterarVelocidad  (max 0 . subtract velocidadAReducir)


--Punto 3

type PowerUp = Auto -> Carrera -> Carrera
terremoto :: PowerUp
terremoto auto = afectarALosQueCumplen (estaCerca auto) (bajarLaVelocidad 50)

miguelitos :: Int -> PowerUp
miguelitos velocidadConfigurable auto = afectarALosQueCumplen (leVaGanando auto) (bajarLaVelocidad velocidadConfigurable)

jetPack :: Int->PowerUp
jetPack tiempo auto = afectarALosQueCumplen (auto ==) (alterarVelocidad (`div` 2).correr tiempo.alterarVelocidad (*2))

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista


--Punto 4

type Color = String
type Evento = Carrera->Carrera

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)] 
simularCarrera estadoInicial eventos = posiciones(foldl (\carreraActual evento -> evento carreraActual) estadoInicial eventos)

posiciones :: Carrera -> [(Int, Color)] 
posiciones carrera = map (obtenerPosicion carrera) carrera

obtenerPosicion :: Carrera -> Auto -> (Int, Color)
obtenerPosicion carrera auto  = (puesto auto carrera, color auto)

correnTodos :: Int -> Evento
correnTodos tiempo = map (correr tiempo)

usaPowerUp :: Color -> PowerUp -> Evento
usaPowerUp color power carrera = power (encontrarAuto color carrera) carrera

encontrarAuto :: Color -> Carrera -> Auto
encontrarAuto colorBuscado =  head.filter ((colorBuscado ==).color)

-----------------------

ejemploDeUsoSimularCarrera :: [(Int, Color)]
ejemploDeUsoSimularCarrera =
    simularCarrera autosDeEjemplo [
        correnTodos 30,
        usaPowerUp "azul" (jetPack 3),
        usaPowerUp "blanco" terremoto,
        correnTodos 40,
        usaPowerUp "blanco" (miguelitos 20),
        usaPowerUp "negro" (jetPack 6),
        correnTodos 10
    ]

autosDeEjemplo :: [Auto]
autosDeEjemplo = map (\color -> Auto color 120 0) ["rojo", "blanco", "azul", "negro"]