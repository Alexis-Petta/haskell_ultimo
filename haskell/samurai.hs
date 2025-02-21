import Text.Show.Functions

data Personaje = UnPersonaje {
nombre :: String,
salud :: Float,
elementos :: [Elemento],
anioPresente :: Int
}deriving (Show)
data Elemento = UnElemento {
tipo :: String,
ataque :: Personaje-> Personaje,
defensa :: Personaje-> Personaje
}deriving (Show)

-- Punto 1 
mandarAlAnio :: Personaje -> Int -> Personaje
mandarAlAnio personaje anio = personaje{anioPresente = anio}

meditar :: Personaje -> Personaje
meditar personaje = personaje{salud = ((salud personaje +).(/2).salud) personaje}

causarDanio ::  Float -> Personaje -> Personaje
causarDanio danio personaje= personaje {salud = (max 0.(\x-> x - danio).salud) personaje}
-- Personaje de prueba
ale :: Personaje
ale = UnPersonaje {nombre = "ale", salud = 100, elementos = [elementoMalvado],anioPresente = 2025}

mari :: Personaje
mari = UnPersonaje {nombre = "mari", salud = 100, elementos = [elementoMalvado, elementoMalvado, elementoMalvado],anioPresente = 2025}

elementoMalvado :: Elemento
elementoMalvado = UnElemento {tipo ="maldad", ataque = causarDanio 50, defensa = meditar}

-- Punto 2
esMalvado :: Personaje -> Bool
esMalvado  = any esElementoMalvado.elementos 

esElementoMalvado :: Elemento -> Bool
esElementoMalvado = ("maldad" ==).tipo

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = ((salud personaje -).(salud.ataque elemento)) personaje 

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje  = filter (loPuedeMatar personaje) 

loPuedeMatar :: Personaje -> Personaje -> Bool
loPuedeMatar personaje  = estaMuerto . foldl (flip causarDanio) personaje . danoListaDeElementos personaje 

danoListaDeElementos :: Personaje -> Personaje -> [Float]
danoListaDeElementos personaje enemigo = map (danioQueProduce personaje) (elementos enemigo)

estaMuerto :: Personaje -> Bool
estaMuerto = (0==).salud

-- Punto 3

noHacerNada = id

concentracion :: Int -> Elemento 
concentracion xVeces = UnElemento {tipo = "magia", ataque = noHacerNada, defensa = repetir xVeces meditar}

repetir :: Int -> (a -> a) -> a -> a
repetir cantidad accion personaje
 |cantidad>0 = repetir (cantidad-1) accion . accion $ personaje
 |otherwise = personaje

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad UnElemento {tipo = "maldad", ataque = causarDanio 1, defensa = noHacerNada}
--replicate cantidad x genera una lista con cantidad copias de x.

jack :: Personaje
jack = UnPersonaje{
nombre= "Jack",
salud = 300, 
elementos = [concentracion 3, katanaMagica],
anioPresente = 200
}

katanaMagica = UnElemento {tipo = "magia", ataque = causarDanio 1000, defensa = noHacerNada}