type Gema = Personaje -> Personaje
type Deseo = Personaje -> Personaje

data Personaje = Personaje {
  nombre::String,
  habilidades::[String],
  planeta:: String,
  edad::Int,
  energia::Float
} deriving (Eq, Show)

type Universo = [Personaje]

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}
-- Punto 1

chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo
 |puedeUsarse guantelete = reducirUniverso universo

reducirUniverso :: Universo -> Universo
reducirUniverso universo = take (div (length universo) 2) universo

puedeUsarse :: Guantelete -> Bool
puedeUsarse guantelete = ((6==).length.gemas) guantelete && (("uru"==).material) guantelete

-- Punto 2

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = any ((<=45).edad)

energiaTotal :: Universo -> Float
energiaTotal = sum.map energia.filter (not.null.habilidades)

-- Punto 3

mente :: Float->Gema
mente = quitarEnergia

quitarEnergia :: Float -> Gema
quitarEnergia valor personaje = personaje {
  energia = energia personaje - valor
}

alma :: String -> Gema
alma habilidad personaje = quitarEnergia 10 personaje {
  habilidades = filter (/=habilidad) (habilidades personaje)
}

espacio :: String -> Gema
espacio nuevoPlaneta personaje = quitarEnergia 20 personaje {
  planeta = nuevoPlaneta
}

poder :: Gema
poder personaje = (quitarPoderes.quitarEnergia (energia personaje)) personaje

quitarPoderes :: Gema
quitarPoderes personaje
 |((2>=).length.habilidades) personaje = personaje {habilidades = []}

tiempo :: Gema
tiempo = quitarEnergia 50.quitarEdad

quitarEdad :: Gema
quitarEdad  personaje = personaje{edad = max 18.div (edad personaje) $ 2}

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

ale :: Personaje
ale = Personaje {nombre = "ale", habilidades = [], planeta = "tierra", edad = 18, energia = 50.0}


--Punto 4

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete{material ="Goma", gemas = [tiempo, alma "usar Mjolnir", gemaLoca (alma "programación el Haskell")]}

-- Punto 5

{-Punto 5: Generar la función utilizar que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. 
Indicar cómo se produce el “efecto de lado” sobre la víctima.-}

utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaDeGemas personaje = foldr ($) personaje listaDeGemas  

-- Punto 6

{-Punto 6: Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 
-}

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = undefined

comparadorDeGemas :: [Gema] -> Personaje -> Gema
comparadorDeGemas [gema1] personaje = gema1
comparadorDeGemas (gema1:gema2:gemas) personaje
 | (energia.gema1) personaje < (energia.gema2) personaje = comparadorDeGemas (gema2:gemas) personaje
 | otherwise = comparadorDeGemas (gema1:gemas) personaje