data Criatura = UnaCriatura {
    peligrosidad :: Int,
    deshacerse :: [Condicion]
}

type Condicion = Persona -> Bool

-- Punto 1

data Persona = UnaPersona {
    edad :: Int,
    items :: [String],
    experiencia :: Int
}


siempreDetras :: Criatura
siempreDetras = UnaCriatura {peligrosidad = 0, deshacerse = []}

gnomo :: Int -> Criatura
gnomo cantidadDeGnomos = UnaCriatura {peligrosidad = 2 ^ cantidadDeGnomos, deshacerse = [tieneItem "Soplador de hojas"]}

fantasma :: Int -> [Condicion] -> Criatura
fantasma poder asuntos = UnaCriatura {peligrosidad = ((20*).min 10 . max 1) poder, deshacerse = asuntos}

-- Definicion de tieneItem
tieneItem :: String -> Condicion
tieneItem item  = elem item . items

-- Punto 2

enfrentarCriatura ::  Criatura -> Persona -> Persona
enfrentarCriatura criatura persona
 | puedeDeshacer persona criatura = (sumarExperiencia persona . peligrosidad) criatura
 | otherwise = sumarExperiencia persona 1

-- Definicion de puedeDeshacer
puedeDeshacer :: Persona -> Criatura ->   Bool
puedeDeshacer persona = all ($ persona) . deshacerse

-- Definicion de sumarExperiencia
sumarExperiencia :: Persona -> Int -> Persona
sumarExperiencia persona experienciaAGanar = persona {experiencia = experienciaAGanar + experiencia persona}

-- Punto 3

{-
Determinar cuánta experiencia es capaz de ganar una persona luego de enfrentar sucesivamente a un grupo de criaturas.
-}

combateContinuo :: [Criatura] -> Persona -> Persona
combateContinuo criaturas personaje = foldl (flip enfrentarCriatura) personaje criaturas

-- Definicion de grupo de criaturas de ejemplo
grupoDeCriaturas :: [Criatura]
grupoDeCriaturas = [siempreDetras, gnomo 10, fantasma 3 [valorMaximo 13 edad, tieneItem "disfraz de oveja"], fantasma 1 [valorMinimo 10 experiencia]]

-- Definicion para valor max y min
valorMaximo :: Int -> (Persona -> Int) -> Condicion
valorMaximo max funcion = (< max).funcion

valorMinimo :: Int -> (Persona -> Int) -> Condicion
valorMinimo min funcion = (> min).funcion

-- Ejemplo de demostracion
ejemplo :: Persona -> Persona
ejemplo = combateContinuo grupoDeCriaturas


-----------------------SEGUNDA PARTE----------------------------

{-

1) Definir recursivamente la función:
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
que a partir de dos listas retorne una lista donde cada elemento:
- se corresponda con el elemento de la segunda lista, en caso de que el mismo no cumpla con la condición indicada
- en el caso contrario, debería usarse el resultado de aplicar la primer función con el par de elementos de dichas listas
Sólo debería avanzarse sobre los elementos de la primer lista cuando la condición se cumple. 
> zipWithIf (*) even [10..50] [1..7]
[1,20,3,44,5,72,7] ← porque [1, 2*10, 3, 4*11, 5, 6*12, 7]

-}

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ _ [] = []
zipWithIf transformador condicion (a:primerLista) (b:segundaLista)
 | condicion b = transformador a b : zipWithIf transformador condicion primerLista segundaLista
 | otherwise = b : zipWithIf transformador condicion (a:primerLista) segundaLista

-- Punto 2

{-
2) Notamos que la mayoría de los códigos del diario están escritos en código César, 
que es una simple sustitución de todas las letras por otras que se encuentran a la misma distancia en el abecedario. 
Por ejemplo, si para encriptar un mensaje se sustituyó la a por la x, la b por la y, la c por la z, la d por la a, la e por la b, etc.. 
Luego el texto "jrzel zrfaxal!" que fue encriptado de esa forma se desencriptaría como "mucho cuidado!".

Hacer una función abecedarioDesde :: Char -> [Char] que retorne las letras del abecedario empezando por la letra indicada. 
O sea, abecedarioDesde 'y' debería retornar 'y':'z':['a' .. 'x'].

Hacer una función desencriptarLetra :: Char -> Char -> Char que a partir una letra clave (la que reemplazaría a la a) y la letra que queremos desencriptar,
retorna la letra que se corresponde con esta última en el abecedario que empieza con la letra clave. Por ejemplo: desencriptarLetra 'x' 'b' retornaría 'e'.

Hint: se puede resolver este problema sin tener que hacer cuentas para calcular índices ;)

Definir la función cesar :: Char -> String -> String que recibe la letra clave y un texto encriptado y retorna todo el texto desencriptado, 
teniendo en cuenta que cualquier caracter del mensaje encriptado que no sea una letra (por ejemplo '!') se mantiene igual. 
Usar zipWithIf para resolver este problema.
Realizar una consulta para obtener todas las posibles desencripciones (una por cada letra del abecedario) usando cesar para el texto "jrzel zrfaxal!".
-}

abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = init  ([letra .. 'z'] ++ ['a' .. letra])

desencriptar :: Char -> Char -> Char
desencriptar letraClave = traductor (abecedarioDesde letraClave) abecedarioNormal

traductor :: [Char] -> [Char] -> Char -> Char
traductor (letraComparadora:listaEncriptada) (primerLetra:listaLetras) letraEncriptada
 |letraComparadora == letraEncriptada = primerLetra
 |otherwise = traductor listaEncriptada listaLetras letraEncriptada

abecedarioNormal :: [Char]
abecedarioNormal = ['a'..'z']

cesar :: Char -> String -> String
cesar letraClave  = zipWithIf desencriptar esLetra (repeat letraClave)

esLetra :: Char -> Bool
esLetra caracter = caracter `elem` abecedarioNormal

probarTodasLasLetras ::  String -> [String]
probarTodasLasLetras palabraADesencriptar = map (`cesar` palabraADesencriptar) abecedarioNormal

vigenere :: String -> String -> String
vigenere codigo  = zipWithIf desencriptar esLetra (cycle  codigo)
