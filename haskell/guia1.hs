{-

1- Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3, p.ej: 
Main> esMultiploDeTres 9 
True 

-}
esMultiploDeTres :: Int -> Bool
esMultiploDeTres numero = mod numero 3 == 0

{-

2- Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero, p.ej. 
Main> esMultiploDe 3 12
True
-}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe numero1 numero2 = mod numero1 numero2 == 0

{-
3- Definir la función cubo/1, devuelve el cubo de un número.
-}

cubo :: Int -> Int
cubo numero = numero*numero*numero

cubo_2 :: Int -> Int
cubo_2 numero = potenciar numero 3

potenciar :: Int -> Int -> Int
potenciar numero potencia
 |potencia == 0 = 1
 |potencia == 1 = numero
 |potencia > 1 = numero * potenciar numero (potencia-1)

{-
4- Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.
-}

area :: Float -> Float -> Float
area base altura = base * altura

{-
5- Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) 
Nota: Resolverlo reutilizando la función esMultiploDe/2
-}

esBisiesto :: Int -> Bool
esBisiesto ano = mod ano 400 == 0 || (mod ano 4 == 0 && mod ano 100 /= 0)

{-
6- Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.
-}

celsiusToFahr :: Float -> Float
celsiusToFahr temperatura = temperatura*9/5+32

{-
7-Definir la función fahrToCelsius/1, la inversa de la anterior.
-}

fahrToCelsius :: Float -> Float
fahrToCelsius temperatura = (temperatura-32)*5/9

{-
8- Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius. 
-}

haceFrio :: Float -> String -> Bool
haceFrio temperatura medicion
 |medicion == "Fahrenheit" = temperatura < 8
 |medicion == "celsius" = celsiusToFahr temperatura < 8

{-
9- Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula. 
m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)} 
Más información. 
Nota: Se puede utilizar gcd.
-}

mcm :: Int -> Int -> Int
mcm numero1 numero2 = div (numero1*numero2) (gcd numero1 numero2)

{-
10- Dispersión
Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días consecutivos; cada medición es un entero que representa una cantidad de cm. 
P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm. 
A partir de estos tres números, podemos obtener algunas conclusiones. 
Definir estas funciones: 

dispersion, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las versiones de dos elementos. 
De esa forma se puede definir dispersión sin escribir ninguna guarda (las guardas están en max y min, que estamos usando). 

diasParejos, diasLocos y diasNormales reciben los valores de los tres días. Se dice que son días parejos si la dispersión es chica, que son días locos si la dispersión es grande, 
y que son días normales si no son ni parejos ni locos. Una dispersión se considera chica si es de menos de 30 cm, y grande si es de más de un metro. 
Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas. 
-}
dispersion :: Int -> Int -> Int -> Int
dispersion valor1 valor2 valor3 = max_2 valor1 valor2 valor3 - min_2 valor1 valor2 valor3

max_2 :: Int -> Int -> Int -> Int
max_2 valor1 valor2 valor3 = max (max valor1 valor2) (max valor2 valor3)

min_2 :: Int -> Int -> Int -> Int
min_2 valor1 valor2 valor3 = min (min valor1 valor2) (min valor2 valor3)

diasParejos :: Int -> Int -> Int -> Bool
diasParejos valor1 valor2 valor3 = dispersion valor1 valor2 valor3 < 30

diasLocos :: Int -> Int -> Int -> Bool
diasLocos valor1 valor2 valor3 = dispersion valor1 valor2 valor3 > 100

diasNormales :: Int -> Int -> Int -> Bool
diasNormales valor1 valor2 valor3 = not (diasParejos valor1 valor2 valor3 || diasLocos valor1 valor2 valor3)
{-
11- En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. El peso de un pino se puede calcular a partir de la altura así: 3 kg x cm hasta 3 metros, 
2 kg x cm arriba de los 3 metros. P.ej. 2 metros ⇒  600 kg, 5 metros ⇒  1300 kg. 
Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un pino fuera de este rango no le sirve a la fábrica. Para esta situación: 
Definir la función pesoPino, recibe la altura de un pino y devuelve su peso. 
Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. 
Definir la función sirvePino, recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. Usar composición en la definición. 
-}



{-
12- Este ejercicio alguna vez se planteó como un Desafío Café con Leche: Implementar la función esCuadradoPerfecto/1, sin hacer operaciones con punto flotante. 
Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. Pensar que el primer cuadrado perfecto es 0, 
para llegar al 2do (1) sumo 1, para llegar al 3ro (4) sumo 3, para llegar al siguiente (9) sumo 5, después sumo 7, 9, 11 etc.. También algo de recursividad van a tener que usar. -}

suma :: Float -> Float
suma = (2*)

resta :: Float -> Float
resta x = x-2

operacion :: Float -> [Float]
operacion = (\x->x : [1,2,3]).suma.(\x->x-2)