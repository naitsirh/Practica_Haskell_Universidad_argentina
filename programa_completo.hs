import Data.Char
import Data.List

--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  1. VALORES Y FUNCIONES  *  *  *  *  *  *  *  *  *  *
--  ===================================================================================

doble :: Num a => a -> a
doble x = x * 2

triple x = x * 3

mitad :: Fractional a => a -> a
mitad x = x / 2

siguiente :: Num a => a -> a
siguiente x = x + 1

anterior :: Num a => a -> a
anterior x = x - 1

masDos = siguiente.siguiente

dobleDelSiguiente = doble.siguiente  --se lee como "dobleDelSiguiente es doble compuesto con siguiente"

tripleDelAnterior = triple.anterior  --tripleDelAnterior es triple compuesto con anterior

cuantoMidenJuntos str1 str2 = length str1 + length str2


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  2. PRACTICA VALORES Y FUNCIONES  *  *  *  *  *  *  *  *
--  ===================================================================================

inversa x = 1 / x

esNumeroPositivo x = x > 0

comienzaConA :: [Char] -> Bool
comienzaConA str = head str == 'a'

eesMultiploDe x y = x / y == 0  --no funciona, no sé por qué

esMultiploDe x y = rem x y == 0

esMultiploDeTres x = esMultiploDe x 3

esMasLargoQue x str = x < length str

esPar :: Int -> Bool
esPar x = rem x 2 == 0

porcentaje :: Float -> Float -> Float
porcentaje cantidad total = cantidad * 100 / total

esParElMayor x y = esPar (max x y)

esSaludo str = (str == "hola") || (str == "chau")

laInicialEstaIncluida str1 str2 = elem (head str1) str2

esBisiesto x = (esMultiploDe x 400) || ((esMultiploDe x 4) && not (esMultiploDe x 100))

deCelsiusAFarenheit gradosCelsius = gradosCelsius * 1.8 + 32

deFarenheitACelsius gradosFarenheit = (gradosFarenheit - 32) / 1.8

haceFrioCelsius temp = temp < 8

haceFrioFarenheit = haceFrioCelsius.deFarenheitACelsius

maximoEntreTres x y z = max z (max x y)

minimoEntreTres x y z = min z (min x y)

dispersion x y z = (maximoEntreTres x y z) - (minimoEntreTres x y z)

diasParejos x y z = (dispersion x y z) < 30

diasLocos x y z = (dispersion x y z) > 100

diasNormales x y z = not (diasParejos x y z) && not (diasLocos x y z)

pesoPino mts = (min mts 3) * 300 + ((max mts 3) - 3) * 200

esPesoUtil kg = kg > 400 && kg < 1000

sirvePino = esPesoUtil.pesoPino


--  ===================================================================================
--  *  *  *  *  *  *  *  *  3. INTRODUCCION A LOS TIPOS DE DATOS *  *  *  *  *  *  *  *
--  ===================================================================================

elDobleEsMultiploDeTres = esMultiploDeTres.doble

largoEsPar :: Foldable t => t a -> Bool
largoEsPar str = even (length str)

cantidadDeLetras :: String -> Int
cantidadDeLetras unaPalabra = length unaPalabra

tieneLongitud :: String -> Int -> Bool
tieneLongitud palabra longitud = length palabra == longitud

nombreCompleto :: String -> String -> String -> String
nombreCompleto nom1 nom2 apell = nom1 ++ " " ++ nom2 ++ " " ++ apell

funcionMisteriosa1 :: Num a => a -> a
funcionMisteriosa1 x = x * x + x

funcionMisteriosa2 :: Show a => a -> Int
funcionMisteriosa2 str = length (show str)

funcionLoca :: (Ord a, Show b) => a -> a -> b -> Bool
funcionLoca x y z = x > y || show z == "hola"

estaEntre :: Ord a => a -> a -> a -> Bool
estaEntre valor menor mayor = valor >= menor && valor <= mayor

--sonIgualesOEstaEntre :: (Ord a1, Eq a2) => a2 -> a2 -> a1 -> a1 -> a1 -> Bool  <<---el que me da la consola
sonIgualesOEstaEntre :: (Eq a, Ord b) => a -> a -> b -> b -> b -> Bool
sonIgualesOEstaEntre unValor otroValor valorEntre menor mayor =
    unValor == otroValor || estaEntre valorEntre menor mayor

--ignoraElPrimero :: p1 -> p2 -> p2 <<---el que me da la consola
ignoraElPrimero :: a -> b -> b
ignoraElPrimero primero segundo = segundo

sumarTres :: Num a => a -> a -> a -> a
sumarTres uno otro otroMas = uno + otro + otroMas

compararSi :: Eq a => Bool -> a -> a -> Bool
compararSi condicion uno otro = not condicion || uno == otro


--  ===================================================================================
--  *  *  *  *  *  *  *  *  4. PRACTICA  DE INFERENCIA DE TIPOS  *  *  *  *  *  *  *  *
--  ===================================================================================

esParO :: Integral a => a -> Bool -> Bool
esParO numero condicion = even numero || condicion

sumarDos :: Num a => a -> a
sumarDos numero = numero + 2

multiplicar :: Num a => a -> a -> a
multiplicar numero otroNumero = numero * otroNumero

fueraDeRango :: Ord a => a -> a -> a -> Bool
fueraDeRango num1 num2 num3 = num1 < num2 || num1 > num3

largoDelShow :: Show a => a -> Int
largoDelShow algo = length (show algo)

maximoEntreTres' :: Ord a => a -> a -> a -> a
maximoEntreTres' uno otro otroMas = max (max uno otro) otroMas

minimoEntreTres' :: Ord a => a -> a -> a -> a
minimoEntreTres' uno otro otroMas = min (min uno otro) otroMas

soloElPrimero :: p1 -> p2 -> p1
soloElPrimero primero segundo  = primero


--  ===================================================================================
--  *  *  *  *  *  *  *  *  5. APLICACION PARCIAL Y ORDEN SUPERIOR  *  *  *  *  *  *  *
--  ===================================================================================

siguienteAP :: Num a => a -> a
siguienteAP = (1 +)

minimo6 :: Int -> Int  --no sé si está correctamente tipada
minimo6 = max 6

tripleAP :: Float -> Float  --por ahí anda para la m...
tripleAP = (3 *)

--dobleDelSiguienteAP :: Integer -> Integer  <<---otra versión
dobleDelSiguienteAP :: Float -> Float
dobleDelSiguienteAP = (*2).(+1)

saludar :: [Char] -> [Char]  --version consola, yo lo haría con "String"
saludar str = "Hola " ++ str

saludar' :: String -> String -> String
saludar' prefijo nombre = "Hola " ++ prefijo ++ " " ++ nombre

sinTitulo nombre = nombre
sr nombre = "Sr. " ++ nombre
sra nombre = "Sra. " ++ nombre
reyQuick nombre = nombre ++ " Rey de los Minisupers"
--lo de abajo en un solo paquete con lo de arriba (todo relacionado)
saludar'' :: (String -> String) -> String -> String
saludar'' titulador nombre = "Hola " ++ titulador nombre
--seguimos...
--saludoDoble :: (t -> [Char]) -> t -> t -> [Char]  <<---según la consola
saludoDoble :: (String -> String) -> String -> String -> String  --según yo
saludoDoble titulador uno otro = "Hola " ++ titulador uno ++ " y " ++ titulador otro

esMenorSegun x y funcion = (funcion x) < (funcion y)

sumaDeDoblesSegun x y funcion = 2 * funcion x + 2 * funcion y


--  ===================================================================================
--  *  *  *  *  *  * 6. PRACTICA  APLICACION PARCIAL Y ORDEN SUPERIOR  *  *  *  *  *  *  *
--  ===================================================================================

sumarNumeroAlTriple x y = ((+y).(*3)) x

sumaEsPar :: Int -> Int -> Bool
sumaEsPar x y = (even.(+y)) x 

-- 120, 3 = 80 || 100, 4 = 50 || 100, 2 = 50
--cuantoPagaCadaUno precio comensales = 
--((techo ((comensales * 3) / 8)) * precio) / comensales  <<---primera hecha, y correcta
--(techo (((*3).(/8)) comensales) * precio) / comensales  <<---segunda correcta
--(((*precio).techo) (((*3).(/8)) comensales)) / comensales  <<---tercera correcta
--((/comensales).((*precio).techo)) (((*3).(/8)) comensales)  <<---cuarta correcta

algunoCumple func x y z = func x || func y || func z

mejor func1 func2 numero = max (func1 numero) (func2 numero)

--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 

martinFierro = ("Martin Fierro", 1872)
fundacion = ("Fundacion", 1951)

tituloo (tituloDelLibro, anioDePublicacion) = tituloDelLibro
esUnClasico (tituloDelLibro, anioDePublicacion) = anioDePublicacion < 1959
esTituloLargo (tituloDelLibro, anioDePublicacion) = length tituloDelLibro > 15

aplicarPar func x y = (func x, func y)

tuplaDeFunciones func1 func2 numero = (func1 numero, func2 numero)

darVuelta :: (a -> b -> c) -> b -> a -> c
darVuelta func x y = func y x

darVuelta' :: (a -> b -> c) -> a -> b -> c
darVuelta' func x y = x `func` y

componer :: (b -> c) -> (a -> b) -> a -> c
componer func1 func2 x = func1 (func2 x)

componer' :: (b -> c) -> (a -> b) -> p -> a -> c
componer' func1 func2 x = func1.func2


--  ===================================================================================
--  *  *  *  *  *  *  *  *  7. ALTERNATIAS: GUARDAS Y PATRONES  *  *  *  *  *  *  *  *
--  ===================================================================================

valorAbsoluto x | x >= 0 = 0
                | x < 0 = -x

minimoEntre x y | x < y = x
                | x >= y = y

cantidadDePochoclosParaMinutosDeCine minutos
    | minutos < 40 = 2
    | minutos > 200 = 10
    | otherwise = minutos / 20

--horasDuerme persona
--    | persona estudiaIngenieria = 4
--    | persona programa = 6

esVocalCerrada :: Char -> Bool
esVocalCerrada 'o' = True
esVocalCerrada 'u' = True
esVocalCerrada 'a' = False
esVocalCerrada 'e' = False
esVocalCerrada 'i' = False
esVocalCerrada _ = False

esCero :: (Eq a, Num a) => a -> Bool
esCero 0 = True
esCero _ = False

puntosParaSetenta :: (Eq p, Fractional p) => p -> p
puntosParaSetenta 1 = 5.5
puntosParaSetenta 10 = 0.5
puntosParaSetenta 11 = 0.5
puntosParaSetenta 12 = 0.5
puntosParaSetenta x = x

sumaDeParOrdenado :: Num a => (a, a) -> a
sumaDeParOrdenado parOrdenado = fst parOrdenado + snd parOrdenado

distanciaAlOrigen :: Floating a => (a, a) -> a  --no lo entiendo
distanciaAlOrigen parOrdenado = sqrt (fst parOrdenado ^ 2 + snd parOrdenado ^ 2)

poderSoldado :: Num a1 => (a2, a1, a1) -> a1
poderSoldado (_, x, y) = x * y

soldadoLeGanaA :: (Ord a, Num a) => (a1, a, a) -> (a3, a, a) -> Bool  --ni, idea
soldadoLeGanaA ganador perdedor = poderSoldado ganador > poderSoldado perdedor


--  ===================================================================================
--  *  *  *  *  *  *  *  *  8. PRACTICA GUARDAS Y PATTERN MATCHING  *  *  *  *  *  *  *
--  ===================================================================================

fst3 :: (a,b,c) -> a
fst3 (a, _, _) = a

snd3 :: (a,b,c) -> b
snd3 (_, b, _) = b

trd3 :: (a,b,c) -> c
trd3 (_, _, c) = c

aplicar :: (t -> a, t -> b) -> t -> (a, b)
aplicar (func1, func2) entero = ((func1 entero), (func2 entero))

cuentaBizarra :: (Int, Int) -> Int
cuentaBizarra (num1, num2)
    | num1 > num2 = num1 + num2
    | (num2 - num1) > 10 = num2 - num1
    | otherwise = num1 * num2

esNotaBochazo :: (Ord a, Num a) => a -> Bool
esNotaBochazo numero = numero < 4

aaprobo :: (Ord a, Num a) => (a, a) -> Bool
aaprobo (nota1, nota2) = not (esNotaBochazo nota1) && not (esNotaBochazo nota2)

promociono :: (Ord a, Num a) => (a, a) -> Bool
promociono (nota1, nota2) = (nota1 + nota2) >= 14 && nota1 >= 6 && nota2 >= 6

notasFinales :: ((Int, Int), (Int, Int)) -> (Int, Int)
notasFinales ((parc1 , parc2), (recup1, recup2)) = ((max parc1 recup1), (max parc2 recup2))

recuperoDeGusto :: ((Int, Int), (Int, Int)) -> Bool
recuperoDeGusto ((parc1, parc2), (recup1, recup2)) =
    promociono (parc1, parc2) && (recup1 /= 0 || recup2 /= 0)

esMayorDeEdad :: (Num b, Ord b) => (a, b) -> Bool
esMayorDeEdad (nombre, edad) = edad >= 21

calcular :: (Int, Int) -> (Int, Int)
calcular (num1, num2)
    | even num1 && even num2 = ((num1 * 2), num2)
    | even num1 && odd num2 = ((num1 * 2), (num2 + 1))
    | odd num1 && even num2 = (num1, num2)
    | otherwise = (num1, (num2 + 1))


--  ===================================================================================
--  *  *  *  *  *  *  * 9. PRACTICA INFERENCIA DE TIPOS, RECARGADA  *  *  *  *  *  *  *
--  ===================================================================================

f1 :: (a -> b) -> a -> b
f1 a b = a b

f2 :: (a -> Bool) -> a -> a -> Bool
f2 x y z = x y || x z

f3 :: (a -> b) -> a -> a -> (b -> b -> c) -> c
f3 m x y z = z (m x) (m y)

f4 :: (b -> d) -> (c -> b) -> (a -> c) -> a -> d
f4 x y m = x . y . m

--f5 :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
--       a     b     c      b     a     c
f5 :: (a -> b -> c) -> b -> a -> c
f5 f x y  = f y x

f6 :: (t, t) -> (t -> a -> b) -> (a, a) -> (b, b)
f6 (x, y) f m = (f x (fst m), f y (snd m))

f7 :: (a -> Bool) -> [a] -> a
f7 f = head.filter f

f8 :: (t -> b -> c) -> (a -> b) -> t -> a -> c
f8 f x m = (f m).x

f9 :: a -> b -> a
f9 x y = x

f10 :: Eq a => (t -> a) -> (t -> a) -> t -> Bool
f10 a1 a2 a3 = a1 a3 == a2 a3

f11 :: (Eq a, Num t) => (t -> a) -> (t -> a) -> t -> Bool
f11 a1 a2 a3 = a1 (a3 + 1) == a2 (a3 + 1)

f12 :: a -> a -> (a -> Bool) -> a
f12 x y c
    | c x = y
    | otherwise = x

f13 :: (Ord a, Num a) => b -> (b -> a) -> Bool
f13 x m = ((>0).m) x

f14 :: Ord a => b -> b -> (b -> a) -> Bool
f14 x y f  =  f x > f y

f15 :: Show a => a -> [Char]
f15 x = "¡" ++ show x ++ "!"

f16 :: Num c => (a -> c) -> (a -> Bool) -> [a] -> c
f16 f g = sum.map f.filter g

fCondicional :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
fCondicional f1 f2 f3 x
    | f1 x = f2 x
    | otherwise = f3 x
    where  transformarCond f1 f2 f3 l = map (fCondicional f1 f2 f3) l

f18 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f18 = (.)(.)(.)


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  *  *   10. LISTAS  *  *  *  *  *  *  *  *  *  *  *  *
--  ===================================================================================

algunosTuits :: [a] -> [a]
algunosTuits = take 3

elPrimeroEsPar :: Integral a => [a] -> Bool  --el que me da la consola. El otro, error
elPrimeroEsPar x = even (head x)

textos :: [(a, b)] -> [b]
textos = map snd

--recortar :: [(a1, [a2])] -> [(a1, [a2])]  <<---la de la consola
recortar :: [(String, String)] -> [(String, String)]
recortarUnTuit (nom, mens) = (nom, take 15 mens)
recortar = map recortarUnTuit


tuitCorto ns = ((<10).length) (snd ns)

--cantidadTuitsCortos = length.filter tuitCorto  <<---la que quiere mumuki
cantidadTuitsCortos :: [(String,String)] -> Int  --la que quiere mumuki
--cantidadTuitsCortos :: Foldable t => [(a1, t a2)] -> Int  --por consola, no entiendo
cantidadTuitsCortos ns = (length.filter tuitCorto) ns

--resumir ns = csv (textos (recortar ns))
--resumir ns = (csv.textos.recortar) ns
--resumir ns = csv.textos.recortar  <<---la que quiere mumuki


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  *  11. PRACTICA  LISTAS  *  *  *  *  *  *  *  *  *  *
--  ===================================================================================

--sumarSegun :: Num a1 => (a2 -> a1) -> [a2] -> a1  --por consola, no entiendo
sumarSegun :: (Num b) => (a->b) -> [a] -> b
sumarSegun funcion lista = sum (map funcion lista)

alguno :: (a -> Bool) -> [a] -> Bool
alguno condicion lista = length (filter condicion lista) > 0

esCapicua :: (Eq a, Foldable t) => t [a] -> Bool
esCapicua lista = (concat lista) == reverse (concat lista)

esMultiploDeAlguno numero = any (esMultiploDe numero)

cuandoHabloMas (lista1, lista2)
--  | sum lista1 > sum lista2 = "normal"  <<---así no la leía el compilador, no sé
    | sum lista1 > sum lista2 = "normal"
    | otherwise = "reducido"

cuandoHizoMasLlamadas (lista1, lista2)
    | length lista1 > length lista2 = "normal"
    | otherwise = "reducido"

cuandoHizoLaLlamadaMasLarga (lista1, lista2)
    | maximum lista1 > maximum lista2 = "normal"
    | otherwise = "reducido"

cuandoHizoMasLlamadasBreves (lista1, lista2)
    | length (filter (<2) lista1) > length (filter (<2) lista2) = "normal"
    | otherwise = "reducido"

promedio :: Fractional a => [a] -> a
promedio xs = sum xs / fromIntegral (length xs)
average lista = realToFrac (sum lista) / realToFrac (length lista)

--EJERCICIO OCHO, NO LO ENTENDI DEL TODO
type Nota = Int
notasAprobadas :: [Nota] -> [Nota]
notasAprobadas = filter (>3)  --no entiendo por qué 'notasAprobadas' aquí sola no funciona
promediosSinAplazos :: [[Nota]] -> [Float]
promediosSinAplazos = map (average.map fromIntegral.notasAprobadas)  --pero aquí sí
--promediosSinAplazos = map (average.map fromIntegral.filter (>3))  <<---todo juntito y fácil

mejoresNotas lista = map maximum lista  --es como me lo toma ghci

aprobo :: [Int] -> Bool
--aprobo = all (>=4)  <<---no lo toma ghci
aprobo lista = all (>=4) lista  --sí lo toma ghci

quienesAprobaron :: [[Int]] -> [[Int]]
quienesAprobaron lista = filter aprobo lista

hayAlgunNegativo :: [Int] -> Bool
hayAlgunNegativo lista = any (<0) lista

cuantosCumplen :: (a -> Bool) -> [a] -> Int
cuantosCumplen condicion lista = length (filter condicion lista)

rechazar :: (a -> Bool) -> [a] -> [a]
rechazar condicion = filter (not.condicion)

contiene :: Eq a => a -> [a] -> Bool
contiene elemento lista = elem elemento lista

rotar :: [a] -> [a]
rotar lista = tail lista ++ [head lista]

iniciales :: String -> String
iniciales string = map head (filter ((>1).length) (words string))
--filter ((>1).length) ["programacion","orientada","a","objetos"]  <--así sí
--filter (length (>1)) ["programacion","orientada","a","objetos"]  <--así no, ni idea

pam :: [a -> b] -> a -> [b]
pam lista valor = map ($ valor) lista

anularIgual :: Eq a => (a, a) -> Bool
anularIgual dupla = fst dupla /= snd dupla
armarFixture :: Eq a => [a] -> [a] -> [(a, a)]
armarFixture lista1 lista2 = filter anularIgual (zipWith (,) lista1 lista2)

{-
ASI SE HACE COMENTARIO MULTILINEA
-}

esMuzza :: (String, b) -> Bool
esMuzza dupla = fst dupla == "muzza"

porcionesDeMuzza :: Num a => [(String, a)] -> a
porcionesDeMuzza lista = sum (map snd (filter esMuzza lista))

cuantasMuzzas :: (RealFrac a, Integral b) => [(String, a)] -> b
--cuantasMuzzas lista = ceiling (porcionesDeMuzza lista /8)  <<---mi solución
cuantasMuzzas lista = ceiling (((/8).porcionesDeMuzza) lista)  --la que pide, con (.)


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  12. LISTAS POR COMPRENSION  *  *  *  *  *  *  *  *  *
--  ===================================================================================

--nombreDePais = undefined
--nombresDePaises = map nombreDePais

nombreDePais = undefined
nombresDePaises paises = [nombreDePais pais | pais <- paises ]

poderDeSuperheroe = undefined
poderesDeSuperheroes superheroes = [poderDeSuperheroe superheroe | superheroe <- superheroes]

casaDePersonaje = undefined
casasDePersonajes personajes = [casaDePersonaje personaje | personaje <- personajes]

nombreDePersonaje = undefined
temporadasEnQueEstaVivo = undefined
nombresDePersonajesVivosEn temporada personajes =
    [nombreDePersonaje personaje | personaje <- personajes]

productosCartesianos :: [a] -> [b] -> [(a, b)]
productosCartesianos xs ys = [(x, y) | x <- xs, y <- ys]

sumaCartesiana :: Num a => [a] -> [a] -> [a]
sumaCartesiana xs ys = [x + y | x <- xs, y <- ys]

--mapM_ print  <<---para imprimir cada elemento de una lista por renglón

diferencia :: Eq a => [a] -> [a] -> [a]
diferencia xs ys = [x | x <- xs, not (elem x ys)]

intersectar :: Eq a => [a] -> [a] -> [a]
intersectar xs ys = [x | x <- xs, x `elem` ys]


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  13. EXPRESIONES LAMBDA  *  *  *  *  *  *  *  *  *  *
--  ===================================================================================

juntarStrings :: [a] -> [[a]] -> [a]
juntarStrings  _ [] = []
juntarStrings x xs = foldl1 (\acc a -> acc ++ x ++ a) xs

intersectar2 :: Eq a => [a] -> [a] -> [a]
intersectar2 xs ys = filter (\x -> x `elem` ys) xs

diferencia2 :: Eq a => [a] -> [a] -> [a]
diferencia2 xs ys = filter (\x -> not (x `elem` ys)) xs

diferencia3 :: Eq a => [a] -> [a] -> [a]
diferencia3 xs ys = filter (\x -> notElem x ys) xs

--diferencia4 :: Eq a => [a] -> [a] -> [a]
--diferencia4 xs ys = xs \\ ys

triplesDe :: Num a => [a] -> [a]
triplesDe = map (*3)

hayAlgunMultiploDe :: Integer -> [Integer] -> Bool
hayAlgunMultiploDe n xs = any (\x -> rem x n == 0) xs

totalKilosProductos :: Num c => [(a, c)] -> c
--totalKilosProductos :: [(a, Integer)] -> Integer
totalKilosProductos xs =  foldl (\x y -> x + snd y) 0 xs

totalKilosProductos2 xs =  foldl (+) 0 (map (\(x, y) -> y) xs)

totalKilosProductos3 =  sum.map snd

totalKilosProductos4 =  foldl (+) 0.map snd


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  14. DOMINAR EL MUNDO CON NADA  *  *  *  *  *  *  *  *  *
--  ===================================================================================

--1) hecho
sumarSegun2 :: Num b => (a -> b) -> [a] -> b
sumarSegun2 f xs = foldl (+) 0 (map f xs)

--con lambda
sumarSegun3 f = foldl (\x y -> x + f y) 0

--2) hecho, a medias
esCapicua2 :: Eq a => [[a]] -> Bool
esCapicua2 xs = (concat xs) == foldl (\acc x -> x:acc) [] (concat xs)

palindromee :: (Integral a) => a -> [Char] -> [String]
palindromee n al =  concat  $  map (pal n) al
    where
      pal :: (Integral a)=> a -> Char -> [String]
      pal n x 
          | n > 2 =  map (surround x) (palindromee (n-2) al)
          | n > 1 = [[x,x]]
          | otherwise = [[x]]
          where
            surround :: Char -> String -> String
            surround lt str = [lt] ++ str ++ [lt]

--3) hecho
esMultiploDeAlguno2 :: Int -> [Int] -> Bool
esMultiploDeAlguno2 n xs = foldl (\ acc x -> if (n `rem` x == 0) then True else acc) False xs

--4) hecho
mejoresNotas2 :: [[Int]] -> [Int]
mejoresNotas2 = foldr (\ x acc -> maximum x : acc) []

mejoresNotas3 :: [[Int]] -> [Int]
mejoresNotas3 xs = map maximum xs

--5) hecho
aprobo2 :: [Int] -> Bool
aprobo2 = foldl (\ acc x -> if x < 4 then False else acc) True

--6) hecho
pam2 f xs = foldr ((:) . f) [] xs                   --
                                                    -- las dos listas hacen lo mismo: map
map2 f list = foldr (\x acm -> f x : acm) [] list   --

pam3 :: [a -> b] -> a -> [b]
pam3 xs valor = map ($ valor) xs

pam4 :: [a -> b] -> a -> [b]
pam4 xs valor = foldr (\f sig -> f valor: sig) [] xs

pam5 :: [a -> b] -> a -> [b]
pam5 xs valor = foldr (\f -> (:) (f valor)) [] xs

--7) hecho
cabeza :: [a] -> a
cabeza = foldl1 (\x _ -> x)

cabeza2 :: [a] -> a
cabeza2 = foldr1 (\x _ -> x)

cabeza3 (x:xs) = x

--8) hecho
filtrar2 :: (a -> Bool) -> [a] -> [a]
filtrar2 pred = foldr ((++) . sel) []
    where
    sel x
        | pred x    = [x]
        | otherwise = []

filtrar3 :: (a -> Bool) -> [a] -> [a]
filtrar3 p = foldr (\x acc -> if p x then x : acc else acc) []

--9) hecho
maximoSegun2 _ [] = 0   --no es el que pide, y me tira falla cuando la llamo
maximoSegun2 f (x:xs)
    | f x > maximum (map f xs) = x
    | otherwise = maximoSegun2 f xs

auxxx f xs = maximum (map f xs)

maximoSegun3 :: (Foldable a, Ord b) => (b -> b) -> a b -> b
maximoSegun3 f xs = foldl1 (\x acc -> if f x > f acc then x else acc) xs

--10) hecho
aparearCon7  :: (a -> b -> c) -> [a] -> [b] -> [c]  --no entendí una goma, sacado de
aparearCon7 f xs ys =                               --stackoverflow
    fst $ foldr aux ([], reverse xs) ys 
        where
            aux _ (res, []) = (res, [])
            aux y (res, z:zs) = (f z y : res, zs)

aparearCon3 f xs ys = foldr (\x y -> f x y ) [] xs  --no funciona en ghci
-- = undefined

aparearCon4 :: (a -> b -> c) -> [a] -> [b] -> [c]
aparearCon4 f = aux
  where
    aux [] _ = []
    aux _ [] = []
    aux (x:xs) (y:ys) = f x y : aux xs ys

aparearCon5 f = foldr f 0

aparearCon6 f = go
    where
        go [] = []
        go (x:xs) = f x (go xs)

--11) hecho
listaConcatenada xs = concatMap (replicate 2) xs

--positivosYNegativos [] = []
--positivosYNegativos x:y:listaConcatenada = x ++ (negate y) ++ positivosYNegativos listaConcatenada

duplicarYNegar :: Int -> [Int]
duplicarYNegar x = x : negate x : []

positivosYNegativos :: [Int] -> [Int]
positivosYNegativos = concatMap duplicarYNegar

positivosYNegativos2 :: [Int] -> [Int]
positivosYNegativos2 = concatMap (\x -> x : negate x : [])


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  15. PRACTICA EVALUACION DIFERIDA  *  *  *  *  *  *  *  *
--  ===================================================================================

--1)
primerosN :: Int -> [Int]
primerosN n = take n $ iterate (+1) 1

--2)
ciclar :: [a] -> [a]
ciclar = cycle

ciclar2 :: [a] -> [a]
ciclar2 [] = error "Otra vez la lista vacía"
ciclar2 xs = aux where aux = xs ++ aux

ciclar3 xs = xs ++ ciclar3 xs

--3)
cuentoDeLaBuenaPipa :: [String]
cuentoDeLaBuenaPipa = ["Te cuento el cuento de la buena pipa?", "Sí!"] ++ 
    cycle ["Pero, te cuento el cuento de la buena pipa?", "Sí!"]

--4)
repetirPalabras :: String -> [String]
repetirPalabras xs = map (xs ++) (map show (iterate (+1) 1))

repetirPalabras2 :: String -> [String]
repetirPalabras2 xs = map (xs ++) (map show [1..])

--5)
repetirPalabras3 :: String -> [String]
repetirPalabras3 xs = zipWith (++) (repeat xs) (map show [1..])

--6) falta
--sumaContiguos (x:y:ys) = (y + x) : sumaContiguos ys
-- -> (\x -> zipWith (+) (x++[0]) (0:x)) xs
-- -> (\x -> zipWith (+) (0:x) (x++[0])) xs

acumulados :: Num a => [a] -> [a]
acumulados = scanl1 (+)

acumulados2 :: Num a => [a] -> [a]
acumulados2 xs = [ sum $ take x xs | x <- [1..length xs] ]  --burrada de ineficiente

acumulados3 :: (Num a) => [a] -> [a]
acumulados3 xs = go xs 0
    where go [] acc = []
          go (x:xs) acc = (acc+x) : go xs (acc+x)


esMultiploDeTres2 :: Int -> Bool
esMultiploDeTres2 num = head (filter (esDivisorDe num) (scanl1 (+) (cycle [3]))) == num

--7)
potenciasDeDos :: [Int]
potenciasDeDos = iterate (*2) 1

--8)
--no cumple requisito, da booleano jaja
primeroQueCumple :: (a -> Bool) -> a -> (a -> a) -> Bool
primeroQueCumple cond valor func = any cond (iterate func valor)

primeroQueCumple2 :: (a -> Bool) -> a -> (a -> a) -> a
primeroQueCumple2 cond valor func = head (filter cond (iterate func valor))

--aquí usando composición
primeroQueCumple3 cond valor func = (head . filter cond) (iterate func valor)

--9)
esDivisorDe :: Int -> Int -> Bool
esDivisorDe num1 num2 = rem num2 num1 == 0

mcm :: Int -> Int -> Int
mcm num1 num2 = head (filter (esDivisorDe num2) (iterate (+num1) num1))

mcm2 n1 n2 = head (filter (esDivisorDe n1) (iterate (+n2) n2))

mcm3 n1 n2 = head (filter (esDivisorDe n2) [n1,(n1+n1)..])


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  *  16. RECURSIVIDAD  *  *  *  *  *  *  *  *  *  *  *
--  ===================================================================================

infinito :: Int
infinito = 1 + infinito

infinitosUnos :: [Int]
infinitosUnos = [1] ++ infinitosUnos

sumatoria :: Num a => [a] -> a
sumatoria [] = 0
sumatoria lista = head lista + sumatoria (tail lista)

sumatoria' :: Num a => [a] -> a
sumatoria' lista
    | null lista = 0
    | otherwise = head lista + sumatoria' (tail lista)

sumatoria'' :: Num a => [a] -> a
sumatoria'' [] = 0
sumatoria'' (cabeza:cola) = cabeza + sumatoria'' cola

longitud' :: [a] -> Int
longitud' [] = 0
longitud' (x:xs) = 1 + longitud' xs


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  17. PRACTICA  RECURSIVIDAD  *  *  *  *  *  *  *  *  *
--  ===================================================================================

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

pertenece :: Eq a => [a] -> a -> Bool
pertenece [] _ = False
pertenece (y:ys) x = (y == x) || pertenece ys x

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion (x:xs) lista
    | x `elem` lista = x : interseccion xs lista
    | otherwise = interseccion xs lista

--interseccion (x:xs) (y:ys)  <<---raraso el resultado jaja
--    | x == y = [x] ++ interseccion xs ys
--    | x /= y = [] ++ interseccion xs ys

sumaLoca :: Int -> Int
sumaLoca x
    | even x = x + 2
    | odd x = x + 1
    | x == 19 = x * 3

transformadaLoca :: [Int] -> [Int]
transformadaLoca [] = []
transformadaLoca (x:xs)
    | x <= 19 = sumaLoca x : transformadaLoca xs
--  | x <= 19 = [sumaLoca x] ++ transformadaLoca xs  <<---otra forma
    | otherwise = [] ++ transformadaLoca xs

productoria :: Num a => [a] -> a
productoria [] = 1
productoria (x:xs) = x * productoria xs

{-
maximo :: Ord a => [a] -> a
maximo [] = error "¿Cómo vas a meter una lista vacía?"
maximo [x] = x
maximo (x:xs) = x `max` (maximo xs)
-}

maximo :: Ord a => [a] -> a
maximo [] = error "¿Cómo vas a meter una lista vacía?"
maximo [x] = x
maximo (x:xs) 
    | x > maxCola = x
    |otherwise = maxCola
    where maxCola = maximo xs

menoresA :: Ord a => a -> [a] -> [a]
menoresA _ [] = []
menoresA n (x:xs)
    | n > x = x : menoresA n xs
    | otherwise = [] ++ menoresA n xs

diferencias :: [Int] -> [Int]
diferencias [] = []
diferencias [_] = []
diferencias (x1:x2:xs) = abs (x1 - x2) : diferencias (x2:xs)  --la que mandé
--diferencias (x:y:xs) = abs (x - y) : diferencias (y:xs)  --también va
--diferencias (x:xs) = abs (x - head xs) : diferencias xs  --también va como piña
--diferencias (x:y:ys) = abs (x - y) : diferencias (x:ys)  --deja fijo <x>, itera sobre resto

sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [x] = [x]
sinRepetidos (x:xs)
    | elem x xs = sinRepetidos xs
    | otherwise = x : sinRepetidos xs

-- | lista == [] = []
-- | any (== head lista) (tail lista) = (take 1 lista) ++ repetidos (filter (/= head lista)) lista
-- | otherwise = repetidos (tail lista)

deleteDuplicate :: (Eq a) => [a] -> [a]
deleteDuplicate [] = []
deleteDuplicate (x:xs) = x : deleteDuplicate (filter (/= x) xs)

--"*"promedio xs = sum xs / fromIntegral (length xs)  <<---ya está declarada más arriba
promedios' :: Fractional a => [[a]] -> [a]
promedios' [] = []
promedios' xs = map promedio xs
--    where
        --aquí puede ir "*"

promedios :: Fractional a => [[a]] -> [a]
promedios [] = []
promedios (x:xs) = promedio x : promedios xs

{- otra forma már fiera, pero todo metido en un sólo lugar
promedios :: Fractional a => [[a]] -> [a]
promedios [] = []
promedios (x:xs) = promediar x : promedios xs
    where
        promediar xs = sum xs / fromIntegral (length xs)
-}

promIndividual :: (Fractional a, Ord a) => [a] -> a
promIndividual xs = sum (filter (>=4) xs) / fromIntegral (length (filter (>=4) xs))

promediosSinAplazos2 :: (Fractional a, Ord a) => [[a]] -> [a]
promediosSinAplazos2 [] = []
promediosSinAplazos2 (x:xs) = promIndividual x : promediosSinAplazos2 xs

mylast [] = error "Salame"
mylast (x:[]) = x
mylast (x:xs) = mylast xs

alVesre :: [a] -> [a]
alVesre [] = []
alVesre (x:xs) = alVesre xs ++ [x]

alVesre' :: [a] -> [a]
alVesre' [] = []
alVesre' xs = last xs : alVesre' (init xs)

--Siguientes dos, extraídas de:
--gist.github.com/YusukeHosonuma/13d83bee3c9dd01863b6c020336b4294

mapear :: (a -> b) -> [a] -> [b]
mapear _ [] = []
mapear funcion (x:xs) = funcion x : mapear funcion xs

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar funcion (x:xs)
    | funcion x = x : filtrar funcion xs
    | otherwise = filtrar funcion xs

aparearCon :: (a -> b -> c) -> [a] -> [b] -> [c]
aparearCon _ [] [] = []
aparearCon _ [_] [] = []
aparearCon _ [] [_] = []
aparearCon funcion (x:xs) (y:ys) = funcion x y : aparearCon funcion xs ys

aparearCon2 :: (a -> b -> c) -> [a] -> [b] -> [c]
aparearCon2 f = go
    where
        go [] _ = []
        go _ [] = []
        go (x:xs) (y:ys) = f x y : go xs ys

maximoSegun :: Ord a => (t -> a) -> [t] -> t
maximoSegun _ [] = error "Otra vez con las listas vacías??"
maximoSegun _ [x] = x
maximoSegun funcion (x:xs)
    | funcion x > maximum (map funcion xs) = x
    | otherwise = maximoSegun funcion xs

{-Solución que terminé copiando, pero era más fácil la mía
  Fallaba porque no entiendo la parte de error, y ghci no lo aclaró
maximoSegun :: Ord a => (t -> a) -> [t] -> t
maximoSegun _ [] = error "Otra vez con las listas vacías??"
maximoSegun _ [x] = x
maximoSegun funcion (x1:x2:xs)
    | funcion x1 > funcion x2 = x1
    | otherwise = maximoSegun funcion (x2:xs)
-}

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

intercalar :: a -> [a] -> [a]
intercalar _ [] = []
intercalar _ [x] = [x]
intercalar elemento (x:y:ys) = x : elemento : intercalar elemento (y:ys)



--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  *  *  18. MODELADO *  *  *  *  *  *  *  *  *  *  *  *
--  ===================================================================================

--cs.us.es/~jalonso/cursos/i1m/doc/manual-Data.Set.html

{-

TODO LO HECHO AQUI ESTA CORRECTO, PERO LO VUELVO HACER PARA PRACTICA

--1)
autoPepe :: (String, Int, Int)
autoPepe = ("ABW100",0,55)

autoMara :: (String, Int, Int)
autoMara = ("GIR982",10,65)

--2)
patente :: (a, b, c) -> a
patente (x, _, _) = x

nivelNafta :: (a, b, c) -> b
nivelNafta (_, y, _) = y

tamanioTanque :: (a, b, c) -> c
tamanioTanque (_, _, z) = z

--3)
armarTupla :: a -> b -> c -> (a, b, c)
armarTupla a b c = (,,) a b c

armarTupla2 :: (a, Int, c) -> (a, Int, c)
armarTupla2 (n1, n2, n3) = (,,) n1 (n2 + 2) n3

armarTupla3 :: (a, Int, c) -> (a, Int, c)
armarTupla3 (n1, n2, n3) = (n1, (n2 + 2), n3)

cargarTanque :: Int -> (a, Int, Int) -> (a, Int, Int)
cargarTanque cantidad (x, y, z) = (,,) x (min (y + cantidad) z) z

cargarTanque2 :: Int -> (a, Int, Int) -> (a, Int, Int)
cargarTanque2 cantidad (x, y, z) = (x, (min (y + cantidad) z), z)

--4)
-- type Auto = String  <<---no es el que quería
type Auto = (String, Int, Int)

--5)
-- prueba de consola en la plataforma

--6)
vaciarTanque :: Auto -> Auto
vaciarTanque (x, y, z) = (,,) x 0 z

vaciarTanque2 :: Auto -> Auto
vaciarTanque2 (x, y, z) = (x, 0, z)

--7)
--estaLleno :: Eq a1 => (a2, a1, a1) -> Bool  --definición de ghci
estaLleno :: Auto -> Bool
estaLleno (x, y, z) = y == z

--estaVacio :: (Eq a1, Num a1) => (a2, a1, c) -> Bool  --definición de ghci
--estaVacio :: (a2, Integer, c) -> Bool  --otra definición
estaVacio :: Auto -> Bool
estaVacio (x, y, z) = y == 0

--como pide el ejercicio, usando tamanioTanque y nivelNafta

estaLleno2 :: Auto -> Bool
estaLleno2 xs = nivelNafta xs == tamanioTanque xs

estaVacio2 :: Auto -> Bool
estaVacio2 xs = nivelNafta xs == 0

--8)
-- prueba de consola en la plataforma

--9)
data Alumno = Alumno String Float
    deriving (Show, Eq)

data Auto2 = ConstructorAuto2 String Int Int
    deriving (Show, Eq)

autoPepe2 :: Auto2
autoPepe2 = ConstructorAuto2 "ABW100" 0 55

autoMara2 :: Auto2
autoMara2 = ConstructorAuto2 "GIR982" 10 65

--10)
cargarTanque3 :: Int -> Auto2 -> Auto2
cargarTanque3 cantidad (ConstructorAuto2 x y z) =
    (ConstructorAuto2 x (min (y + cantidad) z) z)

--11)
tamanioTanque2 :: Auto2 -> Int
tamanioTanque2 (ConstructorAuto2 x y z) = z

nivelNafta2 :: Auto2 -> Int
nivelNafta2 (ConstructorAuto2 x y z) = y

estaLleno3 :: Auto2 -> Bool
estaLleno3 xs = tamanioTanque2 xs == nivelNafta2 xs

estaVacio3 :: Auto2 -> Bool
estaVacio3 xs = nivelNafta2 xs == 0

--12) falta
patente2 :: Auto2 -> String
patente2 (ConstructorAuto2 x y z) = x

cuantoLePuedeDar :: Auto -> Auto -> Int
cuantoLePuedeDar = undefined

transferir :: Auto2 -> Auto2 -> (Auto2, Auto2)
transferir = undefined

--13)
data Auto3 = RegistroAuto {patente3 :: String, nivelNafta3 :: Int, tamanioTanque3 :: Int}
    deriving (Show, Eq)

autoPepe3 :: Auto3
autoPepe3 = RegistroAuto "ABW100" 0 55

autoMara3 :: Auto3
autoMara3 = RegistroAuto {patente3 = "GIR982", nivelNafta3 = 10, tamanioTanque3 = 65}

--14)
type ServicioAutomotor = Auto3 -> Auto3

--vaciarTanque3 :: ServicioAutomotor

--15)
--enchularPatente :: String -> ServicioAutomotor
enchularPatente RegistroAuto = patente3

  *     *     *     *     *     *     *     *     *     *     *     *     *     *     *     
     *     *     *     *     *     *     *     *     *     *     *     *     *     *
  *     *     *     *     *     *     *     *     *     *     *     *     *     *     *

-}

--1) comentado para seguir la ejercitación de mumuki
-- autoPepe :: (String, Int, Int)
-- autoPepe = ("ABW100", 0, 55)

-- autoMara :: (String, Int, Int)
-- autoMara = ("GIR982", 10, 65)

--2) comentado para seguir la ejercitación de mumuki
-- patente :: (a, b, c) -> a
-- patente (x, _, _) = x

-- nivelNafta :: (a, b, c) -> b
-- nivelNafta (_, y, _) = y

-- tamanioTanque :: (a, b, c) -> c
-- tamanioTanque (_, _, z) = z

--3) comentado para seguir la ejercitación de mumuki
-- cargarTanque :: Int -> (a, Int, Int) -> (a, Int, Int)
-- cargarTanque cantidad (x, y, z) = (x, (min (y + cantidad) z), z)

--4) comentado para seguir la ejercitación de mumuki
-- type Auto = (String, Int, Int)

--5)
-- prueba de consola en la plataforma

--6) comentado para seguir la ejercitación de mumuki
-- vaciarTanque :: Auto -> Auto
-- vaciarTanque (x, y, z) = (x, 0, z)

--7) comentado para seguir la ejercitación de mumuki
-- estaLleno :: Auto -> Bool
-- estaLleno xs = nivelNafta xs == tamanioTanque xs

-- estaVacio :: Auto -> Bool
-- estaVacio xs = nivelNafta xs == 0

--8)
-- prueba de consola en la plataforma

--9) comentado para seguir la ejercitación de mumuki
-- data Alumno = Alumno String Float
--     deriving (Show, Eq)

-- data Auto = Auto String Int Int
--     deriving (Show, Eq)

-- autoPepe :: Auto
-- autoPepe = Auto "ABW100" 0 55

-- autoMara :: Auto
-- autoMara = Auto "GIR982" 10 65

--10) comentado para seguir la ejercitación de mumuki
-- cargarTanque :: Int -> Auto -> Auto
-- cargarTanque cantidad (Auto x y z) = (Auto x (min (y + cantidad) z) z)

--11)
-- patente :: Auto -> String
-- patente (Auto x _ _) = x

-- nivelNafta :: Auto -> Int
-- nivelNafta (Auto _ y _) = y

-- tamanioTanque :: Auto -> Int
-- tamanioTanque (Auto _ _ z) = z


estaLleno :: Auto -> Bool
estaLleno xs = nivelNafta xs == tamanioTanque xs

estaVacio :: Auto -> Bool
estaVacio xs = nivelNafta xs == 0

--12) falta
cuantoLePuedeDar :: Auto -> Auto -> Int
cuantoLePuedeDar (Auto a b c) (Auto x y z) = min b (z - y)

transferir :: Auto -> Auto -> (Auto, Auto)
transferir (Auto a b c) (Auto x y z) =
    ((Auto a (b - (cuantoLePuedeDar (Auto a b c) (Auto x y z))) c), 
    (Auto x (y + (cuantoLePuedeDar (Auto a b c) (Auto x y z))) z))

--13)
-- SINTAXIS DE REGISTRO

-- data Alumno = Alumno {nombre :: String, edad :: Int}
--     deriving (Show, Eq)

-- Alumno {nombre = "Juan", edad = 20}    <<---no funciona, no sé por qué
-- Alumno {edad = 20, nombre = "Juan"}

data Auto = Auto {patente :: String, nivelNafta :: Int, tamanioTanque :: Int}
    deriving (Show, Eq)

autoPepe :: Auto
autoPepe = Auto "ABW100" 0 55

autoMara :: Auto
autoMara = Auto {patente = "GIR982", nivelNafta = 10, tamanioTanque = 65}

--14)
type ServicioAutomotor = Auto -> Auto

vaciarTanque :: ServicioAutomotor
vaciarTanque (Auto x y z) = (Auto x (y - y) z)

--15)
cargarTanque :: Int -> ServicioAutomotor
cargarTanque cantidad (Auto x y z) = (Auto x (min (y + cantidad) z) z)

cargarPoco :: ServicioAutomotor
cargarPoco = cargarTanque 10

enchularPatente :: String -> ServicioAutomotor
enchularPatente deco (Auto x y z) = (Auto ((take 3 deco) ++ (drop 3 x)) y z)

--16) falta
agrandarTanque :: Int -> ServicioAutomotor
agrandarTanque cantidad (Auto x y z) = (Auto x y (z + cantidad))

hacerService :: Auto -> [ServicioAutomotor] -> Auto
hacerService auto [] = auto
hacerService auto (x:xs) = hacerService (x auto) xs

-- función para probar la recursividad
kanika num [] = num
kanika num (x:xs) = kanika (x num) xs 


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  *  19. PRACTICA CHOCOBO  *  *  *  *  *  *  *  *  *  *
--  ===================================================================================





--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  20. PdeP JN 2020 - TP FUNCIONAL  *  *  *  *  *  *  *  *
--  ===================================================================================

--1)
esImpar :: Int -> Bool
esImpar numero = mod numero 2 == 1

--2)
leGanaA :: String -> String -> Bool
leGanaA "piedra" "tijera" = True
leGanaA "tijera" "piedra" = False
leGanaA "papel" "piedra" = True
leGanaA "piedra" "papel" = False
leGanaA "tijera" "papel" = True
leGanaA "papel" "tijera" = False
leGanaA "piedra" "piedra" = False
leGanaA "papel" "papel" = False
leGanaA "tijera" "tijera" = False


--3)
estaEntre2 :: Int -> Int -> Int -> Bool
estaEntre2 x y z = z >= x && z <= y

esAurica :: String -> Bool
esAurica palabra = estaEntre2 3 7 (length palabra)

--4)
data Ficha = Ficha { izquierdo :: Int, derecho :: Int }
    deriving (Show, Eq)

crearFicha :: Int -> Int -> Ficha
crearFicha num1 num2 = Ficha { izquierdo = num1, derecho = num2 }

esLaMismaFicha :: Ficha -> Ficha -> Bool
esLaMismaFicha ficha1 ficha2 =
    ( izquierdo ficha1 == izquierdo ficha2 || izquierdo ficha1 == derecho ficha2 ) &&
    ( derecho ficha1 == derecho ficha2 || derecho ficha1 == izquierdo ficha2 )

--5)
data Celular = Celular {
  linea :: Linea,
  saldo :: Int,
  proveedor :: String }
    deriving (Show, Eq)

data Linea = Linea {
  codigoDeArea :: String,
  numeroTelefonico :: String }
    deriving (Show, Eq)

cosa1 = Linea "011" "666"
celuPepe = Celular { 
    linea = cosa1,
    saldo = 50,
    proveedor = "Movistar" }

cosa2 = Linea "011" "668"
celuPepa = Celular cosa2 30 "Personal"

cosa3 = Linea "011" "333"
celuHans = Celular cosa3 200 "Cti"

promoRecarga :: Int -> Celular -> Celular
promoRecarga monto celu
    | proveedor celu == "Personal" =
        Celular {
            linea = linea celu, 
            saldo = (saldo celu) * 2 + monto, 
            proveedor = proveedor celu }
    | proveedor celu == "Movistar" && codigoDeArea (linea celu) == "011" =
        Celular { 
            linea = linea celu, 
            saldo = (saldo celu) + monto * 3, 
            proveedor = proveedor celu }
    | otherwise = Celular { 
                    linea = linea celu, 
                    saldo = (saldo celu) + monto, 
                    proveedor = proveedor celu }

promoRecarga2 :: Int -> Celular -> Celular
promoRecarga2 monto (Celular x y z)
    | proveedor (Celular x y z) == "Personal" =
        ( Celular x ( (saldo (Celular x y z)) * 2 + monto) z ) 
    | proveedor (Celular x y z) == "Movistar" && codigoDeArea (linea (Celular x y z)) == "011" =
        ( Celular x ( (saldo (Celular x y z)) + monto * 3 ) z )
    | otherwise = ( Celular x ( (saldo (Celular x y z)) + monto) z )

--6)
f :: Integral a => a -> Bool -> a -> a -> Bool
f a b c d = b || div a d == c

f22 :: Integral a => a -> Bool -> a -> a -> Bool
f22 a b c d = b || ((== c) . (div a)) d 

g :: Show a => a -> Bool
g a = length (show a) > 3

g2 :: Show a => a -> Bool
g2 a = ((> 3) . length) (show a)    -- <<---terminó pidiendo que no ponga la firma de tipos

--7) es igual que el (5) del modulo siguiente (21.)

-- acá hecha con composición, exactamente como quiere en este caso

-- armarCafe :: Vaso -> Gramos -> Cafe
-- armarCafe tamanioVaso granosDeCafe = 
--     ((servirEnVaso tamanioVaso) . (prepararCafe 1000) . molerGranos) granosDeCafe

-- armarFrapu :: Gramos -> Cafe
-- armarFrapu granosDeCafe = 
--     ((servirEnVaso 400) . (licuar 60 120) . (agregarHielo 6) . (prepararCafe 80) . 
--     molerGranos) granosDeCafe

--8)
esMejorAplicado :: (t1 -> t2 -> t3) -> (t2 -> t1) -> t2 -> t3
esMejorAplicado (comparacion) (funcion) x =
    (comparacion) ((funcion) x) x

--9)
data Empleado = Empleado {
  rol :: String,
  edad :: Int,
  tareas :: [String] }
    deriving (Show)

lautaro = Empleado {
  rol = "contador",
  edad = 26,
  tareas = ["liquidar sueldos", "presentar ganancias", "cargar facturas"] }

victoria = Empleado {
  rol = "rrhh",
  edad = 45,
  tareas = ["entrevistar"] }

ruben = Empleado {
  rol = "legales",
  edad = 70,
  tareas = ["resolver litigios"] }

pancracio = Empleado {
  rol = "contador",
  edad = 70,
  tareas = [] }

laura = Empleado {
  rol = "legales",
  edad = 35,
  tareas = ["resolver litigios", "papeleo"] }

juan = Empleado {
  rol = "rrhh",
  edad = 25,
  tareas = [] }

rolesAtareados :: [Empleado] -> [String]
rolesAtareados xs = [rol x | x <- xs, length (tareas x) >= 2]

-- rolesAtareados xs = [rol x | x <- xs, ((>= 2) . length) (tareas x)]
--      la chanchada que pide mumuki

--10)
hayJubilados :: [Empleado] -> Bool
hayJubilados xs = any (>= 65) [edad x | x <- xs, length (tareas x) == 0]

-- hayJubilados xs = any (>= 65) [edad x | x <- xs, ((== 0) . length) (tareas x)]
--      otra vez, la chanchada que pide mumuki

--11)
data Cuenta = Cuenta {
  saldoBanco :: Int,
  saldoMaximo :: Int }
    deriving (Show, Eq)

data Transaccion = Transaccion {
  iden :: String,
  monto :: Int }
    deriving (Show, Eq)

nasa = Cuenta {
    saldoBanco = 100,
    saldoMaximo = 200 }

x21 = Transaccion {
    iden = "asd",
    monto = 50 }

x22 = Transaccion {
    iden = "sdf",
    monto = (-20) }

x23 = Transaccion {
    iden = "fdj",
    monto = 250 }

modificarSaldo :: Int -> Cuenta -> Cuenta
modificarSaldo x cuenta =
    Cuenta {
        saldoBanco = min (saldoBanco cuenta + x) (saldoMaximo cuenta),
        saldoMaximo = saldoMaximo cuenta }

montosDeLasTransacciones :: [Transaccion] -> [Int]
montosDeLasTransacciones xs = [monto x | x <- xs]

realizarTransacciones :: [Transaccion] -> Cuenta -> Cuenta
realizarTransacciones xs cuenta =
    foldr (modificarSaldo) cuenta (montosDeLasTransacciones (reverse xs))


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  21. PdeP MiT 2020 - TP FUNCIONAL  *  *  *  *  *  *  *  *
--  ===================================================================================

--1)
esMes :: Int -> Bool
esMes x = x >= 1 && x <=12

hayCambioDeEstacion :: Int -> Bool
hayCambioDeEstacion x = x <= 12 && rem x 3 == 0

estacion :: Int -> String
estacion x
    | x == 1 || x == 2 = "verano"
    | x == 3 = "verano/otoño"
    | x == 4 || x == 5 = "otoño"
    | x == 6 = "otoño/invierno"
    | x == 7 || x == 8 = "invierno"
    | x == 9 = "invierno/primavera"
    | x == 10 || x == 11 = "primavera"
    | x == 12 = "primavera/verano"

--2)
data Video = Video {
    titulo :: String,
    duracion :: (Int, Int, Int),
    hashtags :: String }
        deriving (Show, Eq)

videoMuyLargo :: Video
videoMuyLargo = Video "Senior anillado" (1,15,45) "#scifi #epic"

videoNormal :: Video
videoNormal = Video "Docu Las tortus" (0,32,12) "#natur #bio #ecologia"

videoCorto :: Video
videoCorto = Video "Trance Mix" (0,10,59) "#music #trance"

otroVideoCorto :: Video
otroVideoCorto = Video "Cortometraje" (0,10,20) "#experimentacion"

--3)
esMasLargo :: Video -> Video -> Bool
esMasLargo video1 video2 = duracion video1 > duracion video2

--4)
duracionEnMinutos :: Video -> Int
duracionEnMinutos (Video x (a, b, c) z) = a * 60 + b

porcentajeDeReproduccionPromedio :: Int -> (Int, Int, Int) -> Video -> Int
porcentajeDeReproduccionPromedio vistas (x, y, z) video =
    div (div ((x * 60 + y) * 100) vistas) (duracionEnMinutos video)

--5)
data Cafe = Cafe {
  intensidad :: Int,
  temperatura :: Int,
  ml :: Int }
    deriving (Show, Eq)

type Gramos = Int
type Agua = Int
type Vaso = Int
type Segundos = Int
type Leche = Int
type Hielos = Int

molerGranos :: Gramos -> Gramos
molerGranos granosDeCafe = granosDeCafe * 90 `div` 100

prepararCafe :: Agua -> Gramos -> Cafe
prepararCafe mlAgua grCafeMolido =
    Cafe { temperatura = 60, intensidad = max (grCafeMolido * 20 `div` mlAgua) 1, ml = mlAgua }

servirEnVaso :: Vaso -> Cafe -> Cafe
servirEnVaso tamanioVaso cafe = conCantidad (min tamanioVaso (ml cafe) - 10) cafe

agregarHielo :: Hielos -> Cafe -> Cafe
agregarHielo cantHielos = disminuirTemperatura (cantHielos * 7) . disminuirIntensidad 1

licuar :: Segundos -> Leche -> Cafe -> Cafe
licuar segundos mlLeche =
    aumentarCantidad mlLeche . disminuirTemperatura (segundos `div` 10) . 
    disminuirIntensidad (mlLeche `div` 100)

disminuirTemperatura grados cafe = conTemperatura (temperatura cafe - grados) cafe
disminuirIntensidad cant cafe = conIntensidad (max (intensidad cafe - cant) 0) cafe
aumentarCantidad cantidad cafe = conCantidad (ml cafe + cantidad) cafe

conCantidad cantidad (Cafe intensidad temperatura _) = Cafe intensidad temperatura cantidad
conTemperatura grados (Cafe intensidad _ ml) = Cafe intensidad grados ml
conIntensidad nuevaIntensidad (Cafe _ temperatura ml) = Cafe nuevaIntensidad temperatura ml

--  --------  --------

armarCafe :: Vaso -> Gramos -> Cafe
armarCafe tamanioVaso granosDeCafe =
    servirEnVaso tamanioVaso ( prepararCafe 1000 ( molerGranos granosDeCafe ) )

armarFrapu :: Gramos -> Cafe
armarFrapu granosDeCafe =
    servirEnVaso 400 (licuar 60 120 ( agregarHielo 6 ( prepararCafe 80 
    ( molerGranos granosDeCafe ) ) ) )

--6)
lowerCase :: [Char]
lowerCase = ['a'..'z']

upperCase :: [Char]
upperCase = ['A'..'Z']

convertirMayuscula :: Char -> Char
convertirMayuscula char = (fst.head.filter ((==char).snd).zip lowerCase) upperCase

todoAMinuscula :: String -> String
todoAMinuscula xs = [toLower x | x <- xs]

{-para encontrar si existe una palabra en una cadena más larga

-- primera funcion, toma la lista y luego el substring:
check::Eq a => [a]->[a]->Bool
check l s = check' l s True where
    check' _ [] h          = True
    check' [] _ h          = False
    check' (x:xs) (y:ys) h = (y == x && check' xs ys False) || (h && check' xs (y:ys) h)

-- segunda funcion, la que viene predefinida en Data.List
-- (no se si la toma mumuki):
isInfixOf "hola" "hola mundo" == True

tercera funcion, la documentacion de la anterior:
isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _         =  True
isPrefixOf _  []        =  False
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys

isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
-}

tieneHashtag :: String -> Video -> Bool
tieneHashtag hashtag video =
    isInfixOf (todoAMinuscula hashtag) (todoAMinuscula (hashtags video))

listaHashtags :: Video -> [String]
listaHashtags video = words (todoAMinuscula (hashtags video))

estanRelacionados :: Video -> Video -> Bool
estanRelacionados video1 video2 =
    length [x | x <- (listaHashtags video1), x `tieneHashtag` video2] >= 3


-- funcion que la hice solo para probar
primerHashtag :: Video -> String
primerHashtag video = (words (hashtags video)) !! 0

--7)
agregarHashtag :: String -> Video -> Video
agregarHashtag hashtag video
    | isInfixOf (todoAMinuscula hashtag) (todoAMinuscula (hashtags video)) = video
    | otherwise = Video (titulo video) (duracion video) (hashtags video ++ " " ++ hashtag)

agregarHashtags :: [String] -> Video -> Video
agregarHashtags xs video = foldr (agregarHashtag) video xs

--8) falta

--9) pide cargar algo en consola







































