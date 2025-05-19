module Library where
import PdePreludat
import Data.Int (Int)

--El costo de estacionamiento es de 50 pesos la hora, con un mínimo de 2 horas”, esto significa que
--si estamos 1 hora, nos cobrarán por 2 horas
--si estamos 3 horas, nos cobrarán por 3 horas

costoEstacionamiento:: Number -> Number
costoEstacionamiento = (50 *).max 2 

tamanioLista :: [Number] -> Number
tamanioLista [] = 0
tamanioLista (_:xs)= 1+tamanioLista xs 

sumoLista :: [Number] -> Number
sumoLista [] = 0
sumoLista (x:xs)= x + sumoLista xs 

esMultiploDe :: Number -> Number -> Bool
esMultiploDe base multiplo = mod multiplo base == 0

esBisiesto :: Number -> Bool
esBisiesto anio= esMultiploDe 400 anio || (esMultiploDe 4 anio && not (esMultiploDe 100 anio))

pesoPino :: Number -> Number
pesoPino alturaCM
    |alturaCM <=limite = pesoBase alturaCM
    |otherwise= pesoBase limite + pesoExtra (alturaCM - limite)
    where
        limite=300
        pesoBase p = p * 3
        pesoExtra p = p * 2 
--  mientras sea menor o = a 300cm => 3 * altura
 --   una vez superado los 300 cm, => 2 * altura

esPesoUtil :: Number -> Bool
esPesoUtil pesoKg= (pesoKg >= 400) && (pesoKg <= 1000)

sirvePino :: Number -> Bool
sirvePino  = esPesoUtil . pesoPino 

dispersion :: Number -> Number -> Number -> Number
dispersion medidaDia1 medidaDia2 medidaDia3 = max medidaDia3 (max medidaDia1 medidaDia2) - min medidaDia3 (min medidaDia1 medidaDia2)

diasParejos medidaDia1 medidaDia2 medidaDia3 = dispersion medidaDia1 medidaDia2 medidaDia3 <= 30
diasLocos medidaDia1 medidaDia2 medidaDia3 = dispersion medidaDia1 medidaDia2 medidaDia3 >= 100

diasNormales :: Number -> Number -> Number -> Bool
diasNormales medidaDia1 medidaDia2 medidaDia3 = not(diasParejos medidaDia1 medidaDia2 medidaDia3 || diasLocos medidaDia1 medidaDia2 medidaDia3)

--esCuadradoPerfecto :: Number -> Bool
--esCuadradoPerfecto 0 = True
--esCuadradoPerfecto numero
--   |numero == 

-------------------------------------------COMPOSICION----------------------------

siguiente = (+1)

mitad = (/2)

inversa = (1/)

triple = (3*)

esNumeroPositivo = (>0)

--EJERCICIO 7
igual0 = (==0)

esMultiploDe' base= igual0 . flip mod base

esBisiesto' anio = multiploDe400 anio || (multiploDe4 anio && not (multiploDe100 anio))  
    where 
        multiploDe400 = esMultiploDe' 400
        multiploDe4 = esMultiploDe' 4
        multiploDe100 = esMultiploDe' 100

inversaRaizCuadrada = inversa.sqrt

incrementMCuadradoN = (+).(^2)

esResultadoPar n = even . (n^)

-------------------------------------------TUPLAS----------------------------

fst3 (a,_,_)=a
snd3(_,a,_)=a
trd3(_,_,a)=a

cuantaBizarra  (a,b)
    |a>b = a+b
    |b>a+10=b-a
    |otherwise=b*a

esNotaBochazo =(<6)

aprobo (a,b) = not (esNotaBochazo a && esNotaBochazo b)

promociono (a,b) = alMenos15 (a+b) && promocionoParcial a && promocionoParcial b
    where
        alMenos15= (>=15)
        promocionoParcial= (>=7)

--CONSULTA : (not.esNotaBochazo.fst) (5,8)  /// Indica si aprobo el primer parcial 

notasFinales :: (Ord a, Ord b) => ((a, b), (a, b)) -> (a, b)
notasFinales ((a,b),(c,d))= (max a c,max b d)

--CONSULTA: (not.aprobo.notasFinales) ((2,7),(6,-1)) //INDICA si recursa o no

--CONSULTA: ((/=(-1)).fst.snd) ((2,7),(6,-1)) //INDICA si recupero o no el primer parcial

recuperoDeGusto ((a,b),(c,d)) = promociono notasParciales && recuperoAlgunParcial
    where 
        notas= ((a,b),(c,d))
        notasParciales = fst notas
        notasRecus= snd notas
        recuperoAlgunParcial= fst notasRecus /=(-1) || snd notasRecus /=(-1)

-------------------------------------------LISTAS----------------------------

frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]


sumarListas []=0
sumarListas (elemento : elementos)= elemento + sumarListas elementos

promedioFrecuenciaCardiaca :: [Number] -> Number
promedioFrecuenciaCardiaca frecuencias= sum frecuencias/length frecuencias

calcularMinuto= (/10)

frecuenciaCardiacaMinuto = (frecuenciaCardiaca !! ) . calcularMinuto

frecuenciasHastaMomento = flip take frecuenciaCardiaca . calcularMinuto


esCapicua listas =  listaCompleta == reverse listaCompleta
    where listaCompleta = concat listas

