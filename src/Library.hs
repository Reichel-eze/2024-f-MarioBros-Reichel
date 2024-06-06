module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Plomero = UnPlomero {
    nombre :: String,
    dinero :: Number,
    reparaciones :: [Reparacion],
    herramientas :: [Herramienta]
}   deriving (Show, Eq)

data Herramienta = UnaHerramienta {
    denominacion :: String,
    empuñadura :: Material,
    precio :: Number 
}   deriving (Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Show,Eq)

-- 1) Modelar a los plomeros y sus herramientas

-- a) Mario, un plomero que tiene $1200, 
-- no hizo ninguna reparación hasta ahora y 
-- en su caja de herramientas lleva una llave inglesa 
-- con mango de hierro que tiene un precio de $200 
-- y un martillo con empuñadura de madera que le salió $20.

mario :: Plomero
mario = UnPlomero "Mario" 1200 [] [UnaHerramienta "llave inglesa" Hierro 200 , UnaHerramienta "martillo" Madera 20] 

-- b) Wario, tiene 50 centavos encima, no hizo reparaciones, 
-- lleva infinitas llaves francesas, obviamente de hierro, 
-- la primera le salió un peso, pero cada una que compraba 
-- le salía un peso más cara. La inflación lo está matando

wario :: Plomero
wario = UnPlomero "Wario" 0.50 [] (map (UnaHerramienta "llave francesa" Hierro) [1..]) 

-- 2) Saber si un plomero:

-- a) Tiene una herramienta con cierta denominación.
--tieneHerramienta'' :: Plomero -> Herramienta -> Bool 
--tieneHerramienta' plomero herramienta = (denominacion herramienta) `elem` (herramientas plomero . denominacion)

tieneHerramienta :: String -> Plomero -> Bool                                                                           -- any porque con al menos tener una herramienta con esa denomicion ya es TRUE
tieneHerramienta denominacionHerramienta = any ((== denominacionHerramienta). denominacion) . herramientas  

tieneHerramienta' :: String -> Plomero -> Bool 
tieneHerramienta' denominacion plomero = any (== denominacion) (listaDeDenominaciones plomero)

--listaDeHerramientas :: Plomero -> [Herramienta]      --> esto seria lo que hace herramientas
--listaDeHerramientas plomero = herramientas plomero 

listaDeDenominaciones :: Plomero -> [String]
listaDeDenominaciones plomero = map denominacion (herramientas plomero)

-- b) Es malvado: se cumple si su nombre empieza con Wa.

esMalvado :: Plomero -> Bool
esMalvado = (=="Wa") . take 2 . nombre

-- c) Puede comprar una herramienta: esto sucede si tiene el dinero suficiente para pagar el precio de la misma.

tenedor :: Herramienta                                  -- ejemplito para utilizar
tenedor = UnaHerramienta "tenedor" Hierro 100

puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar herramienta plomero = dinero plomero >= precio herramienta

puedeComprar' :: Herramienta -> Plomero -> Bool
puedeComprar' herramienta = (>= precio herramienta) . dinero

-- 3) Saber si una herramienta es buena, cumpliendose solamente si tiene empuñadura de hierro que sale más de $10000 
-- o es un martillo con mango de madera o goma.

--esHerramientaBuena :: Herramienta -> Bool
--esHerramientaBuena herramienta = (empuñadura herramienta == Hiero && precio herramienta > 10000) || (nombre herramienta == "martillo" && empuñadura herramienta == (Madera || Goma))

esBuena :: Herramienta -> Bool
esBuena (UnaHerramienta     _      Hierro precio) = precio > 1000                       -- si tiene empuñadura de hierro y sale mas de $10000
esBuena (UnaHerramienta "martillo" material  _  ) = material `elem` [Madera,Goma]       -- si tiene empuñadura de madera o goma 
esBuena _                                         = False                               -- si no me dan herramienta --> entonces NO es buena 

-- 4) Todo plomero necesita comprar una herramienta, cuando lo hace paga su precio y agrega la herramienta a las suyas. Solo sucede si puede pagarlo.

comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta newherramienta plomero 
    | puedeComprar newherramienta plomero = (perderDinero newherramienta . agregarHerramienta newherramienta) plomero
    | otherwise = plomero 

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta newherramienta plomero = plomero {herramientas = newherramienta : herramientas plomero}

perderDinero :: Herramienta -> Plomero -> Plomero
perderDinero herramienta plomero = plomero {dinero = dinero plomero - precio herramienta}

-- 5) Hay un sinfín de reparaciones que los plomeros deben resolver. 
-- Cada una de ellas goza de una descripción del problema a reparar y un requerimiento que varía dependiendo de la reparación. 
-- Por ejemplo, una filtración de agua requiere que un plomero tenga una llave inglesa en su caja de herramientas.

-- a) Modelar las reparaciones y la filtración de agua.

data Reparacion = UnaReparacion {
    descripcion :: String,
    requerimiento :: (Plomero -> Bool)
}deriving (Show,Eq)

filtracionDeAgua :: Reparacion
filtracionDeAgua = UnaReparacion "filtracion de agua" requerimientoFiltracionDeAgua

requerimientoFiltracionDeAgua :: Plomero -> Bool
requerimientoFiltracionDeAgua plomero = tieneHerramienta' "llave inglesa" plomero

reparacionDificil :: Reparacion
reparacionDificil = UnaReparacion (replicate 101 'A') requerimientoFiltracionDeAgua

-- b) Saber si una reparación es difícil: esto ocurre cuando su descripción es complicada, 
-- es decir que tiene más de 100 caracteres y además es un grito, es decir está escrito totalmente en mayúsculas.

esDificil :: Reparacion -> Bool
esDificil = esComplicada . descripcion

esComplicada :: String -> Bool
esComplicada palabra = esGrito palabra && tieneMasDe100Caracteres palabra 

tieneMasDe100Caracteres :: String -> Bool
tieneMasDe100Caracteres = (>100) . length

esGrito :: String -> Bool
esGrito palabra = all (esMayuscula) palabra 

-- isUpper :: Char -> Bool -- Esta función me dice si una letra es mayúscula o no 

esMayuscula :: Char -> Bool
esMayuscula letra = letra `elem` ['A'..'Z']

-- c)Saber el presupuesto de una reparación, el cual se calcula como el 300% de la longitud de su descripción 
-- (por eso es importante describir los problemas de manera sencilla).

presupuestoReparacion :: Reparacion -> Number
presupuestoReparacion reparacion = (length (descripcion reparacion)) * 3

-- 6) Hacer que un plomero haga una reparación. 
-- * Si no puede resolverla te cobra $100 la visita. 
-- * Si puede hacerla, cobra el dinero por el presupuesto de la misma y agrega esa reparación a su historial de reparaciones, además de:

--Si el plomero es malvado, le roba al cliente un destornillador con mango de plástico, claramente su precio es nulo.
--Si no es malvado y la reparación es difícil, pierde todas sus herramientas buenas.
--Si no es malvado ni es difícil la reparación, sólo se olvida la primera de sus herramientas.
--Un plomero puede hacer una reparación si cumple su requerimiento o es un plomero malvado con un martillo.

intentarHacerReparacion :: Reparacion -> Plomero -> Plomero
intentarHacerReparacion reparacion plomero 
    | puedeHacerReparacion reparacion plomero = (cambiarHerramientasSegun reparacion. agregarReparacion reparacion. cobrarPresupuesto reparacion) plomero   
    | otherwise = cobrarDinero 100 plomero

cambiarHerramientasSegun :: Reparacion -> Plomero -> Plomero
cambiarHerramientasSegun reparacion plomero 
    | esMalvado plomero = leRoba plomero
    | esDificil reparacion = perderHerramientasBuenas plomero 
    | otherwise = plomero {herramientas = tail (herramientas plomero)}

leRoba :: Plomero -> Plomero
leRoba plomero = agregarHerramienta (UnaHerramienta "destornillador" Plastico 0) plomero

perderHerramientasBuenas :: Plomero -> Plomero
perderHerramientasBuenas plomero = plomero {herramientas = filter (not . esBuena) (herramientas plomero)}

cobrarPresupuesto :: Reparacion -> Plomero -> Plomero
cobrarPresupuesto reparacion plomero = cobrarDinero (presupuestoReparacion reparacion) plomero

agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion newReparacion plomero = plomero {reparaciones = newReparacion : reparaciones plomero}

puedeHacerReparacion :: Reparacion -> Plomero -> Bool
puedeHacerReparacion reparacion plomero = (requerimiento reparacion plomero) || (esMalvado plomero && tieneHerramienta' "martillo" plomero)

cobrarDinero :: Number -> Plomero -> Plomero
cobrarDinero delta plomero = plomero {dinero = dinero plomero + delta}

-- 7) Nintendo, pese a ser una empresa de consolas y juegos, gana millones de dólares con su red de plomeros. 
-- Cada plomero realiza varias reparaciones en un día. Necesitamos saber cómo afecta a un plomero una jornada de trabajo. 
-- Bajan línea desde Nintendo que no usemos recursividad.

type Plomeros = [Plomero]

jornadaDeTrabajo :: [Reparacion] -> Plomero -> Plomero
jornadaDeTrabajo reparaciones plomero = foldr intentarHacerReparacion plomero reparaciones

-- 8) Nintendo beneficia a sus plomeros según ciertos criterios, es por eso que necesita saber, dado un conjunto de reparaciones a realizar 
-- en una jornada laboral, cuál de todos sus empleados es:
-- * El empleado más reparador: El plomero que más reparaciones tiene en su historial una vez realizada su jornada laboral.
-- * El empleado más adinerado: El plomero que más dinero tiene encima una vez realizada su jornada laboral.
-- * El empleado que más invirtió: El plomero que más plata invertida tiene entre las herramientas que le quedaron una vez realizada su jornada laboral.

type Criterio = [Reparacion] -> Plomero -> Number

--empleadoMasReparador :: [Reparacion] -> [Plomero] -> Plomero
--empleadoMasReparador reparaciones (plomero1:plomero2:plomeros) 
--    | cantididadReparacionesTotales reparaciones plomero1 > cantididadReparacionesTotales reparaciones plomero2 = empleadoMasReparador reparaciones (plomero1:plomeros)
--    | otherwise = empleadoMasReparador reparaciones (plomero2:plomeros)

elEmpleadoMasSegun :: Criterio -> [Reparacion] -> [Plomero] -> Plomero
elEmpleadoMasSegun criterio repas (plomero1:plomero2:plomeros)
    | criterio repas plomero1 > criterio repas plomero2 = elEmpleadoMasSegun criterio repas (plomero1:plomeros)
    | otherwise = elEmpleadoMasSegun criterio repas (plomero2:plomeros)

-- CRITERIOS -- 

cantididadReparacionesTotales :: Criterio
cantididadReparacionesTotales unasReparaciones plomero = length (reparaciones (jornadaDeTrabajo unasReparaciones plomero))

cantididadDineroTotal :: Criterio
cantididadDineroTotal unasReparaciones plomero = dinero (jornadaDeTrabajo unasReparaciones plomero)

cantidadPlataInvertida :: Criterio
cantidadPlataInvertida unasReparaciones plomero = totalPlataHerramientas (jornadaDeTrabajo unasReparaciones plomero)

totalPlataHerramientas :: Plomero -> Number
totalPlataHerramientas plomero = sum (map precio (herramientas plomero)) 

