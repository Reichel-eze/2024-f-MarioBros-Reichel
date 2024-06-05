module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Plomero = UnPlomero {
    nombre :: String,
    dinero :: Number,
    reparaciones :: [String],
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
}

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


