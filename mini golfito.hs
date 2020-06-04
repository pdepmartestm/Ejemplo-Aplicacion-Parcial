-- JUGADOR --
data Jugador = Jugador {
    nombre :: String,
    padre :: String,
    habilidades :: Habilidades
} deriving (Show, Eq)

bart = Jugador "Bart" "Homero" (25, 60)
todd = Jugador "Todd" "Ned" (15, 80)
rafa = Jugador "Rafa" "Clancy" (10, 1)

type Fuerza = Int
type Precision = Int

type Habilidades = (Fuerza, Precision)

fuerza :: Jugador -> Fuerza
fuerza jugador = fst (habilidades jugador)

precisionJ :: Jugador -> Precision
precisionJ jugador = snd (habilidades jugador)

-- TIRO --
data Tiro = Tiro {
    velocidad :: Int,
    precision :: Int,
    altura :: Int
} deriving (Show, Eq)

tiroMuerto = Tiro 0 0 0

conVelocidad n tiro = tiro { velocidad = n }
conPrecision n tiro = tiro { precision = n }
conAltura n tiro = tiro { altura = n }

cambiarVelocidad f tiro = tiro { velocidad = f (velocidad tiro)} 
cambiarAltura f tiro = tiro { altura = f (altura tiro)} 

-- PALOS --
type Palo = Jugador -> Tiro

-- putter :: Jugador -> Tiro
putter :: Palo
putter jugador = conVelocidad 10 (conPrecision (2 *precisionJ jugador) tiroMuerto)

madera :: Palo
madera jugador = conVelocidad 100 (conAltura 5 (conPrecision (mitad (precisionJ jugador)) tiroMuerto))

-- madera jugador = Tiro {velocidad = 100, altura = 5, precision = mitad (precisionJ jugador)}

-- hierro :: Int -> Jugador -> Tiro
hierro :: Int -> Palo
hierro n jugador = conVelocidad (n * fuerza jugador) (conPrecision (dividirPor n (precisionJ jugador)) (conAltura (cuadrado n) tiroMuerto))

-- OBSTACULOS --
type Obstaculo = (Condicion, Efecto)

type Condicion = Tiro -> Bool
type Efecto = Tiro -> Tiro

condicion :: Obstaculo -> Condicion
condicion obstaculo = fst obstaculo

efecto :: Obstaculo -> Efecto
efecto obstaculo = snd obstaculo

tunelConRampita :: Obstaculo
tunelConRampita = (condicionRampa, efectoRampa)

-- condicionRampa :: Tiro -> Bool
condicionRampa :: Condicion
condicionRampa tiro = buenaPrecision tiro && sinAltura tiro

-- efectoRampa :: Tiro -> Tiro
efectoRampa :: Efecto
efectoRampa tiro = conVelocidad (velocidad tiro) (conPrecision 100 tiroMuerto)

laguna :: Int -> Obstaculo
laguna largo = (condicionLaguna, efectoLaguna largo)

--laguna largo = (
--    (\tiro -> buenaVelocidad tiro && entre 10 50 (altura tiro))
--    , (\tiro -> tiro {altura = dividirPor largo (altura tiro)}))

condicionLaguna :: Condicion
condicionLaguna tiro = buenaVelocidad tiro && entre 10 50 (altura tiro)

--efectoLaguna :: Int -> Efecto
efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiro = conAltura (dividirPor largo (altura tiro)) tiro 

hoyo :: Obstaculo
hoyo = (condicionHoyo, efectoHoyo)

condicionHoyo :: Condicion
condicionHoyo tiro = entre 5 20 (velocidad tiro) && excelentePrecision tiro && sinAltura tiro

efectoHoyo :: Efecto 
efectoHoyo tiro = tiroMuerto

superHoyo::Obstaculo
superHoyo = ( not.condicionHoyo , efectoLaguna 10.efectoHoyo)

superHoyo = (\ tiro ->  not (condicionHoyo tiro), (\tiro -> efectoLaguna 10 (efectoHoyo tiro))
        
buenaPrecision = esBuena.precision
buenaVelocidad tiro = esBuena (velocidad tiro)

excelentePrecision tiro = esExcelente (precision tiro)


sinAltura tiro = (altura tiro) == 0
sinAltura = (0==).altura


esBuena n = n > 80
esExcelente n = n > 95


-- REQUERIMIENTOS --
-- type Palo = Jugador -> Tiro

golpear :: Jugador -> Palo -> Tiro
golpear jugador palo = palo jugador


atravesar :: Obstaculo -> Tiro -> Tiro
atravesar (cond, efec) tiro 
    | cond tiro = efec tiro
    | otherwise = tiroMuerto

atravesar obstaculo tiro 
    | (condicion obstaculo) tiro = (efecto obstaculo) tiro
    | otherwise = tiroMuerto

-- UTILS --
entre n m x = elem x [n..m]
mitad = dividirPor 2
dividirPor = flip div
cuadrado = (^2)

doble = (*2)
   

