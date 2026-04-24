module TiposIA where

-- ============================================================
--  TiposIA.hs
--  Define los tipos de datos, registros y definiciones
--  auxiliares relacionadas con los experimentos de IA.
-- ============================================================

-- | Registro que representa un experimento de un modelo de IA
data Experimento = Experimento
  { modelo    :: String   -- Nombre del modelo
  , precision :: Float    -- Precisión (0.0 – 1.0)
  , perdida   :: Float    -- Pérdida (loss)
  , epocas    :: Int      -- Número de épocas de entrenamiento
  } deriving (Show, Eq)

-- | Tipo enumerado para clasificar el desempeño de un modelo
data Desempeno = Excelente | Bueno | Regular | Deficiente
  deriving (Show, Eq, Ord)

-- | Tupla conveniente: (nombre del modelo, precisión)
type ModeloPrecision = (String, Float)

-- | Convierte un Desempeno a su representación en texto
desempenoTexto :: Desempeno -> String
desempenoTexto Excelente  = "Excelente"
desempenoTexto Bueno      = "Bueno"
desempenoTexto Regular    = "Regular"
desempenoTexto Deficiente = "Deficiente"

-- | Devuelve True si la lista de experimentos está vacía
--   (ajuste de patrones)
esListaVacia :: [a] -> Bool
esListaVacia []    = True
esListaVacia _     = False

-- | Devuelve la cabeza de la lista de forma segura
--   (ajuste de patrones)
primeraOp :: [a] -> Maybe a
primeraOp []    = Nothing
primeraOp (x:_) = Just x
