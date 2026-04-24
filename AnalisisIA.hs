module AnalisisIA where

import TiposIA

-- ============================================================
--  AnalisisIA.hs
--  Contiene todas las funciones de análisis sobre la lista
--  de experimentos: estadísticas, clasificación y filtrado.
-- ============================================================


-- ----------------------------------------------------------
--  CLASIFICACIÓN CON GUARDAS
-- ----------------------------------------------------------

-- | Clasifica el desempeño de un experimento según su precisión.
--   Usa guardas para determinar la categoría.
clasificar :: Experimento -> Desempeno
clasificar exp
  | precision exp >= 0.95 = Excelente
  | precision exp >= 0.85 = Bueno
  | precision exp >= 0.75 = Regular
  | otherwise             = Deficiente


-- ----------------------------------------------------------
--  FUNCIONES RECURSIVAS
-- ----------------------------------------------------------

-- | Cuenta el número de experimentos en la lista.
contarExperimentos :: [Experimento] -> Int
contarExperimentos []     = 0
contarExperimentos (_:xs) = 1 + contarExperimentos xs

-- | Suma el total de épocas de todos los experimentos.
sumarEpocas :: [Experimento] -> Int
sumarEpocas []     = 0
sumarEpocas (x:xs) = epocas x + sumarEpocas xs

-- | Suma la precisión de todos los experimentos (auxiliar).
sumarPrecision :: [Experimento] -> Float
sumarPrecision []     = 0.0
sumarPrecision (x:xs) = precision x + sumarPrecision xs

-- | Suma la pérdida de todos los experimentos (auxiliar).
sumarPerdida :: [Experimento] -> Float
sumarPerdida []     = 0.0
sumarPerdida (x:xs) = perdida x + sumarPerdida xs

-- | Calcula el promedio de precisión de la lista.
promedioPrecision :: [Experimento] -> Float
promedioPrecision [] = 0.0
promedioPrecision exps =
  sumarPrecision exps / fromIntegral (contarExperimentos exps)

-- | Calcula el promedio de pérdida de la lista.
promedioPerdida :: [Experimento] -> Float
promedioPerdida [] = 0.0
promedioPerdida exps =
  sumarPerdida exps / fromIntegral (contarExperimentos exps)

-- | Busca si existe un modelo con el nombre dado.
buscarModelo :: String -> [Experimento] -> Bool
buscarModelo _ []     = False
buscarModelo nom (x:xs)
  | modelo x == nom = True
  | otherwise        = buscarModelo nom xs


-- ----------------------------------------------------------
--  MEJOR MODELO (ajuste de patrones + recursión)
-- ----------------------------------------------------------

-- | Devuelve el experimento con mayor precisión.
mejorModelo :: [Experimento] -> Maybe Experimento
mejorModelo [] = Nothing
mejorModelo xs = Just (foldr1 mayor xs)
  where
    mayor a b
      | precision a >= precision b = a
      | otherwise                  = b


-- ----------------------------------------------------------
--  TUPLAS: lista (modelo, precisión)
-- ----------------------------------------------------------

-- | Genera una lista de tuplas (nombre, precisión).
--   Usa comprensión de listas.
modelosPrecision :: [Experimento] -> [ModeloPrecision]
modelosPrecision exps = [(modelo e, precision e) | e <- exps]


-- ----------------------------------------------------------
--  FILTRADO CON COMPRENSIÓN DE LISTAS
-- ----------------------------------------------------------

-- | Filtra experimentos cuya precisión supera el umbral dado.
filtrarPorUmbral :: Float -> [Experimento] -> [Experimento]
filtrarPorUmbral umbral exps =
  [e | e <- exps, precision e >= umbral]

-- | Devuelve los experimentos clasificados como Excelente o Bueno.
modelosDestacados :: [Experimento] -> [Experimento]
modelosDestacados exps =
  [e | e <- exps, clasificar e `elem` [Excelente, Bueno]]

-- | Genera una lista de pares (modelo, clasificación).
tablaClasificacion :: [Experimento] -> [(String, Desempeno)]
tablaClasificacion exps =
  [(modelo e, clasificar e) | e <- exps]
