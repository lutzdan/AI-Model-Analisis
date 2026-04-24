module ArchivoIA where

import TiposIA
import AnalisisIA
import Data.List (intercalate)

-- ============================================================
--  ArchivoIA.hs
--  Manejo de archivos: lectura de experimentos.txt,
--  parseo de líneas y escritura del reporte.txt
-- ============================================================


-- ----------------------------------------------------------
--  LECTURA Y PARSEO
-- ----------------------------------------------------------

-- | Lee el archivo de experimentos y devuelve la lista de registros.
leerExperimentos :: FilePath -> IO [Experimento]
leerExperimentos path = do
  contenido <- readFile path
  let lineas = lines contenido
  return (parsearLineas lineas)

-- | Convierte una lista de líneas en registros Experimento.
--   Ignora líneas vacías o que sean la cabecera (ajuste de patrones).
parsearLineas :: [String] -> [Experimento]
parsearLineas []     = []
parsearLineas (l:ls)
  | esLineaValida l = parsearLinea l : parsearLineas ls
  | otherwise       = parsearLineas ls

-- | Determina si una línea contiene datos válidos (no cabecera/vacía).
--   Usa guardas.
esLineaValida :: String -> Bool
esLineaValida linea
  | null linea                  = False
  | head (words linea) == "modelo" = False
  | length (words linea) /= 4  = False
  | otherwise                   = True

-- | Parsea una línea con el formato: nombre precision perdida epocas
parsearLinea :: String -> Experimento
parsearLinea linea =
  let ws = words linea
  in Experimento
       { modelo    = ws !! 0
       , precision = read (ws !! 1)
       , perdida   = read (ws !! 2)
       , epocas    = read (ws !! 3)
       }


-- ----------------------------------------------------------
--  GENERACIÓN DEL REPORTE
-- ----------------------------------------------------------

-- | Escribe el reporte completo en reporte.txt.
escribirReporte :: FilePath -> [Experimento] -> IO ()
escribirReporte path exps = do
  let contenido = generarReporte exps
  writeFile path contenido
  putStrLn $ "Reporte generado en: " ++ path

-- | Construye el texto completo del reporte.
generarReporte :: [Experimento] -> String
generarReporte exps = unlines
  [ separador '=' 55
  , centrar "REPORTE DE ANALISIS DE MODELOS DE IA"
  , separador '=' 55
  , ""
  , "  Total de modelos analizados : " ++ show (contarExperimentos exps)
  , "  Promedio de precision       : " ++ formatFloat (promedioPrecision exps)
  , "  Promedio de perdida         : " ++ formatFloat (promedioPerdida exps)
  , "  Suma total de epocas        : " ++ show (sumarEpocas exps)
  , ""
  , separador '-' 55
  , "  MEJOR MODELO"
  , separador '-' 55
  , mostrarMejor (mejorModelo exps)
  , ""
  , separador '-' 55
  , "  MODELOS DESTACADOS (Excelente / Bueno)"
  , separador '-' 55
  , mostrarDestacados (modelosDestacados exps)
  , ""
  , separador '-' 55
  , "  TABLA DE CLASIFICACION COMPLETA"
  , separador '-' 55
  , encabezadoTabla
  , separador '-' 55
  , concatMap filaTabla exps
  , separador '=' 55
  ]

-- | Formatea la información del mejor modelo.
mostrarMejor :: Maybe Experimento -> String
mostrarMejor Nothing  = "  (sin datos)"
mostrarMejor (Just e) =
  "  Modelo    : " ++ modelo e    ++ "\n" ++
  "  Precision : " ++ formatFloat (precision e) ++ "\n" ++
  "  Perdida   : " ++ formatFloat (perdida e)   ++ "\n" ++
  "  Epocas    : " ++ show (epocas e)

-- | Lista los modelos destacados con su clasificación.
mostrarDestacados :: [Experimento] -> String
mostrarDestacados [] = "  (ninguno supera el umbral Bueno)\n"
mostrarDestacados ds =
  concatMap (\e -> "  * " ++ modelo e ++ " [" ++
                   desempenoTexto (clasificar e) ++ "]\n") ds

-- | Encabezado de la tabla final.
encabezadoTabla :: String
encabezadoTabla =
  padR 15 "  Modelo" ++
  padR 12 "Precision" ++
  padR 10 "Perdida" ++
  padR 8  "Epocas" ++
  "Clasificacion\n"

-- | Genera una fila de la tabla para un experimento.
filaTabla :: Experimento -> String
filaTabla e =
  padR 15 ("  " ++ modelo e) ++
  padR 12 (formatFloat (precision e)) ++
  padR 10 (formatFloat (perdida e))   ++
  padR 8  (show (epocas e))           ++
  desempenoTexto (clasificar e) ++ "\n"


-- ----------------------------------------------------------
--  UTILIDADES DE FORMATO
-- ----------------------------------------------------------

formatFloat :: Float -> String
formatFloat f = show (fromIntegral (round (f * 100)) / 100.0 :: Float)

separador :: Char -> Int -> String
separador c n = replicate n c

centrar :: String -> String
centrar s =
  let ancho = 55
      pad   = (ancho - length s) `div` 2
  in replicate pad ' ' ++ s

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
