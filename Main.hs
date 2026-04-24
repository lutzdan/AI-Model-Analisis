module Main where

import TiposIA
import AnalisisIA
import ArchivoIA

-- ============================================================
--  Main.hs
--  Punto de entrada del programa. Coordina la lectura del
--  archivo de experimentos, el análisis y la generación del
--  reporte.
-- ============================================================

main :: IO ()
main = do
  putStrLn ""
  putStrLn ".____________________________________________."
  putStrLn "|                                           |"
  putStrLn "|    Analizador de Experimentos de IA       |"
  putStrLn "|___________________________________________|"
  putStrLn ""

  -- 1. Leer experimentos desde archivo
  let archivoEntrada = "experimentosPrueba2.txt"
  putStrLn $ "Leyendo datos desde: " ++ archivoEntrada
  experimentos <- leerExperimentos archivoEntrada

  -- 2. Verificar que se leyeron datos
  if esListaVacia experimentos
    then putStrLn "Error: No se encontraron experimentos válidos."
    else do
      putStrLn $ "Experimentos cargados: " ++ show (contarExperimentos experimentos)
      putStrLn ""

      -- 3. Mostrar métricas en consola
      putStrLn "-- Métricas generales ---------------------"
      putStrLn $ "  Promedio de precisión : " ++ show (promedioPrecision experimentos)
      putStrLn $ "  Promedio de pérdida   : " ++ show (promedioPerdida   experimentos)
      putStrLn $ "  Suma de épocas        : " ++ show (sumarEpocas        experimentos)
      putStrLn ""

      -- 4. Mostrar tuplas (modelo, precisión)
      putStrLn "-- Tuplas (modelo, precisión) ---------------------"
      mapM_ (putStrLn . ("  " ++) . show) (modelosPrecision experimentos)
      putStrLn ""

      -- 5. Mostrar clasificaciones
      putStrLn "-- Clasificación por desempeño ---------------------"
      mapM_ (\(m, d) -> putStrLn $ "  " ++ m ++ " -> " ++ desempenoTexto d)
            (tablaClasificacion experimentos)
      putStrLn ""

      -- 6. Mejor modelo
      case mejorModelo experimentos of
        Nothing -> putStrLn "  (sin datos para mejor modelo)"
        Just e  -> putStrLn $ "-- Mejor modelo: " ++ modelo e ++
                              " (precisión: " ++ show (precision e) ++ ")"
      putStrLn ""

      -- 7. Buscar modelos específicos de ejemplo
      let busquedas = ["CNN", "Transformer"]
      putStrLn "-- Búsqueda de modelos ---------------------"
      mapM_ (\nom ->
        putStrLn $ "  ¿Existe \"" ++ nom ++ "\"? " ++
                   if buscarModelo nom experimentos then "Si" else "No"
        ) busquedas
      putStrLn ""

      -- 8. Generar reporte
      let archivoSalida = "reporte.txt"
      escribirReporte archivoSalida experimentos
      putStrLn ""
      putStrLn "________________________________________________"
      putStrLn "|                                              |"
      putStrLn "|       Proceso completado exitosamente        |"
      putStrLn "|______________________________________________|"
      putStrLn ""
