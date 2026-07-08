###############################################################
#
# ACOPLAMIENTO TIERRA–ATMÓSFERA EN AMBIENTE SEMIÁRIDO
# UN ESTUDIO DEL BOSQUE SERRANO EN EL NORESTE
# DE LA PROVINCIA DE SAN LUIS, ARGENTINA
#
# TESIS DOCTORAL
#
# Autor:
# Juan Pablo Zbrun Luoni
#
# SCRIPT 02
# CALIBRACIÓN INTERSENSOR LANDSAT
#
# Basado en:
# Bayle et al. (2024)
# Berner et al. (2023)
#
###############################################################

###############################################################
# 1. LIBRERÍAS
###############################################################

library(LandsatTS)

library(data.table)

library(dplyr)

library(ggplot2)

library(Metrics)

library(broom)

library(scales)

library(viridis)

library(patchwork)

library(readr)

theme_set(theme_bw(base_size = 14))

options(stringsAsFactors = FALSE)

###############################################################
# 2. CARPETAS DEL PROYECTO
###############################################################

dir.create(
  "Resultados",
  showWarnings = FALSE
)

dir.create(
  "Resultados/Tablas",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "Resultados/Figuras",
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################
# 3. LECTURA DE LA BASE PREPROCESADA
###############################################################

cat("=============================================\n")

cat("SCRIPT 02 - CALIBRACIÓN INTERSENSOR\n")

cat("=============================================\n")

dt_ndvi <-
  
  readRDS(
    
    "Resultados/dt_ndvi.rds"
    
  )

cat("Observaciones disponibles:",
    
    nrow(dt_ndvi),
    
    "\n")

cat("Coberturas:\n")

print(
  
  table(
    
    dt_ndvi$cover
    
  )
  
)

cat("Sensores:\n")

print(
  
  table(
    
    dt_ndvi$satellite
    
  )
  
)

###############################################################
# 4. CALIBRACIÓN INTERSENSOR
###############################################################

cat("---------------------------------------------\n")

cat("Aplicando calibración polinómica...\n")

dt_cal <-
  
  lsat_calibrate_poly(
    
    dt_ndvi,
    
    band.or.si = "ndvi",
    
    train.with.highlat.data = TRUE,
    
    overwrite.col = FALSE
    
  )

cat("Calibración finalizada.\n")

###############################################################
# 5. CONTROL DE CALIDAD
###############################################################

cat("---------------------------------------------\n")

cat("Variables disponibles\n")

print(
  
  names(dt_cal)
  
)

cat("---------------------------------------------\n")

cat("Variables NDVI\n")

print(
  
  grep(
    
    "ndvi",
    
    names(dt_cal),
    
    value = TRUE
    
  )
  
)

###############################################################
# 6. RESUMEN DE LA CALIBRACIÓN
###############################################################

cat("---------------------------------------------\n")

cat("NDVI original\n")

print(
  
  summary(
    
    dt_cal$ndvi
    
  )
  
)

cat("---------------------------------------------\n")

cat("NDVI calibrado\n")

print(
  
  summary(
    
    dt_cal$ndvi.xcal
    
  )
  
)

###############################################################
# 7. CONTROL DE VALORES FALTANTES
###############################################################

cat("---------------------------------------------\n")

cat("Valores NA\n")

na_original <-
  
  sum(
    
    is.na(
      
      dt_cal$ndvi
      
    )
    
  )

na_calibrado <-
  
  sum(
    
    is.na(
      
      dt_cal$ndvi.xcal
      
    )
    
  )

cat("NDVI original :",na_original,"\n")

cat("NDVI calibrado:",na_calibrado,"\n")

###############################################################
# 8. RANGO DE LOS DATOS
###############################################################

cat("---------------------------------------------\n")

cat("Rango NDVI original\n")

print(
  
  range(
    
    dt_cal$ndvi,
    
    na.rm = TRUE
    
  )
  
)

cat("---------------------------------------------\n")

cat("Rango NDVI calibrado\n")

print(
  
  range(
    
    dt_cal$ndvi.xcal,
    
    na.rm = TRUE
    
  )
  
)

###############################################################
# 9. OBSERVACIONES POR SENSOR
###############################################################

tabla_sensor <-
  
  dt_cal %>%
  
  group_by(
    
    satellite
    
  )%>%
  
  summarise(
    
    observaciones=n(),
    
    inicio=min(year),
    
    fin=max(year),
    
    escenas=n_distinct(landsat.scene.id),
    
    .groups="drop"
    
  )

print(tabla_sensor)

###############################################################
# 10. OBSERVACIONES POR COBERTURA Y SENSOR
###############################################################

tabla_cobertura <-
  
  dt_cal %>%
  
  group_by(
    
    cover,
    
    satellite
    
  )%>%
  
  summarise(
    
    observaciones=n(),
    
    puntos=n_distinct(sample.id),
    
    .groups="drop"
    
  )

print(tabla_cobertura)

###############################################################
# 11. EXPORTACIÓN DE TABLAS PRELIMINARES
###############################################################

write_csv(
  
  tabla_sensor,
  
  "Resultados/Tablas/Tabla03_Sensores.csv"
  
)

write_csv(
  
  tabla_cobertura,
  
  "Resultados/Tablas/Tabla04_Cobertura_Sensor.csv"
  
)

cat("---------------------------------------------\n")

cat("Primera etapa finalizada.\n")

cat("---------------------------------------------\n")

