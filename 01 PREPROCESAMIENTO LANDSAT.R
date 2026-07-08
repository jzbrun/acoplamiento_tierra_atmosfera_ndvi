###############################################################
#
# ACOPLAMIENTO TIERRA–ATMÓSFERA EN AMBIENTE SEMIÁRIDO
# UN ESTUDIO DEL BOSQUE SERRANO EN EL NORESTE
# DE LA PROVINCIA DE SAN LUIS, ARGENTINA
#
# Tesis Doctoral
#
# Autor:
# Juan Pablo Zbrun Luoni
#
# SCRIPT 01
# PREPROCESAMIENTO LANDSAT
#
###############################################################

###############################################################
# 1. LIBRERÍAS
###############################################################

library(LandsatTS)

library(data.table)

library(dplyr)

library(ggplot2)

library(scales)

library(readr)

###############################################################
# 2. CONFIGURACIÓN DEL PROYECTO
###############################################################

options(stringsAsFactors = FALSE)

theme_set(theme_bw(base_size = 14))

dir.create("Resultados", showWarnings = FALSE)

dir.create("Resultados/Tablas",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("Resultados/Figuras",
           recursive = TRUE,
           showWarnings = FALSE)

###############################################################
# 3. LECTURA DE LOS DATOS
###############################################################

cat("---------------------------------------\n")

cat("Lectura de datos Landsat\n")

cat("---------------------------------------\n")

dt_raw <- rbindlist(
  
  list(
    
    fread("Bosque_chunk_1.csv"),
    
    fread("Arbustal_chunk_1.csv"),
    
    fread("Pastizal_chunk_1.csv")
    
  )
  
)

cat("Observaciones leídas:", nrow(dt_raw), "\n")

###############################################################
# 4. FORMATEO
###############################################################

cat("---------------------------------------\n")

cat("Formateando datos...\n")

dt <- lsat_format_data(dt_raw)

###############################################################
# 5. LIMPIEZA
###############################################################

cat("---------------------------------------\n")

cat("Control de calidad\n")

dt_clean <- lsat_clean_data(dt)

cat("Observaciones originales :", nrow(dt), "\n")

cat("Observaciones válidas    :", nrow(dt_clean), "\n")

cat("Retención (%)            :",
    
    round(
      
      100 * nrow(dt_clean) /
        
        nrow(dt),
      
      2
      
    ),
    
    "\n")

###############################################################
# 6. CLASIFICACIÓN DE COBERTURAS
###############################################################

cat("---------------------------------------\n")

cat("Clasificando coberturas...\n")

dt_clean <- dt_clean %>%
  
  mutate(
    
    cover = case_when(
      
      grepl("^B_", sample.id) ~ "Bosque",
      
      grepl("^A_", sample.id) ~ "Arbustal",
      
      grepl("^P_", sample.id) ~ "Pastizal",
      
      TRUE ~ NA_character_
      
    )
    
  )

table(dt_clean$cover)

###############################################################
# 7. CÁLCULO DEL NDVI
###############################################################

cat("---------------------------------------\n")

cat("Calculando NDVI...\n")

dt_ndvi <- lsat_calc_spectral_index(
  
  dt_clean,
  
  si = "ndvi"
  
)

###############################################################
# 8. CONTROL DE CALIDAD DEL NDVI
###############################################################

cat("---------------------------------------\n")

cat("Resumen NDVI\n")

summary(dt_ndvi$ndvi)

cat("Valores NA:",
    
    sum(is.na(dt_ndvi$ndvi)),
    
    "\n")

cat("Rango NDVI:\n")

range(dt_ndvi$ndvi)

###############################################################
# 9. ANÁLISIS EXPLORATORIO
###############################################################

cols <- c(
  
  Bosque="#1b9e77",
  
  Arbustal="#d95f02",
  
  Pastizal="#7570b3"
  
)

Figura01 <- ggplot(
  
  dt_ndvi,
  
  aes(
    
    x = cover,
    
    y = ndvi,
    
    fill = cover
    
  )
  
)+
  
  geom_violin(
    
    trim = FALSE,
    
    alpha = .30,
    
    colour = NA
    
  )+
  
  geom_boxplot(
    
    width = .18,
    
    outlier.size = .20
    
  )+
  
  geom_jitter(
    
    width = .08,
    
    alpha = .02,
    
    size = .15
    
  )+
  
  scale_fill_manual(values = cols)+
  
  labs(
    
    x = NULL,
    
    y = "NDVI",
    
    fill = "Cobertura"
    
  )+
  
  theme_classic(base_size = 14)

Figura01

###############################################################
# 10. ESTADÍSTICOS DESCRIPTIVOS
###############################################################

cat("---------------------------------------\n")

cat("Calculando estadísticos...\n")

tabla_ndvi <- dt_ndvi %>%
  
  group_by(cover) %>%
  
  summarise(
    
    observaciones = n(),
    
    puntos = n_distinct(sample.id),
    
    media = mean(ndvi, na.rm = TRUE),
    
    mediana = median(ndvi, na.rm = TRUE),
    
    desvio = sd(ndvi, na.rm = TRUE),
    
    cv = 100 * desvio / media,
    
    minimo = min(ndvi, na.rm = TRUE),
    
    p05 = quantile(ndvi, .05),
    
    q25 = quantile(ndvi, .25),
    
    q75 = quantile(ndvi, .75),
    
    p95 = quantile(ndvi, .95),
    
    maximo = max(ndvi, na.rm = TRUE)
    
  )

print(tabla_ndvi)

###############################################################
# 11. COBERTURA TEMPORAL
###############################################################

tabla_temporal <- dt_ndvi %>%
  
  group_by(
    
    sample.id,
    
    cover
    
  ) %>%
  
  summarise(
    
    anios = n_distinct(year),
    
    .groups = "drop"
    
  ) %>%
  
  group_by(cover) %>%
  
  summarise(
    
    minimo = min(anios),
    
    q25 = quantile(anios,.25),
    
    mediana = median(anios),
    
    media = mean(anios),
    
    q75 = quantile(anios,.75),
    
    maximo = max(anios)
    
  )

print(tabla_temporal)

###############################################################
# 12. EXPORTACIÓN
###############################################################

write_csv(
  
  tabla_ndvi,
  
  "Resultados/Tablas/Tabla01_Estadisticos_NDVI.csv"
  
)

write_csv(
  
  tabla_temporal,
  
  "Resultados/Tablas/Tabla02_Cobertura_Temporal.csv"
  
)

ggsave(
  
  filename="Resultados/Figuras/Figura01_Distribucion_NDVI.png",
  
  plot=Figura01,
  
  width=18,
  
  height=12,
  
  units="cm",
  
  dpi=600
  
)

###############################################################
# 13. COMPOSICIÓN DE LA BASE DE DATOS
###############################################################

table(dt_ndvi$satellite)

table(
  dt_ndvi$cover,
  dt_ndvi$satellite
)


dt_ndvi %>%
  group_by(satellite) %>%
  summarise(
    inicio = min(year),
    fin = max(year),
    observaciones = n()
  )

imagenes_anuales <-
  
  dt_ndvi %>%
  
  group_by(year) %>%
  
  summarise(
    
    escenas=n_distinct(landsat.scene.id),
    
    observaciones=n()
    
  )


ggplot(
  imagenes_anuales,
  aes(year,escenas)
)+
  
  geom_col(fill="grey40")


dt_ndvi %>%
  
  group_by(
    year,
    cover
  )%>%
  
  summarise(
    n=n()
  )

saveRDS(
  
  dt_ndvi,
  
  "Resultados/dt_ndvi.rds"
  
)


###############################################################
# 13. FIN DEL SCRIPT
###############################################################

cat("---------------------------------------\n")

cat("SCRIPT 01 FINALIZADO\n")

cat("---------------------------------------\n")

