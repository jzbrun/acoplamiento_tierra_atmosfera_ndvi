library(data.table)
dt_raw <- rbindlist(list(
  fread("Bosque_chunk_1.csv"),
  fread("Arbustal_chunk_1.csv"),
  fread("Pastizal_chunk_1.csv")
))
dt <- lsat_format_data(dt_raw)
dt_clean <- lsat_clean_data(dt)
nrow(dt)
nrow(dt_clean)

100 * nrow(dt_clean) / nrow(dt)

library(dplyr)

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

dt_ndvi <- lsat_calc_spectral_index(
  dt_clean,
  si = "ndvi"
)


summary(dt_ndvi$ndvi)

library(ggplot2)

library(ggplot2)
ggplot(dt_ndvi,
       aes(x = cover,
           y = ndvi,
           fill = cover)) +
  geom_boxplot(outlier.size = 0.3) +
  labs(
    x = "",
    y = "NDVI"
  ) +
  theme_bw()

dt_ndvi %>%
  group_by(cover) %>%
  summarise(
    observaciones = n(),
    puntos = n_distinct(sample.id)
  )


dt_cal <- lsat_calibrate_poly(
  dt_ndvi,
  band.or.si = "ndvi",
  train.with.highlat.data = TRUE,
  overwrite.col = TRUE
)
summary(dt_ndvi$ndvi)

dt_ndvi %>%
  group_by(cover) %>%
  summarise(
    n = n(),
    media = mean(ndvi, na.rm = TRUE),
    sd = sd(ndvi, na.rm = TRUE),
    minimo = min(ndvi, na.rm = TRUE),
    maximo = max(ndvi, na.rm = TRUE)
  )


library(dplyr)

dt_ndvi %>%
  group_by(sample.id, cover) %>%
  summarise(
    n_years = n_distinct(year),
    .groups = "drop"
  ) %>%
  group_by(cover) %>%
  summarise(
    min = min(n_years),
    q25 = quantile(n_years, 0.25),
    mediana = median(n_years),
    media = mean(n_years),
    max = max(n_years)
  )


dt_cal <- lsat_calibrate_poly(
  dt_ndvi,
  band.or.si = "ndvi",
  train.with.highlat.data = TRUE,
  overwrite.col = TRUE
)
dt_pheno <- lsat_fit_phenological_curves(
  dt_cal,
  si = "ndvi"
)
dt_gs <- lsat_summarize_growing_seasons(
  dt_pheno,
  si = "ndvi"
)

library(dplyr)

muestras <- muestras %>%
  rename(sample.id = sample_id)
dt_gs <- dt_gs %>%
  left_join(
    st_drop_geometry(muestras),
    by = "sample.id"
  )
table(dt_gs$cover)

ndvi_anual <- dt_gs %>%
  group_by(year, cover) %>%
  summarise(
    ndvi_max = mean(ndvi.max, na.rm = TRUE),
    sd = sd(ndvi.max, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )
head(ndvi_anual)

summary(ndvi_anual)

table(ndvi_anual$cover)


library(ggplot2)

ggplot(ndvi_anual,
       aes(year, ndvi_max,
           color = cover,
           group = cover)) +
  
  geom_line(linewidth = 1) +
  
  geom_point(size = 2) +
  
  geom_ribbon(aes(ymin = ndvi_max - se,
                  ymax = ndvi_max + se,
                  fill = cover),
              alpha = 0.20,
              colour = NA) +
  
  theme_bw(base_size = 14) +
  
  labs(
    x = "Año",
    y = "NDVI máximo",
    color = "Cobertura",
    fill = "Cobertura"
  )
ndvi_anual %>%
  group_by(cover) %>%
  do(broom::tidy(
    lm(ndvi_max ~ year, data = .)
  ))
