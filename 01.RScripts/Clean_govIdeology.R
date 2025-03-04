# Base de datos: ideologia politica del gobierno
# Se hace un merge de las bases de datos correspondientes a la ideologia del
# gobierno. Estas bases son aquellas provistas por "Baker & Greene", y "Herre".

# Se seleccionan unicamente observaciones para latinoamerica entre 1970 y 2018.
# Dado que Baker & Greene unicamente brindan informacion para 1993 en adelante,
# se encuentra una elevada cantidad de observaciones faltantes.

# Adicionalmente, se construyen cuatro nuevas variables dummy para grupos de
# partidos politicos segun su inclinacion ideologica: L, CL, CR, R. Estas nuevas
# variables se construyen con el objetivo de hacer comparaciones con la califi-
# cacion elaborada por Herre, siguiendo el puntaje original asignado por Baker & 
# Greene.

# ---------- Preambulo ----------
# Fijo la direccion al directorio de trabajo
setwd("/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/00.Data")

# Cargo parqueterias de trabajo basicas
library(tidyverse)

# Cargo bases de datos originales
herre <- read.csv("herre_ideology.csv")
byg <- read.csv("baker_greene.csv")

# ---------- Limpieza general ---------- 
# Limpieza de la base de datos original de Herre
herre <- herre %>%
  filter(region == "Latin America and Caribbean",
         country_name %in% c("Argentina", "Bolivia", "Brazil", "Chile",
                             "Colombia", "Costa Rica", "Dominican Republic",
                             "Ecuador", "El Salvador", "Guatemala", "Honduras",
                             "Mexico", "Nicaragua", "Panama", "Paraguay",
                             "Peru", "Uruguay", "Venezuela"),
         year >= 1970 & year <= 2018) %>%
  select(country_name, year, hog_party, hog_party_abbr, hog_party_id,
         hog_ideology_num_full, hog_right, hog_left, hog_center, hog_noideo,
         hog_noinfo, hog_ideomiss)

# Guardo en "herre" la variable de ideologia de "byg" ("byg_ideology")
herre$byg_ideology <- byg$byg_ideology

# Elaboro dummies para grupos de ideologia gubernamental
herre <- herre %>%
  mutate(
    byg_right = case_when(
      byg_ideology > 15 ~ 1,
      TRUE ~ 0
    ),
    byg_cright = case_when(
      byg_ideology > 10 & byg_ideology <= 15 ~ 1,
      TRUE ~ 0
    ),
    byg_cleft = case_when(
      byg_ideology > 5 & byg_ideology <= 10 ~ 1,
      TRUE ~ 0
    ),
    byg_left = case_when(
      byg_ideology > 0 & byg_ideology <= 5 ~ 1,
      TRUE ~ 0
    )
  )

# ---------- Resultados finales ----------
# Guardo el resultado en un objeto llamado "gov_ideology"
gov_ideology <- herre

# Guardo resultado final en formato "csv"
save(gov_ideology, file = "gov_ideology.RData")