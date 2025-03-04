# Tesis doctoral
# Limpieza de base de datos para hipotesis 3

# Pedro I. Rosas-Medina
# Doctorado de Investigacion en Ciencias Sociales
# Concentracion en Ciencia Politica
# Seminario de Investigacion en Economia Politica Comparada
# FLACSO Mexico

# ---------- Configuracion del entorno de trabajo ----------
# Directorio de trabajo
setwd("/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/00.Data")

# Paqueterias
library(tidyverse)
library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(ggstance)
library(jtools)
library(marginaleffects)
library(ggeffects)
library(texreg)
library(devEMF)
library(ggpubr)
library(gtable)
library(grid)

# ---------- Limpieza h3 ----------
# Datos base
df1 <- read_xlsx("tesis_long.xlsx")
load("gov_ideology.RData")
v_indoc <- read.csv("v_indoc.csv")

# Coincidencias de paises entre "v_indoc"-"df1" y "v_indoc"-"gov_ideology"
coinc_country1 <- intersect(v_indoc$country_name, df1$country)
coinc_country2 <- intersect(v_indoc$country_name, gov_ideology$country_name)

# Me quedo unicamente con paises coincidentes entre "v_indoc" y "df1"
df1 <-
  df1 %>%
  filter(country %in% coinc_country1)

# Me quedo unicamente con paises coincidentes entre "v_indoc" y "gov_ideology"
gov_ideology <-
  gov_ideology %>%
  filter(country_name %in% coinc_country2)

# Me quedo con variables de interes de "v_indoc"
v_indoc <-
  v_indoc %>%
  select(country_name:year, v2edteunion_mean, v2edteunion_ord,
         v2edteunionindp_mean, v2edteunionindp_ord)

# Me quedo con variables de interes de "gov_ideology"
gov_ideology <-
  gov_ideology %>%
  select(country_name:year, VP_mean2, Hrre_hog:Hrre_hogC)

# Guardo variables de interes desde "gov_ideology" en "df1"
df1 <-
  df1 %>%
  mutate(
    Hrre_hog = gov_ideology$Hrre_hog,
    Hrre_hogR = gov_ideology$Hrre_hogR,
    Hrre_hogL = gov_ideology$Hrre_hogL,
    VP_mean = gov_ideology$VP_mean2
  )

# Guardo variables de interes desde "v_indoc" en "df1"
colnames(v_indoc)
df1 <-
  df1 %>%
  mutate(
    v2edteunion_mean = v_indoc$v2edteunion_mean,
    v2edteunion_ord = v_indoc$v2edteunion_ord,
    v2edteunionindp_mean = v_indoc$v2edteunionindp_mean,
    v2edteunionindp_ord = v_indoc$v2edteunionindp_ord
  )

# Verifico columnas para eliminar aquellas innecesarias
colnames(df1)

# En un nuevo objeto, "h3", guardo las variables de interes desde "df1"
h3 <-
  df1 %>%
  select(country:year, polity2, exp_w1, basic, tertiary, VP_mean, Hrre_hogR,
         KOFGI:KOFFiGIdj, v2edteunion_mean:v2edteunionindp_ord, pop_w1:pop_w2,
         pop_w9:pop_w10, eco_w1:eco_w2, eco_w4, eco_w6, eco_w14:eco_w16)

# Transformo variables necesarias para analisis
h3 <-
  h3 %>%
  mutate(pop_w1_ln = log(pop_w1),
         eco_w2_ln = log(eco_w2),
         eco_w2_ln2 = log(eco_w2)^2,
         eco_w4_ln = log(eco_w4),
         eco_w4_ln2 = log(eco_w4)^2,
         eco_w14_ln = if_else(eco_w14 > 0, log(eco_w14), NA))

# Ultimo analisis para quedarme unicamente con variables necesarias
colnames(h3)
h3 <-
  h3 %>%
  select(country:pop_w1, pop_w1_ln, pop_w2, pop_w9:pop_w10, eco_w1:eco_w14_ln)

# Cambio formato de variables especificas
h3 <-
  h3 %>%
  mutate(country = as.character(country),
         scode = as.character(scode),
         ccode = as.factor(ccode),
         Hrre_hogR = as.factor(Hrre_hogR),
         basic = as.numeric(basic),
         tertiary = as.numeric(tertiary),
         VP_mean = as.numeric(VP_mean))

# Reacomodo final
h3 <-
  h3 %>%
  select(country:v2edteunionindp_ord, pop_w1_ln, pop_w2, eco_w4, eco_w6, 
         eco_w16)

# ---------- Guardo en .csv y RData el resultado final ----------
# Formato .csv
write.csv(h3,
          "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/00.Data/h3.csv",
          row.names = FALSE)

# Formato .RData
save(h3, file = "h3.RData")