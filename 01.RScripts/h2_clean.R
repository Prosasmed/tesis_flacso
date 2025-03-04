# Tesis doctoral
# Limpieza de base de datos para hipotesis 1

# Pedro I. Rosas-Medina
# Doctorado de Investigacion en Ciencias Sociales
# Ciencia Politica
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

# ---------- Limpieza h2 ----------
# Datos base
df1 <- read_xlsx("tesis_long.xlsx")
load("gov_ideology.RData")

# Guardo variables de "gov_ideology" dentro de "df1"
df1$Hrre_hog <- gov_ideology$Hrre_hog
df1$Hrre_hog_num <- gov_ideology$Hrre_hog_num
df1$Hrre_hogR <- gov_ideology$Hrre_hogR
df1$Hrre_hogL <- gov_ideology$Hrre_hogL
df1$Hrre_hogC <- gov_ideology$Hrre_hogC
df1$VP_mean <- gov_ideology$VP_mean2

# Guardo en un nuevo df (h2) las variables de interes
colnames(df1)
h2 <-
  df1 %>%
  select(country:year, polity2, basic, tertiary, VP_mean, Hrre_hogR,
         KOFGI:pop_w10, eco_w1:eco_w2, eco_w4, eco_w6, eco_w14:eco_w16)

# Guardo transformaciones de variables
h2 <-
  h2 %>%
  mutate(pop_w1_ln = log(pop_w1),
         eco_w2_ln = log(eco_w2),
         eco_w2_ln2 = log(eco_w2)^2,
         eco_w4_ln= log(eco_w4),
         eco_w4_ln2 = log(eco_w4)^2,
         eco_w14_ln = if_else(eco_w14 > 0, log(eco_w14), NA))

# Me quedo unicamente con variables a utilizar
colnames(h2)
h2 <-
  h2 %>%
  select(country:pop_w1, pop_w1_ln, pop_w2, pop_w9:pop_w10, eco_w1:eco_w14_ln)

# Cambio formato de variables
h2 <-
  h2 %>%
  mutate(country = as.character(country),
         scode = as.character(scode),
         ccode = as.factor(ccode),
         Hrre_hogR = as.factor(Hrre_hogR),
         basic = as.numeric(basic),
         tertiary = as.numeric(tertiary),
         VP_mean = as.numeric(VP_mean))

# ---------- Reacomodo final ----------
colnames(h2)
h2 <-
  h2 %>%
  select(country:Hrre_hogR, KOFGI:KOFFiGIdj, pop_w1_ln, pop_w2, eco_w4, eco_w6, 
         eco_w16)

# ---------- Correccion de dos variables ----------
# Debido a un problema de calculo en las variables "basic", y "tertiary", retomo
# calculos correctos.
load("total_expenditures.RData")

df2 <-
  df2 |>
  select(basic, tertiary)

# Verifico que "basic" en "h2", sea igual a "basic" en "df2"
identical(df2$basic, h2$basic)

# Hago lo mismo para "tertiary"
identical(df2$tertiary, h2$tertiary)

# RESULTADO:
# La variable "basic" en "df2", es igual a "basic" en "h2". Sin embargo, la co-
# lumna "tertiary" en las dos bases de datos, no son iguales. Retomo la de "df2"
# que estoy seguro es correcta
h2$tertiary <- df2$tertiary

# ---------- Resultado final ----------
# Formato .csv
write.csv(h2,
          "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/00.Data/h2.csv",
          row.names = FALSE)

# Formato .RData
save(h2, file = "h2.RData")
