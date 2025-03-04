# Government ideology data frame

# Pedro I. Rosas-Medina
# PhD Candidate in Social Sciences, with focus in Political Science
# FLACSO Mexico

# ---------- Preamble ----------
# Working directory
setwd("/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/00.Data")

# Libraries - Packages
library(tidyverse)
library(readxl)

# Data
herre <- read.csv("herre_ideology.csv")
byg <- read.csv("baker_greene.csv")
vparty <- read_xlsx("V-Party_LATAM_clean_duplicated.xlsx")

# ---------- Cleaning ----------
# Equal observations for Herre, taking byg and vparty
herre <-
  herre %>%
  filter(region == "Latin America and Caribbean",
         country_name %in% c("Argentina", "Bolivia", "Brazil", "Chile", 
                             "Colombia", "Costa Rica", "Dominican Republic",
                             "Ecuador", "El Salvador", "Guatemala", "Honduras",
                             "Mexico", "Nicaragua", "Panama", "Paraguay",
                             "Peru", "Uruguay", "Venezuela"),
         year >= 1970 & year <= 2018)

# I create a data frame from scratch
# First, the name of the countries included in the panel
countries <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
               "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", 
               "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", 
               "Paraguay", "Peru", "Uruguay", "Venezuela")

# Now, I create a vector of years, from 1970 to 2018
years <- 1970:2018

# I get the panel
ideology <- expand.grid(country_name = countries, year = years)

# I arrange by country_name and year
ideology <-
  ideology %>%
  arrange(country_name, year)

# 
# summary(vparty$v2pariglef, na.rm = TRUE)
# summary(vparty$v2pariglef_mean, na.rm = TRUE) # De 0 a 6 (deberia ir...) 
# summary(vparty$v2pariglef_ord, na.rm = TRUE) # De 0 a 6 (deberia ir...)

# df <- readRDS("V-Dem-CPD-Party-V2.rds")
# summary(df$v2pariglef, na.rm = TRUE)
# summary(df$v2pariglef_mean, na.rm = TRUE) # De 0 a 6
# summary(df$v2pariglef_ord, na.rm = TRUE) # De 0 a 6

# Guardo algunas variables de interes en "ideology"
ideology$VP_wincan <- vparty$win_cand
ideology$Hrre_hogN <- herre$hog
ideology$Hrre_hog <- herre$hog_ideology
ideology$Hrre_hogR <- herre$hog_right
ideology$Hrre_hogL <- herre$hog_left
ideology$Hrre_hogC <- herre$hog_center
ideology$Hrre_hogNI <- herre$hog_noideo
ideology$Hrre_hogInf <- herre$hog_noinfo
ideology$Hrre_hogMiss <- herre$hog_ideomiss

ideology$VP_ideo <- vparty$v2pariglef
ideology$VP_mean <- vparty$v2pariglef_mean
ideology$VP_ord <- vparty$v2pariglef_ord

ideology$byg_Ideo <- vparty$byg_ideology

# Estandarizo "VPmean", y "VP_ord" para que simule a "byg_Ideo"
ideology <-
  ideology %>%
  mutate(VP_mean2 = (VP_mean * 20) / 6)

ideology <-
  ideology %>%
  mutate(VP_ord2 = (VP_ord * 20) / 6)

# Preparo variable perdida en Herre: hog_ideology_num
ideology <-
  ideology %>%
  mutate(Hrre_hog_num = factor(case_when(Hrre_hog == "rightist" ~ 0,
                                  Hrre_hog == "leftist" ~ 1,
                                  Hrre_hog == "centrist" ~ 2,
                                  Hrre_hog == "no information" ~ 3)),
         .after = 5)

# Genero variables dummy sobre posicion ideologia del gobierno segun ByG
ideology <- ideology %>%
  mutate(
    byg_IdeoRight = case_when(
      byg_Ideo > 15 ~ 1,
      TRUE ~ 0
    ),
    byg_IdeoCRight = case_when(
      byg_Ideo > 10 & byg_Ideo <= 15 ~ 1,
      TRUE ~ 0
    ),
    byg_IdeoCLeft = case_when(
      byg_Ideo > 5 & byg_Ideo <= 10 ~ 1,
      TRUE ~ 0
    ),
    byg_IdeoLeft = case_when(
      byg_Ideo > 0 & byg_Ideo <= 5 ~ 1,
      TRUE ~ 0
    )
  )

# Genero variables dummy sobre posicion ideologia del gobierno segun VParty
ideology <- ideology %>%
  mutate(
    VP_Right = case_when(
      VP_mean2 > 15 ~ 1,
      TRUE ~ 0
    ),
    VP_CRight = case_when(
      VP_mean2 > 10 & VP_mean2 <= 15 ~ 1,
      TRUE ~ 0
    ),
    VP_CLeft = case_when(
      VP_mean2 > 5 & VP_mean2 <= 10 ~ 1,
      TRUE ~ 0
    ),
    VP_Left = case_when(
      VP_mean2 > 0 & VP_mean2 <= 5 ~ 1,
      TRUE ~ 0
    )
  )

gov_ideology <- ideology
save(gov_ideology, file = "gov_ideology.RData")

