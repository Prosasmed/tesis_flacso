# Tesis doctoral
# Pruebas para hipotesis 2

# Pedro I. Rosas-Medina
# Doctorado de Investigacion en Ciencias Sociales
# Ciencia Politica
# Seminario de Investigacion en Economia Politica Comparada
# FLACSO Mexico

# ---------- Configuracion ----------
# Working directory
setwd("/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/00.Data")

# Packages (libraries)
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
library(tidyplots)

# ---------- Limpieza final ----------
# Data
load("h1.RData")
load("h2.RData")
load("h3.RData")

# Preparo datos en formato panel y establezco id individuo-tiempo
h1 <- pdata.frame(h1,
                  index = c("country", "year"))
h2 <- pdata.frame(h2,
                  index = c("country", "year"))
h3 <- pdata.frame(h3,
                  index = c("country", "year"))


# Verifico resultado panel
class(h1)
class(h2)
class(h3)
is.pbalanced(h1)
is.pbalanced(h2)
is.pbalanced(h3)

# Modelos de dos vias
h1_tw2 <- plm(exp_w1 ~ VP_mean + KOFGIdj + VP_mean * KOFGIdj +
                pop_w2 + pop_w1_ln + eco_w4 + eco_w6 + eco_w16,
              data = h1,
              model = "within",
              effect = "twoways")

h2_tw2 <- plm(basic ~ VP_mean + KOFFiGIdj + VP_mean * KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = "within",
              effect = "twoways")

h3_tw2 <- plm(exp_w1 ~ VP_mean + KOFGI + v2edteunionindp_ord +
                (VP_mean * KOFGI * v2edteunionindp_ord) +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h1_tw2)
summary(h2_tw2)
summary(h3_tw2)

# Errores estandar robustos
coeftest(h1_tw2, vcov = vcovHC(h1_tw2, type = "HC1"))
coeftest(h2_tw2, vcov = vcovHC(h2_tw2, type = "HC1"))
coeftest(h3_tw2, vcov = vcovHC(h3_tw2, type = "HC1"))

# ---------- Indagacion 1 para estudio cualitativo ----------
# Nivel de globalizacion economica, por pais
# Cada punto representa una observacion pais-año
h2 |>
  arrange(country, year) |>
  ggplot(aes(x = country, y = KOFGI,
             colour = country, group = country)) +
  geom_point() +
  geom_hline(yintercept = 50, linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_hline(yintercept = 43, linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  coord_flip() +
  theme_apa(legend.pos = "bottom", legend.font.size = 10, 
            remove.y.gridlines = FALSE)

# Serie de tiempo agrupada de cinco en cinco paises
h2 |>
  filter(country %in% c("Argentina", "Bolivia", "Brazil", "Chile", 
                        "Colombia")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = KOFGI,
             colour = country, group = country)) +
  geom_line(aes(linetype = country)) +
  geom_hline(yintercept = 50, linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_vline(xintercept = "1990", linetype = "dashed", size = 1, alpha = 0.5) +
  geom_vline(xintercept = "2000", linetype = "dashed", size = 1, alpha = 0.5) +
  theme_apa(legend.pos = "bottom", legend.font.size = 10)

h2 |>
  filter(country %in% c("Costa Rica", "Ecuador", "El Salvador", "Guatemala", 
                        "Honduras")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = KOFGI,
             colour = country, group = country)) +
  geom_line(aes(linetype = country)) +
  geom_hline(yintercept = 50, linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_vline(xintercept = "1990", linetype = "dashed", size = 1, alpha = 0.5) +
  geom_vline(xintercept = "2000", linetype = "dashed", size = 1, alpha = 0.5) +
  theme_apa(legend.pos = "bottom", legend.font.size = 10)

# Chile, Argentina, o Costa Rica hasta este punto

h2 |>
  filter(country %in% c("Mexico", "Nicaragua", "Panama", "Paraguay", 
                        "Peru")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = KOFGI,
             colour = country, group = country)) +
  geom_line(aes(linetype = country)) +
  geom_hline(yintercept = 50, linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_vline(xintercept = "1990", linetype = "dashed", size = 1, alpha = 0.5) +
  geom_vline(xintercept = "2000", linetype = "dashed", size = 1, alpha = 0.5) +
  theme_apa(legend.pos = "bottom", legend.font.size = 10)

h2 |>
  filter(country %in% c("República Dominicana", "Uruguay", "Venezuela")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = KOFGI,
             colour = country, group = country)) +
  geom_line(aes(linetype = country)) +
  geom_hline(yintercept = 50, linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_vline(xintercept = "1990", linetype = "dashed", size = 1, alpha = 0.5) +
  geom_vline(xintercept = "2000", linetype = "dashed", size = 1, alpha = 0.5) +
  theme_apa(legend.pos = "bottom", legend.font.size = 10)

h2 |>
  filter(country %in% c("Argentina", "Brazil", "Mexico", "Panama", 
                        "Paraguay")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = KOFGI,
             colour = country, group = country)) +
  geom_line(aes(linetype = country)) +
  geom_hline(yintercept = 50, linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_vline(xintercept = "1990", linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_vline(xintercept = "2000", linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  theme_apa(legend.pos = "bottom", legend.font.size = 10)

# Panama y Mexico parecerian ser los casos a retomar. Parten de niveles muy simi
# lares de globalizacion, pero Panama abre su economia por encima del 50% antes
# que Mexico, en aproximadamente cinco años.

# Otra combinacion podria ser Paraguay. Sin embargo, Paraguay parte de niveles
# de globalizacion muy por debajo de Panama.

h2 |>
  filter(country %in% c("Mexico", "Panama")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = KOFGI,
             colour = country, group = country)) +
  geom_line(aes(linetype = country)) +
  geom_hline(yintercept = 50, linetype = "dashed", size = 0.5, alpha = 0.5,
             colour = "grey55") +
  geom_vline(xintercept = "1990", linetype = "dashed", size = 0.5, alpha = 0.5,
             colour = "grey55") +
  geom_vline(xintercept = "2000", linetype = "dashed", size = 0.5, alpha = 0.5,
             colour = "grey55") +
  ggtitle("Evolución de la globalización:",
          subtitle = "México y Panamá, 1970-2018") +
  labs(x = NULL, y = "Nivel de globalización",
       caption = "Fuente: elaboración con base en datos del Índice KOF.") +
  theme_apa(legend.pos = "bottom", legend.font.size = 10) +
  theme(plot.title = element_text(size = 12, colour = "black"),
        plot.subtitle = element_text(size = 12, colour = "black"),
        plot.caption = element_text(size = 8, colour = "black", hjust = 0),
        axis.title.x = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1,
                                   colour = "black"),
        axis.text.y = element_text(size = 8, colour = "black"))


# Ahora veamos el gasto de estos paises
# Educacion (total)
h1 |>
  filter(country %in% c("Brazil", "Mexico", "Panama")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = exp_w1,
             colour = country, group = country)) +
  geom_line(aes(linetype = country)) +
  geom_vline(xintercept = "1990", linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_vline(xintercept = "2000", linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  theme_apa(legend.pos = "bottom", legend.font.size = 10)

# Educacion basica
h2 |>
  filter(country %in% c("Brazil", "Mexico", "Panama")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = basic,
             colour = country, group = country)) +
  geom_line(aes(linetype = country)) +
  geom_vline(xintercept = "1990", linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_vline(xintercept = "2000", linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  theme_apa(legend.pos = "bottom", legend.font.size = 10)

h2 |>
  filter(year %in% c("2011":"2018")) |>
  group_by(year) |>
  summarise(mean = mean(basic, na.rm = TRUE))

# Educacion superior
h2 |>
  filter(country %in% c("Brazil", "Mexico", "Panama")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = tertiary,
             colour = country, group = country)) +
  geom_line(aes(linetype = country)) +
  geom_vline(xintercept = "1990", linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  geom_vline(xintercept = "2000", linetype = "dashed", size = 1, alpha = 0.5,
             colour = "grey40") +
  theme_apa(legend.pos = "bottom", legend.font.size = 10)

# Ahora veamos los gobiernos
h1 |>
  filter(country %in% c("Mexico", "Panama")) |>
  filter(year %in% c("1990", "1991", "1992", "1993", "1994", "1995", "1996",
                     "1997", "1998", "1999", "2000", "2001", "2002", "2003",
                     "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                     "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                     "2018")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = Hrre_hogR, colour = country, group = country)) +
  geom_point() +
  geom_jitter(height = 0.05) +
  scale_color_manual(values = c("Mexico" = "#006341", "Panama" = "#DA121A")) +
  ggtitle("Posición ideológica gubernamental:",
          subtitle = "México y Panamá, 1990-2018") +
  labs(x = NULL, y = "Posición ideológica",
       caption = "Fuente: elaboración propia con datos de Herre (2023).") +
  theme_apa(legend.pos = "bottom", remove.y.gridlines = FALSE) +
  theme(plot.title = element_text(size = 12, colour = "black"),
        plot.subtitle = element_text(size = 12, colour = "black"),
        plot.caption = element_text(size = 8, colour = "black", hjust = 0),
        axis.title.x = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1,
                                   colour = "black"),
        axis.text.y = element_text(size = 8, colour = "black"))


# ---------- Indagacion 2 para estudio cualitativo ----------
h1 |>
  filter(year %in% c("1990", "1991", "1992", "1993", "1994", "1995", "1996", 
                     "1997", "1998", "1999")) |>
  filter(Hrre_hogR == "1") |>
  arrange(country, year) |>
  ggplot(aes(x = country, y = KOFGIdj, colour = country, group = country)) +
  geom_point() +
  geom_hline(yintercept = 50,
             linetype = "dashed",
             size = 1,
             alpha = 0.5,
             colour = "black") +
  theme_apa(legend.pos = "bottom") +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

h1 |>
  filter(year %in% c("1990", "1991", "1992", "1993", "1994", "1995", "1996", 
                     "1997", "1998", "1999")) |>
  filter(Hrre_hogR == "1") |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = KOFGIdj, group = country)) +
  geom_line() +
  facet_wrap(~ country)

h3 |>
  filter(year %in% c("1990", "1991", "1992", "1993", "1994", "1995", "1996", 
                     "1997", "1998", "1999", "2000", "2001", "2002", "2003",
                     "2004", "2005", "2006", "2007", "2008", "2009", "2010")) |>
  filter(Hrre_hogR == "1") |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = v2edteunionindp_mean, group = country)) +
  geom_line() +
  facet_wrap(~ country)

h3 |>
  filter(!country %in% c("Guatemala", "Nicaragua")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = KOFTrGIdj, group = country)) +
  geom_vline(xintercept = "1990", linetype = "dashed", colour = "black") +
  geom_line() +
  facet_wrap(~ country)

h3 |>
  filter(!country %in% c("Guatemala", "Nicaragua")) |>
  arrange(country, year) |>
  ggplot(aes(x = year, y = v2edteunionindp_mean, group = country)) +
  geom_vline(xintercept = "1990", linetype = "dashed", colour = "black") +
  geom_line() +
  facet_wrap(~ country) +
  ggtitle("Independencia política sindical:",
          subtitle = "Latinoamérica, 1970-2018") +
  labs(x = NULL, y = "Nivel de independencia política") +
  scale_x_discrete(breaks = c("1970", "1980", "1990", "2000", "2010", "2018")) +
  theme_apa(facet.title.size = 10) +
  theme(axis.title.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 6, colour = "black"))



