# Tesis doctoral
# Pruebas para hipotesis 2

# Pedro I. Rosas-Medina
# Doctorado de Investigacion en Ciencias Sociales
# Ciencia Politica
# Seminario de Investigacion en Economia Politica Comparada
# FLACSO Mexico

# ---------- Configuracion ----------
# Working directory
setwd('/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/00.Data')

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

# ---------- Limpieza final ----------
# Data
load("h2.RData")

# Preparo datos en formato panel y establezco id individuo-tiempo
h2 <- pdata.frame(h2,
                  index = c("country", "year"))

# Verifico resultado panel
class(h2)
is.pbalanced(h2)

# ---------- H2: Educacion superior ----------
# ---------- Variable independiente ideologia continua ----------
# Principales variables independientes, con controles, sin interaccion
# Efectos individuales nivel pais
h2b_in1 <- plm(tertiary ~ VP_mean + KOFEcGI +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "individual")

# Efectos fijos nivel pais y año
h2b_tw1 <- plm(tertiary ~ VP_mean + KOFTrGI +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "twoways")

# Resumenes
summary(h2b_in1)
summary(h2b_tw1)

# Errores estandar robustos
coeftest(h2b_in1,
         vcov = vcovHC(h2b_in1, 
                       type = "HC1"))
coeftest(h2b_tw1,
         vcov = vcovHC(h2b_tw1, 
                       type = "HC1"))

# ---------------------------------------------------------------------------- #
# Principales variables independientes, con controles e interaccion
# Efectos individuales nivel pais
h2b_in2 <- plm(tertiary ~ VP_mean + KOFTrGIdj + VP_mean * KOFTrGIdj +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "individual")

# Efectos fijos nivel pais y año
h2b_tw2 <- plm(tertiary ~ VP_mean + KOFTrGIdj + VP_mean * KOFTrGIdj +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "twoways")

# Resumenes
summary(h2b_in2)
summary(h2b_tw2)

# Errores estandar robustos
coeftest(h2b_in2,
         vcov = vcovHC(h2b_in2,
                       type = "HC1"))
coeftest(h2b_tw2,
         vcov = vcovHC(h2b_tw2,
                       type = "HC1"))

# ---------- Variable ideologia dicotomica (1 = right, 0 = left) ----------
# Principales variables independientes, con controles, sin interaccion
# Efectos individuales nivel pais
colnames(h2)
h2b_in3 <- plm(tertiary ~ Hrre_hogR + KOFTrGI +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "individual")

# Efectos nivel pais y tiempo
h2b_tw3 <- plm(tertiary ~ Hrre_hogR + KOFTrGI +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "twoways")

# Resumenes
summary(h2b_in3)
summary(h2b_tw3)

# Errores estandar robustos
coeftest(h2b_in3,
         vcov = vcovHC(h2b_in3,
                       type = "HC1"))
coeftest(h2b_tw3,
         vcov = vcovHC(h2b_tw3,
                       type = "HC1"))

# ---------------------------------------------------------------------------- #
# Principales variables independientes, con controles e interaccion
# Efectos individuales nivel pais
h2b_in4 <- plm(tertiary ~ Hrre_hogR + KOFTrGIdj + Hrre_hogR * KOFTrGIdj +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "individual")

# Efectos nivel pais y tiempo
h2b_tw4 <- plm(tertiary ~ Hrre_hogR + KOFGI + Hrre_hogR * KOFGI +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "twoways")

# Resumenes
summary(h2b_in4)
summary(h2b_tw4)

# Errores estandar robustos
coeftest(h2b_in4,
         vcov = vcovHC(h2b_in4,
                       type = "HC1"))
coeftest(h2b_tw4,
         vcov = vcovHC(h2b_tw4,
                       type = "HC1"))

# ---------- Efectos marginales ----------
# A continuacion se calculan efectos marginales de la posicion ideologica de los
# gobiernos, sobre el gasto publico educativo en el subsistema de enseñanza su-
# perior, bajo diferentes niveles de globalizacion. Particularmente, se calculan
# efectos marginales condicionales para el rango de valores posibles de Z (es de
# cir, por debajo, o por encima de los valores observados).

# Establezco un rango de valores posibles de Z, y una secuencia de estos
koffgi_values <- seq(from = 0, to = 100, by = 0.20)

# Calculo efectos marginales promedio para valores especificos de Z
summary(h2b_tw2)
c_me_unobserved_avg <- marginaleffects::avg_slopes(
  model = h2b_tw2,
  newdata = datagrid(KOFTrGI = koffgi_values),
  variables = "VP_mean",
  by = "KOFTrGI",
  conf_level = 0.90
)

# Grafica
cme_h2btw2 <-
  c_me_unobserved_avg %>%
  ggplot(aes(x = KOFTrGI, y = estimate)) +
  geom_hline(yintercept = 0.00,
             linetype = "dashed",
             colour = "grey25") +
  geom_line(color = "dodgerblue2",
            linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.2,
              colour = "black",
              linetype = "dotdash",
              fill = "darkgrey",
              linewidth = 0.5) +
  ggtitle(NULL) +
  labs(x = "Nivel de globalización",
       y = "Efecto de la Ideología") +
  theme_apa(x.font.size = 10,
            y.font.size = 10) +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(color = "black",
                                   size = 8),
        axis.text.y = element_text(color = "black",
                                   size = 8))

cme_h2btw2

# Para el analisis podria ser interesante calcular el nivel de globalizacion (Z)
# donde el efecto marginal de X sobre Y es neutro, es decir: ME(X | Z) = 0.

# Este punto puede ser encontrado resolviendo el punto donde la derivada parcial
# de X sobre Y dado Z es igual a 0, es decir:

# \beta_{X} + \beta_{XZ}Z_{it} = 0

# Resolviendo para $Z_{it}$ tenemos:

# Z_{it} = -\frac{\beta_{X}}{\beta_{XZ}},
# Z_{it} = -\frac{0.09128}{0.0016475} = 55.40

# ---------------------------------------------------------------------------- #
# Los efectos marginales condicionales, no obstante, se hacen en funcion de la
# variable de ideologia gubernamental continua. Ahora, se efectua un analisis si
# milar, pero para la variable categorica de ideologia (1 = gobiernos de derecha
# y 0 = gobiernos de izquierda)

# La grafica permitira visualizar los valores de gasto publico educativo pronos-
# ticados para el subsistema de enseñanza superior, para cada tipo de gobierno,
# a diferentes niveles de globalizacion.

# Inicio estableciendo un conjunto de valores en secuencia para la variable de
# globalizacion (independientemente si son, o no observados, para el conjunto de
# datos)
summary(h2b_tw4)
koffgi_values <- seq(from = 0, to = 100, by = 0.20)

# Calculo los valores pronosticados de gasto en el subsistema superior
c_me <- marginaleffects::predictions(
  h2b_tw4,
  newdata = datagrid(Hrre_hogR = c(0, 1),
                     KOFGI = koffgi_values)
)

# Grafica
c_meh2btw4 <-
  c_me %>%
  ggplot(aes(x = KOFGI, y = estimate,
             group = Hrre_hogR,
             colour = Hrre_hogR,
             fill = Hrre_hogR)) +
  geom_line(aes(linetype = Hrre_hogR),
            linewidth = 0.7) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high),
              alpha = 0.05, 
              colour = "grey100", 
              linewidth = 0) + 
  scale_colour_manual(values = c("dodgerblue3", "firebrick"),
                      labels = c("Izquierda", "Derecha")) + 
  scale_fill_manual(values = c("dodgerblue3", "firebrick"),
                    labels = c("Izquierda", "Derecha")) + 
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Izquierda", "Derecha")) + 
  ggtitle(NULL) +
  labs(x = "Nivel de globalización",
       y = "Gasto público educativo total (% PIB)",
       colour = "Tipo de gobierno", 
       fill = "Tipo de gobierno", 
       linetype = "Tipo de gobierno") + 
  theme_apa(legend.pos = "bottom",
            legend.font.size = 10,
            x.font.size = 10,
            y.font.size = 10) +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(color = "black",
                                   size = 8),
        axis.text.y = element_text(color = "black",
                                   size = 8))

c_meh2btw4

# ---------- Anexos: condicion de simetria ----------
# Siguiendo el ejercicio elaborado antes con el efecto marginal de X sobre Y da-
# do Z, se genera ahora el grafico de efectos marginales de Z sobre Y, dado X,
# pero para todo el rango de valores posibles en X.

# Establezco un rango de valores posibles de X, y una secuencia de estos
VP_meanValues <- seq(from = 1, to = 20, by = 1)

# Calculo efectos marginales promedio para valores especificos de X
c_me2_unobserved_avg <- marginaleffects::avg_slopes(
  model = h2b_tw2,
  newdata = datagrid(VP_mean = VP_meanValues),
  variables = "KOFTrGI",
  by = "VP_mean",
  conf_level = 0.95
)

# Grafico de efectos marginales condicionales para la condicion de simetria
cme2_h2btw2 <-
  c_me2_unobserved_avg %>%
  ggplot(aes(x = VP_mean, y = estimate)) +
  geom_hline(yintercept = 0.00,
             linetype = "dashed",
             colour = "grey25") +
  geom_line(color = "dodgerblue2",
            linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.2,
              colour = "black",
              linetype = "dotdash",
              fill = "darkgrey",
              linewidth = 0.5) +
  ggtitle(NULL) +
  labs(x = "Ideología gubernamental",
       y = "Efecto de la globalización") +
  theme_apa(x.font.size = 10,
            y.font.size = 10) +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(color = "black",
                                   size = 8),
        axis.text.y = element_text(color = "black",
                                   size = 8))

cme2_h2btw2

# ---------- Resultados de analisis ----------
# Graficos de tesis
ggsave(
  "cme_h2d.pdf",
  cme_h2btw2,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 13.67,
  height = 9.22,
  units = "cm",
  dpi = "print"
)

ggsave(
  "cme_h2e.pdf",
  c_meh2btw4,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 13.67,
  height = 9.22,
  units = "cm",
  dpi = "print"
)

ggsave(
  "cme_h2f.pdf",
  cme2_h2btw2,
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 13.67,
  height = 9.22,
  units = "cm",
  dpi = "print"
)









