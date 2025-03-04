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

# ---------- H2: Educacion basica ----------
# ---------- Variable independiente ideologia continua ----------
# Principales variables independientes, con controles, sin interaccion
# Efectos individuales nivel pais
h2_in1 <- plm(basic ~ VP_mean + KOFGIdf +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              id = c("country", "year"),
              data = h2,
              model = "within",
              effect = "individual")

# Efectos de dos vias
h2_tw1 <- plm(basic ~ VP_mean + KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              id = c("country", "year"),
              data = h2,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h2_in1)
summary(h2_tw1)

# Errores estandar robustos
coeftest(h2_in1,
         vcov = vcovHC(h2_in1,
                       type = "HC1"))
coeftest(h2_tw1,
         vcov = vcovHC(h2_tw1,
                       type = "HC1"))

# ---------------------------------------------------------------------------- #
# Principales variables independientes, con controles e interacciones
# Efectos individuales nivel pais
h2_in2 <- plm(basic ~ VP_mean + KOFFiGIdj + VP_mean * KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = "within",
              effect = "individual")

# Efectos nivel pais y tiempo
h2_tw2 <- plm(basic ~ VP_mean + KOFFiGIdj + VP_mean * KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h2_in2)
summary(h2_tw2)

# Errores estandar robustos
coeftest(h2_in2,
         vcov = vcovHC(h2_in2,
                       type = "HC1"))
coeftest(h2_tw2,
         vcov = vcovHC(h2_tw2,
                       type = "HC1"))

# ---------- Variable ideologia dicotomica (1 = right, 0 = left) ----------
# Principales variables independientes, con controles, sin interaccion
# Efectos individuales nivel pais
h2_in3 <- plm(basic ~ Hrre_hogR + KOFGIdf +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              id = c("country", "year"),
              data = h2,
              model = "within",
              effect = "individual")

# Efectos de dos vias
h2_tw3 <- plm(basic ~ Hrre_hogR + KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              id = c("country", "year"),
              data = h2,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h2_in3)
summary(h2_tw3)

# Errores estandar robustos
coeftest(h2_in3,
         vcov = vcovHC(h2_in3,
                       type = "HC1"))
coeftest(h2_tw3,
         vcov = vcovHC(h2_tw3,
                       type = "HC1"))

# ---------------------------------------------------------------------------- #
# Principales variables independientes, con controles e interacciones
# Efectos individuales nivel pais
h2_in4 <- plm(basic ~ Hrre_hogR + KOFTrGIdj + Hrre_hogR * KOFTrGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = "within",
              effect = "individual")

# Efectos nivel pais y tiempo
colnames(h2)
h2_tw4 <- plm(basic ~ Hrre_hogR + KOFFiGIdj + Hrre_hogR * KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h2_in4)
summary(h2_tw4)

# Errores estandar robustos
coeftest(h2_in4,
         vcov = vcovHC(h2_in4,
                       type = "HC1"))
coeftest(h2_tw4,
         vcov = vcovHC(h2_tw4,
                       type = "HC1"))

# ---------- Efectos marginales ----------
# A continuacion se calculan efectos marginales de la posicion ideologica de los
# gobiernos sobre el gasto publico educativo total, bajo diferentes niveles de
# globalizacion. Particularmente, se calculan efectos marginales condicionales
# para el rango de valores posibles de Z (es decir, por debajo, o por encima de
# los valores observados).

# Establezco un rango de valores posibles de Z y una secuencia de estos
koffgi_values <- seq(from = 0, to = 100, by = 0.20)

# Calculo efectos marginales promedio para valores especificos de Z
c_me_unobserved_avg <- marginaleffects::avg_slopes(
  model = h2_tw2,
  newdata = datagrid(KOFFiGIdj = koffgi_values),
  variables = "VP_mean",
  by = "KOFFiGIdj",
  vcov = "HC1",
  conf_level = 0.90
)

# Grafica
cme_h2tw2 <-
  c_me_unobserved_avg %>%
  ggplot(aes(x = KOFFiGIdj, y = estimate)) +
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
cme_h2tw2  

# Para el analisis podria ser interesante calcular el nivel de globalizacion (Z)
# donde el efecto marginal de X sobre Y es neutro, es decir: ME(X | Z) = 0.

# Este punto puede ser encontrado resolviendo el punto donde la derivada parcial
# de X sobre Y dado Z es igual a cero, es decir:

# \beta_{X} + \beta_{XZ}Z_{it} = 0

# Resolviendo para $Z_{it}$ tenemos:

# Z_{it} = -\frac{\beta_{X}}{\beta_{XZ}},
# Z_{it} = -\frac{-0.061962}{0.0012401} = 50.03

# ---------------------------------------------------------------------------- #
# Los efectos marginales condicionales, no obstante, se hacen en función de la
# variable de ideologia gubernamental continua. Ahora, se efectua un analisis
# similar, pero para la variable categorica de ideologia (1 = gobiernos de dere-
# cha, y 0 = gobiernos de izquierda).

# La grafica permite visualizar, particularmente, los valores de gasto publico
# educativo pronosticados (para el subsistema de enseñanza basico), condicionado
# por el tipo de gobierno (izquierdas y derechas), a diferentes niveles de globa
# lizacion.

# Inicio estableciendo un conjunto de valores en secuencia para la variable de
# globalizacion (independientemente si son, o no observados, para el conjunto de
# datos)
koffgi_values <- seq(from = 0, to = 100, by = 0.20)

# Calculo los valores pronosticados de gasto en el subsistema basico
c_me <- marginaleffects::predictions(
  h2_tw4,
  newdata = datagrid(Hrre_hogR = c(0, 1),
                     KOFFiGIdj = koffgi_values)
)

# Graficar los resultados
cme_h2t4 <- 
  c_me %>% 
  ggplot(aes(x = KOFFiGIdj, y = estimate,  
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

cme_h2t4

# ---------- Anexos: condicion de simetria ----------
# Siguiendo el mismo ejercicio elaborado antes con el efecto marginal de X sobre
# Y dado Z, se genera ahora el grafico de efectos marginales de Z sobre Y, dado
# X, pero para todo el rango de valores posibles en X.

# Establezco un rango de valores posibles de X y una secuencia de estos
VP_meanValues <- seq(from = 1, to = 20, by = 1)

# Calculo efectos marginales promedio para valores especificos de X
c_me2_unobserved_avg <- marginaleffects::avg_slopes(
  model = h2_tw2,
  newdata = datagrid(VP_mean = VP_meanValues),
  variables = "KOFFiGIdj",
  by = "VP_mean",
  vcov = "HC1",
  conf_level = 0.90
)

# Con base en los resultados se genera el -->

# Grafico de efectos marginales condicionales, ME(Z | X)
cme2_h2tw2 <-
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

cme2_h2tw2

# ---------- Resultados de analisis ----------
# Graficos de tesis
ggsave(
  "cme_h2a.pdf",
  cme_h2tw2,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 13.67,
  height = 9.22,
  units = "cm",
  dpi = "print"
)

ggsave(
  "cme_h2b.pdf",
  cme_h2t4,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 13.67,
  height = 9.22,
  units = "cm",
  dpi = "print"
)

# Graficos de anexos
ggsave(
  "cme_h2c.pdf",
  cme2_h2tw2,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 13.67,
  height = 9.22,
  units = "cm",
  dpi = "print"
)
