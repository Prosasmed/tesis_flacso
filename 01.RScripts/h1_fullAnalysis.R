# Tesis doctoral
# Pruebas y graficas relacionadas al primer conjunto de hipotesis
# MGAP. Pedro Ignacio Rosas-Medina
# Creacion: enero - agosto de 2024
# Ultima revision: febrero de 2025

# ---------- Configuracion ----------

# Directorio de trabajo
setwd('/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/00.Data')

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
library(tidyplots)
library(showtext)

# Datos
load('h1.RData')

# ---------- Limpieza previa ----------

# Datos en formato panel
h1 <- pdata.frame(h1,
                  index = c('country', 'year'))

# Verificacion
class(h1)
is.pbalanced(h1)

# Fuentes para graficas
font_add_google('EB Garamond')
showtext_auto()

# Tema predeterminado para graficas principales en texto
theme <- theme_set(theme(
  plot.title = element_text(size = 12, colour = 'black', 
                            family = 'EB Garamond'),
  plot.caption = element_text(size = 10, colour = 'black', hjust = -0.40,
                              family = 'EB Garamond'),
  axis.title.x = element_text(colour = 'black', size = 12,
                              family = 'EB Garamond'), 
  axis.text.x = element_text(color = 'black', size = 10,
                             family = 'EB Garamond'),
  axis.title.y = element_text(colour = 'black', size = 12,
                              family = 'EB Garamond'),
  axis.text.y = element_text(color = 'black', size = 10,
                             family = 'EB Garamond')))

# ---------- Pruebas (variable ideologia continua) ----------
# Variables independientes principales, controles, sin interaccion
# Modelo individual
h1_in1 <- plm(exp_w1 ~ VP_mean + KOFFiGIdj + 
                pop_w2 + pop_w1_ln + eco_w4 + eco_w6 + eco_w16,
              data = h1,
              model = 'within',
              effect = 'individual')

# Modelo de dos vias
h1_tw1 <- plm(exp_w1 ~ VP_mean + KOFFiGIdj + 
                pop_w2 + pop_w1_ln + eco_w4 + eco_w6 + eco_w16,
              data = h1,
              model = 'within',
              effect = 'twoways')

# Resumenes
summary(h1_in1)
summary(h1_tw1)

# Errores estandar robustos
coeftest(h1_in1, vcov = vcovHC(h1_in1, type = 'HC1'))
coeftest(h1_tw1, vcov = vcovHC(h1_tw1, type = 'HC1'))

# ---------------------------------------------------------------------------- #
# Variables independientes principales, controles, e interaccion
# Modelo individual
h1_in2 <- plm(exp_w1 ~ VP_mean + KOFTrGIdj + VP_mean * KOFTrGIdj +
                pop_w2 + pop_w1_ln + eco_w4 + eco_w6 + eco_w16,
              data = h1,
              model = 'within',
              effect = 'individual')

# Modelo de dos vias
h1_tw2 <- plm(exp_w1 ~ VP_mean + KOFGIdj + VP_mean * KOFGIdj +
                pop_w2 + pop_w1_ln + eco_w4 + eco_w6 + eco_w16,
              data = h1,
              model = 'within',
              effect = 'twoways')

# Resumenes
summary(h1_in2)
summary(h1_tw2)

# Errores estandar robustos
coeftest(h1_in2, vcov = vcovHC(h1_in2, type = 'HC1'))
coeftest(h1_tw2, vcov = vcovHC(h1_tw2, type = 'HC1'))

# ---------- Pruebas (variable ideologia dicotomica) ----------
# Variables independientes principales, controles, sin interaccion
# Modelo individual
h1_in3 <- plm(exp_w1 ~ Hrre_hogR + KOFFiGIdj + 
                pop_w2 + pop_w1_ln + eco_w4 + eco_w6 + eco_w16,
              data = h1,
              model = 'within',
              effect = 'individual')

# Modelo de dos vias
h1_tw3 <- plm(exp_w1 ~ Hrre_hogR + KOFFiGIdj + 
                pop_w2 + pop_w1_ln + eco_w4 + eco_w6 + eco_w16,
              data = h1,
              model = 'within',
              effect = 'twoways')

# Resumenes
summary(h1_in3)
summary(h1_tw3)

# Errores estandar robustos
coeftest(h1_in3, vcov = vcovHC(h1_in3, type = 'HC1'))
coeftest(h1_tw3, vcov = vcovHC(h1_tw3, type = 'HC1'))

# ---------------------------------------------------------------------------- #
# Variables independientes principales, controles, e interaccion
# Modelo individual
h1_in4 <- plm(exp_w1 ~ Hrre_hogR + KOFTrGIdj + Hrre_hogR * KOFTrGIdj +
                pop_w2 + pop_w1_ln + eco_w4 + eco_w6 + eco_w16,
              data = h1,
              model = 'within',
              effect = 'individual')

# Modelo de dos vias
h1_tw4 <- plm(exp_w1 ~ Hrre_hogR + KOFGIdj + Hrre_hogR * KOFGIdj +
                pop_w2 + pop_w1_ln + eco_w4 + eco_w6 + eco_w16,
              data = h1,
              model = 'within',
              effect = 'twoways')

# Resumenes
summary(h1_in4)
summary(h1_tw4)

# Errores estandar robustos
coeftest(h1_in4, vcov = vcovHC(h1_in4, type = 'HC1'))
coeftest(h1_tw4, vcov = vcovHC(h1_tw4, type = 'HC1'))

# ---------- Efectos marginales ----------
# A continuacion se calculan efectos marginales de la posicion ideologica de los
# gobiernos sobre el gasto publico educativo total, bajo diferentes niveles de
# globalizacion. Particularmente, se calculan efectos marginales condicionales
# para todo el rango de valores posibles de Z (es decir, por debajo, o por enci-
# ma de los valores observados).

# Establezco un rango de valores posibles de Z y una secuencia de estos
koffgi_values <- seq(from = 0, to = 100, by = 0.20)

# Ahora calculo efectos marginales promedios para valores especificos de Z
c_me_unobserved_avg <- marginaleffects::avg_slopes(
  model = h1_tw2,
  newdata = datagrid(KOFGIdj = koffgi_values),
  variables = "VP_mean",
  by = "KOFGIdj",
  conf_level = 0.90
)

theme <- theme_set(theme(
  plot.title = element_text(size = 12, colour = "black", 
                            family = "EB Garamond"),
  plot.caption = element_text(size = 10, colour = "black", hjust = -0.5,
                              family = "EB Garamond"),
  axis.title.x = element_text(colour = "black", size = 12,
                              family = "EB Garamond"), 
  axis.text.x = element_text(color = "black", size = 10,
                             family = "EB Garamond"),
  axis.title.y = element_text(colour = "black", size = 12,
                              family = "EB Garamond"),
  axis.text.y = element_text(color = "black", size = 10,
                             family = "EB Garamond")
))

cme_h1a_full <-
  ggplot(c_me_unobserved_avg, aes(x = KOFGIdj, y = estimate)) +
  geom_hline(yintercept = 0.00,
             linetype = "dashed",
             colour = "grey25") +
  geom_line(color = "dodgerblue2",
            linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2,
              colour = "black", 
              linetype = "dotdash", 
              fill = "darkgrey",
              linewidth = 0.5) +
  ggtitle("Figura 3.1. Efectos marginales condicionales sobre el gasto educativo") +
  labs(x = "Nivel de globalización", 
       y = "Efecto de la ideología",
       caption = 
         expression(
           italic("Fuente")*": elaboración propia con base en datos del Banco Mundial (2024) y V-Party (2022)."
           )) +
  theme_apa() +
  theme_set(theme)

cme_h1a_full


# Para el analisis podria ser interesante calcular el nivel de globalizacion (Z)
# donde el efecto marginal de X sobre Y es neutro, es decir: ME(X | Z) = 0.

# Este punto puede ser encontrado resolviendo el punto donde la derivada parcial
# de X sobre Y dado Z es igual a cero, es decir:

# \beta_{X} + \beta_{XZ}Z_{it} = 0

# Resolviendo para $Z_{it}$ tenemos:

# Z_{it} = -\frac{\beta_{X}}{\beta_{XZ}},
# Z_{it} = -\frac{0.37518739}{-0.00569599} = 65.86

# ---------------------------------------------------------------------------- #
# Los efectos marginales condicionales, no obstante, se hacen en funcion de la
# variable de ideologia gubernamental continua. Ahora, se efectua un analisis
# similar, pero para la variable de ideologia dicotomica (1 = gobiernos de dere-
# cha, y 0 = gobiernos de izquierda).

# La grafica a visualizar, particularmente, mostrara los valores pronosticados
# de gasto publico educativo total, condicionado por el tipo de gobierno
# (izquierdas y derechas), a diferentes niveles de globalizacion.
koffgi_values <- seq(from = 0, to = 100, by = 0.20)

# Calcula las predicciones utilizando este rango de valores para KOFGIdj
c_me <- marginaleffects::predictions(
  h1_tw6,
  newdata = datagrid(Hrre_hogR = c(0, 1), KOFGIdj = koffgi_values)
)

# Graficar los resultados
cme_h1b_full <- 
  c_me %>% 
  ggplot(aes(x = KOFGIdj, y = estimate,  
             group = Hrre_hogR,  
             colour = Hrre_hogR, 
             fill = Hrre_hogR)) + 
  geom_line(aes(linetype = Hrre_hogR),
            linewidth = 0.7) + 
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high),
              alpha = 0.1, 
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

cme_h1b_full

# ---------- Anexos: condicion de simetria ----------
# Siguiendo el mismo ejercicio elaborado antes con el efecto marginal de X sobre
# Y dado Z, se genera ahora el efecto marginal de Z sobre Y dado X, pero para to
# do el rango de valores posibles de X.

# Establezco un rango de valores posibles de X y una secuencia de estos
VP_meanValues <- seq(from = 1, to = 20, by = 1)

# Calculo efectos marginales promedios para valores especificos de X
c_me2_unobserved_avg <- marginaleffects::avg_slopes(
  model = h1_tw2,
  newdata = datagrid(VP_mean = VP_meanValues),
  variables = "KOFGIdj",
  by = "VP_mean",
  conf_level = 0.90
)

# Con base en este resultado, se genera la -->

# Grafico el resultado: efectos marginales condicionales, ME(Z | X)
cme_h1c_full <-
  ggplot(c_me2_unobserved_avg,
         aes(x = VP_mean, y = estimate)) +
  geom_hline(yintercept = 0.00,
             linetype = "dashed",
             colour = "grey25") +
  geom_line(color = "dodgerblue",
            linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
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

cme_h1c_full

# ---------- Guardo resultados ----------
# Graficos de tesis
ggsave(
  "cme_h1a.pdf",
  cme_h1a_full,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.50,
  height = 9.75,
  units = "cm",
  dpi = "print"
)

ggsave(
  "cme_h1b.pdf",
  cme_h1b_full,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.51,
  height = 9.22,
  units = "cm",
  dpi = "print"
)

# Graficos de anexo
ggsave(
  "cme_h1c.pdf",
  cme_h1c_full,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.51,
  height = 9.22,
  units = "cm",
  dpi = "print"
)
