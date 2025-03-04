# Tesis doctoral
# Pruebas para hipotesis 3

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
library(sjmisc)

# ---------- Limpieza final ----------
# Data
load("h3.RData")

# Confirmo transformacion
class(h3$v2edteunionindp_ord)
levels(h3$v2edteunionindp_ord)
unique(h3$v2edteunionindp_ord)

# Preparo datos en formato panel y establezco id individuo-tiempo
h3 <- pdata.frame(h3,
                  index = c("country", "year"))

# Verifico resultado panel
class(h3)
is.pbalanced(h3)

# ---------- H3.1: Gasto publico educativo total ----------
# Principales variables independientes, con controles, sin interaccion
# Efectos individuales nivel pais
h3_in1 <- plm(exp_w1 ~ VP_mean + KOFGI + v2edteunionindp_ord +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = "within",
              effect = "individual")

# Efectos de dos vias
h3_tw1 <- plm(exp_w1 ~ VP_mean + KOFGI + v2edteunionindp_ord +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h3_in1)
summary(h3_tw1)

coef(h3_in1)

# Errores estandar robustos
coeftest(h3_in1,
         vcov = vcovHC(h3_in1,
                       type = "HC1"))
coeftest(h3_tw1,
         vcov = vcovHC(h3_tw1,
                       type = "HC1"))

# ---------------------------------------------------------------------------- #
# Principales variables independientes, con controles e interacciones
# Efectos individuales nivel pais
h3_in2 <- plm(exp_w1 ~ VP_mean + KOFGI + v2edteunionindp_ord +
                (VP_mean * KOFGI * v2edteunionindp_ord) +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = "within",
              effect = "individual")

# Efectos de dos vias
h3_tw2 <- plm(exp_w1 ~ VP_mean + KOFGI + v2edteunionindp_ord +
                (VP_mean * KOFGI * v2edteunionindp_ord) +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h3_in2)
summary(h3_tw2)

# Errores estandar robustos
coeftest(h3_in2,
         vcov = vcovHC(h3_in2,
                       type = "HC1"))
coeftest(h3_tw2,
         vcov = vcovHC(h3_tw2,
                       type = "HC1"))

# ---------- Variable dicotomica (1 = right, 0 = left) ----------
# Principales variables independientes, con controles, sin interaccion
# Efectos individuales nivel pais
h3_in3 <- plm(exp_w1 ~ Hrre_hogR + KOFGI + v2edteunionindp_ord +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = "within",
              effect = "individual")

# Efectos de dos vias
h3_tw3 <- plm(exp_w1 ~ Hrre_hogR + KOFGI + v2edteunionindp_ord +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h3_in3)
summary(h3_tw3)

# Errores estandar robustos
coeftest(h3_in3,
         vcov = vcovHC(h3_in3,
                       type = "HC1"))
coeftest(h3_tw3,
         vcov = vcovHC(h3_tw3,
                       type = "HC1"))

# ---------------------------------------------------------------------------- #
# Principales variables independientes, con controles e interacciones
# Efectos individuales nivel pais
h3_in4 <- plm(exp_w1 ~ Hrre_hogR + KOFGI + v2edteunionindp_ord +
                (Hrre_hogR * KOFGI * v2edteunionindp_ord) +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = "within",
              effect = "individual")

# Efectos de dos vias
h3_tw4 <- plm(exp_w1 ~ Hrre_hogR + KOFGI + v2edteunionindp_ord +
                (Hrre_hogR * KOFGI * v2edteunionindp_ord) +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h3_in4)
summary(h3_tw4)

# Errores estandar robustos
coeftest(h3_in4,
         vcov = vcovHC(h3_in4,
                       type = "HC1"))
coeftest(h3_tw4,
         vcov = vcovHC(h3_tw4,
                       type = "HC1"))

# ---------- Efectos marginales ----------
# A continuacion se calculan efectos marginales de la posicion ideologica de los
# gobiernos sobre el gasto publico educativo total, bajo diferentes niveles de
# globalizacion, y distintos escenarios de sindicalismo. Particularmente, se
# calculan efectos marginales condicionales para el rango de valores posibles de
# Z (es decir, por debajo o por encima de los valores observados); y se trata la
# variable de sindicalismo como un factor de cuatro niveles (0 = sindicalismo
# dependiente; 1 = sindicalismo moderado; 2 = sindicalismo independiente, 3 =
# sindicalismo autonomo).

# Establezco un rango de valores posibles de globalizacion en secuencia
koffgi_values <- seq(from = 0, to = 100, by = 0.20)

# Establezco los niveles particulares de sindicalismo
union_values <- 0:3

# Calculo efectos marginales promedio para valores especificos de Z, tratando la
# variable de sindicalismo como factor
c_me_unobserved_avg <- marginaleffects::avg_slopes(
  model = h3_tw2,
  newdata = datagrid(
    KOFGI = koffgi_values,
    v2edteunionindp_ord = union_values
  ),
  variables = "VP_mean",
  by = c("KOFGI", "v2edteunionindp_ord"),
  vcov = "HC1",
  conf_level = 0.90
)

cme_h3tw2 <-  
  c_me_unobserved_avg %>%  
  ggplot(aes(x = KOFGI, y = estimate)) +  
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
  labs(x = "Economic Globalization",  
       y = "Effect of Ideology") +  
  theme_apa(x.font.size = 10,  
            y.font.size = 10) +  
  theme(plot.title = element_text(size = 10),  
        axis.text.x = element_text(color = "black", size = 8),  
        axis.text.y = element_text(color = "black", size = 8),  
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10)) +
  facet_wrap(~ v2edteunionindp_ord,  
             labeller = as_labeller(c('0' = "Dependent Unionism",  
                                      '1' = "Moderate Unionism",  
                                      '2' = "Independent Unionism",  
                                      '3' = "Autonomous Unionism")))

cme_h3tw2

# ---------------------------------------------------------------------------- #
# Los efectos marginales condicionales, no obstante, se elaboran en funcion de
# la variable de ideologia gubernamental continua. Ahora, se efectua un analisis
# similar, pero para la variable categorica de ideologia (1 = gobiernos de dere-
# cha; y 0 = gobiernos de izquierda).

# Las graficas permitiran visualizar, particularmente, los valores de gasto
# pronosticados, condicionado por el tipo de gobierno (izquierda o derecha), a
# diferentes niveles de globalizacion, y escenarios de sindicalismo.

# Establezco un rango de valores posibles de globalizacion en secuencia
koffgi_values <- seq(from = 0, to = 100, by = 0.20)

# Establezco los niveles particulares de sindicalismo
union_values <- 0:3

# Calculo niveles de gasto esperados para valores especificos de Z, tratando la
# variable de sindicalismo como factor
c_me <- marginaleffects::predictions(
  h3_tw4,
  newdata = datagrid(
    Hrre_hogR = c(0, 1),
    KOFGI = koffgi_values,
    v2edteunionindp_ord = union_values),
  vcov = "HC1",
  conf_level = 0.90
)

# Grafico
cme_h3tw4 <-
c_me %>%
  ggplot(aes(x = KOFGI, y = estimate,
             group = Hrre_hogR,
             colour = Hrre_hogR,
             fill = Hrre_hogR)) +
  geom_line(aes(linetype = Hrre_hogR),
            linewidth = 0.7) +
  scale_colour_manual(values = c("dodgerblue", "firebrick"),
                      labels = c("Left-leaning", "Right-leaning")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick"),
                    labels = c("Left-leaning", "Right-leaning")) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Left-leaning", "Right-leaning")) +
  labs(x = "Economic Globalization", 
       y = "Effect of Ideology") + 
  theme_apa(x.font.size = 10, 
            y.font.size = 10) + 
  theme(plot.title = element_text(size = 10), 
        axis.text.x = element_text(color = "black", size = 8), 
        axis.text.y = element_text(color = "black", size = 8)) + 
  facet_wrap(~ v2edteunionindp_ord, 
             labeller = as_labeller(c('0' = "Dependent Unionism",  
                                      '1' = "Moderate Unionism",  
                                      '2' = "Independent Unionism",  
                                      '3' = "Autonomous Unionism")))

cme_h3tw4

# ---------- Resultados del analisis ----------
# Graficos de tesis
ggsave(
  "cme_h3tw2_english.pdf",
  cme_h3tw2,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.51,
  height = 9.22,
  units = "cm",
  dpi = "print"
)
