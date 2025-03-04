# Tesis doctoral
# Efectos marginales para la hipotesis 2

# Pedro I. Rosas-Medina
# Doctorado de Investigacion en Ciencias Sociales
# Ciencia Politica
# Seminario de Investigacion en Economia Politica Comparada
# FLACSO Meixco

# ---------- Configuracion ----------
# Directorio de trabajo
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

# ---------- Modelos de interes ----------
# ---------- Educacion basica ----------
# Variables independientes principales, con controles e interacciones
h2_tw2 <- plm(basic ~ VP_mean + KOFFiGIdj + VP_mean * KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = "within",
              effect = "twoways")

h2_tw4 <- plm(basic ~ Hrre_hogR + KOFFiGIdj + Hrre_hogR * KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = "within",
              effect = "twoways")

# Resumenes
summary(h2_tw2)
summary(h2_tw4)

# Errores estandar robustos
coeftest(h2_tw2,
         vcov = vcovHC(h2_tw2,
                       type = "HC1"))
coeftest(h2_tw4,
         vcov = vcovHC(h2_tw4,
                       type = "HC1"))

# ---------- Educacion superior ----------
# Variables independientes principales, con controles e interacciones
h2b_tw2 <- plm(tertiary ~ VP_mean + KOFTrGI + VP_mean * KOFTrGI +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "twoways")

h2b_tw4 <- plm(tertiary ~ Hrre_hogR + KOFGI + Hrre_hogR * KOFGI +
                 pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
               data = h2,
               model = "within",
               effect = "twoways")

# Resumenes
summary(h2b_tw2)
summary(h2b_tw4)

# Errores estandar robustos
coeftest(h2b_tw2,
         vcov = vcovHC(h2b_tw2,
                       type = "HC1"))
coeftest(h2b_tw4,
         vcov = vcovHC(h2b_tw4,
                       type = "HC1"))

# ---------- Efectos marginales ----------
# Efecto marginal de X sobre Y (basica y superior), segun Z
# Educacion basica
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
  ggtitle("A. Educación básica") +
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

# Educacion superior
# Establezco un rango de valores posibles de Z y una secuencia de estos
koffgi_values <- seq(from = 0, to = 100, by = 0.20)

# Calculo efectos marginales promedio para valores especificos de Z
summary(h2b_tw2)
c_me_unobserved_avg <- marginaleffects::avg_slopes(
  model = h2b_tw2,
  newdata = datagrid(KOFTrGI = koffgi_values),
  variables = "VP_mean",
  by = "KOFTrGI",
  vcov = "HC1",
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
  ggtitle("B. Educación superior") +
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

# Junto los graficos para efectos marginales condicionales
cme_h2 <-
  ggarrange(
    cme_h2tw2, cme_h2btw2,
    font.label = list(size = 11, color = "black", face = "plain")
  )

cme_h2

# ---------- Valores esperados ----------
# Educacion basica
# Establezco un rango de valores posibles de Z y una secuencia de estos
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
  ggtitle("A. Educación básica") + 
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

# Educacion superior
# Establezco un rango de valores posibles de Z y una secuencia de estos
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
  ggtitle("B. Educación superior") +
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

# Junto los graficos para efectos marginales condicionales
cme_h2b <-
  ggarrange(
    cme_h2t4, c_meh2btw4,
    font.label = list(size = 11, color = "black", face = "plain")
  )

cme_h2b

# ---------- Anexos: condicion de simetria ----------
# Educacion basica
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
  ggtitle("A. Educación básica") +
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

# Educacion superior
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
  ggtitle("B. Educación superior") +
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

# Junto los graficos para efectos marginales condicionales
cme_h2Anexo <-
  ggarrange(
    cme2_h2tw2, cme2_h2btw2,
    font.label = list(size = 11, color = "black", face = "plain")
  )

cme_h2Anexo

# ---------- Guardo resultados para analisis ----------
ggsave(
  "cme_h2a.pdf",
  cme_h2,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.51,
  height = 9.22,
  units = "cm",
  dpi = "print"
)

ggsave(
  "cme_h2b.pdf",
  cme_h2b,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.51,
  height = 9.22,
  units = "cm",
  dpi = "print"
)

ggsave(
  "cme_h2Anexo.pdf",
  cme_h2Anexo,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.51,
  height = 9.22,
  units = "cm",
  dpi = "print"
)
