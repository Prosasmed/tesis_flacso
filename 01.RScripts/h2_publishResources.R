# Tesis doctoral
# Pruebas de hipotesis para la Tabla 3.2 y Figura 3.2
# MGAP. Pedro Ignacio Rosas-Medina
# Creacion: febrero de 2025

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
library(wesanderson)

# Datos
load('h2.RData')

# ---------- Limpieza previa ----------

# Datos en formato panel
h2 <- pdata.frame(h2,
                  index = c('country', 'year'))

# Verificacion
class(h2)
is.pbalanced(h2)

# ---------- Modelos: gasto publico educativo en educacion basica ----------
# Modelo A
h2_twA <- plm(basic ~ VP_mean + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twA)
coeftest(h2_twA, vcov = vcovHC(h2_twA, type = 'HC1'))

# Modelo B
h2_twB <- plm(basic ~ KOFFiGIdj + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + 
                eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twB)
coeftest(h2_twB, vcov = vcovHC(h2_twB, type = 'HC1'))

# Modelo C
h2_twC <- plm(basic ~ VP_mean + KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twC)
coeftest(h2_twC, vcov = vcovHC(h2_twC, type = 'HC1'))

# Modelo D
h2_twD <- plm(basic ~ VP_mean + KOFFiGIdj + VP_mean * KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twD)
coeftest(h2_twD, vcov = vcovHC(h2_twD, type = 'HC1'))

# ---------- Modelos: gasto publico educativo en educacion superior ----------
# Modelo E
h2_twE <- plm(tertiary ~ VP_mean + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + 
                eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twE)
coeftest(h2_twE, vcov = vcovHC(h2_twD, type = 'HC1'))

# Modelo F
h2_twF <- plm(tertiary ~ KOFTrGIdj + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + 
                eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twF)
coeftest(h2_twF, vcov = vcovHC(h2_twF, type = 'HC1'))

# Modelo G
h2_twG <- plm(tertiary ~ VP_mean + KOFTrGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twG)
coeftest(h2_twG, vcov = vcovHC(h2_twG, type = 'HC1'))

# Modelo H
h2_twH <- plm(tertiary ~ VP_mean + KOFTrGI + VP_mean * KOFTrGI +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twH)
coeftest(h2_twH, vcov = vcovHC(h2_twH, type = 'HC1'))

# ---------- Grafica de efectos marginales condicionales ----------
# Fuentes de las graficas
font_add_google('EB Garamond')
showtext_auto()

# Configuro un tema para reducir espacio de codigo
theme <-
  theme_set(
    theme(
      plot.title = element_text(size = 12, colour = 'black', 
                                family = 'EB Garamond'),
      plot.caption = element_text(size = 10, colour = 'black', hjust = -0.50,
                                  family = 'EB Garamond'),
      axis.title.x = element_text(size = 12, colour = 'black',
                                  family = 'EB Garamond'),
      axis.title.y = element_text(size = 12, colour = 'black',
                                  family = 'EB Garamond'),
      axis.text.x = element_text(size = 10, colour = 'black',
                                 family = 'EB Garamond'),
      axis.text.y = element_text(size = 10, colour = 'black',
                                 family = 'EB Garamond')
    )
  )

### Grafica individual para efectos marginales condicionales sobre SEB ###
# Establezco rango de valores posibles para Z y secuencia de estos
koffgi_values <-
  seq(from = 0, to = 100,
      by = 0.20)

# Calculo efectos marginales promedios para valores en Z
c_me_unobserved_avg <-
  marginaleffects::avg_slopes(
    model = h2_twD,
    newdata = datagrid(KOFFiGIdj = koffgi_values),
    variables = 'VP_mean',
    by = 'KOFFiGIdj',
    vcov = 'HC1',
    conf_level = 0.90
  )

# Grafica individual
cme_h2twD <-
  c_me_unobserved_avg |>
  ggplot(aes(x = KOFFiGIdj, y = estimate)) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue3', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  ggtitle('A. Educación básica') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología') +
  theme_apa() +
  theme_set(theme)

cme_h2twD

### Grafica individual para efectos marginales condicionales sobre ES-P-V ###
# Establezco rango de valores posibles para Z y secuencia de estos
koffgi_values <-
  seq(from = 0, to = 100,
      by = 0.20)

# Calculo efectos marginales promedio para valores en Z
c_me_unobserved_avg2 <-
  marginaleffects::avg_slopes(
    model = h2_twH,
    newdata = datagrid(KOFTrGI = koffgi_values),
    variables = 'VP_mean',
    by = 'KOFTrGI',
    vcov = 'HC1',
    conf_level = 0.90
  )

# Grafica individual
cme_h2twH <-
  c_me_unobserved_avg2 |>
  ggplot(aes(x = KOFTrGI, y = estimate)) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue3', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  ggtitle('B. Educación superior') +
  labs(x = 'Nivel de globalización', y = NULL) +
  theme_apa() +
  theme_set(theme)

### Graficos juntos ###
cme_h2 <-
  ggarrange(
    cme_h2twD, cme_h2twH,
    nrow = 1, ncol = 2
  )

cme_h2 <- annotate_figure(
  cme_h2,
  bottom = text_grob(
    expression(
      italic("Fuente")~": elaboración propia con base en datos del Banco Mundial (2024), y V-Party (2022)"), 
    size = 10, family = 'EB Garamond', x = 0.38)
)

# ---------- Resultados de visualizacion ----------
# Grafica 3.2 en tesis
ggsave(
  'cme_h2a.pdf',
  cme_h2,
  device = 'pdf',
  path = '/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots',
  width = 16.51,
  height = 9.22,
  units = 'cm',
  dpi = 'print'
)
