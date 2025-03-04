# Tesis doctoral
# Pruebas de hipotesis para la Tabla 3.1 y Figura 3.1
# MGAP. Pedro Ignacio Rosas-Medina
# Creacion: febrero de 2025

# ---------- Configuracion ----------

# Directorio de trabajo
setwd('/Users/pirm/Library/CloudStorage/GoogleDrive-prosasmed@gmail.com/My Drive/Academic/Tesis/tesis_doctorado/00.Data')

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
load('h1.RData')

# ---------- Limpieza previa ----------

# Datos en formato panel
h1 <- pdata.frame(h1,
                  index = c('country', 'year'))

# Verificacion
class(h1)
is.pbalanced(h1)

# ---------- Modelos: gasto publico educativo total o agregado ----------
# Modelo A
h1_twA <-
  plm(exp_w1 ~ VP_mean + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h1,
      model = 'within',
      effect = 'twoways')

summary(h1_twA)
coeftest(h1_twA, vcov = vcovHC(h1_twA, type = 'HC1'))

# Modelo B
h1_twB <-
  plm(exp_w1 ~ KOFGIdj + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h1,
      model = 'within',
      effect = 'twoways')

summary(h1_twB)
coeftest(h1_twB, vcov = vcovHC(h1_twB, type = 'HC1'))

# Modelo C
h1_twC <-
  plm(exp_w1 ~ VP_mean + KOFGIdj + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + 
        eco_w16,
      data = h1,
      model = 'within',
      effect = 'twoways')

summary(h1_twC)
coeftest(h1_twC, vcov = vcovHC(h1_twC, type = 'HC1'))

# Modelo D
h1_twD <-
  plm(exp_w1 ~ VP_mean + KOFGIdj + VP_mean * KOFGIdj + 
        pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h1,
      model = 'within',
      effect = 'twoways')

summary(h1_twD)
coeftest(h1_twD, vcov = vcovHC(h1_twD, type = 'HC1'))

# ---------- Grafica de efectos marginales condicionales ----------
# Fuentes de las graficas
font_add_google('EB Garamond')
showtext_auto()

# Configuro un tema para reducir espacio de codigo
theme <-
  theme_set(
    theme(
      plot.title = element_text(size = 12, colour = 'black', 
                                family = 'EB Garamond', hjust = -4),
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
  
### Grafica ###
# Establezco rango de valores posibles para Z y secuencia de estos
koffgi_values <-
  seq(from = 0, to = 100,
      by = 0.20)

# Calculo efectos marginales promedios para valores en Z
c_me_unobserved_avg <-
  marginaleffects::avg_slopes(
    model = h1_twD,
    newdata = datagrid(KOFGIdj = koffgi_values),
    variables = 'VP_mean',
    by = 'KOFGIdj',
    conf_level = 0.90
  )

# ggplot
cme_h1_full <-
  c_me_unobserved_avg |>
  ggplot(aes(x = KOFGIdj, y = estimate)) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue3', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  ggtitle('Figura 2.1. Efectos marginales condicionales sobre el gasto público educativo total') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), y V-Party (2022).'
         )) +
  theme_apa() +
  theme_set(theme)

cme_h1_full

# ---------- Resultados de visualizacion ----------
# Grafica 2.1 en tesis
ggsave(
  "cme_h1a.pdf",
  cme_h1_full,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.50,
  height = 9.75,
  units = "cm",
  dpi = "print"
)
