# Tesis doctoral
# Pruebas de hipotesis para las Tablas 3.3 y 3.4; y Figuras 3.3 y 3.4
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
load('h3.RData')

# ---------- Limpieza previa ----------

# Datos en formato panel
h3 <- pdata.frame(h3,
                  index = c('country', 'year'))

# Verificacion
class(h3)
is.pbalanced(h3)

# ---------- Modelos: gasto publico educativo total o agregado ----------
# Modelo A
h3_twA <-
  plm(exp_w1 ~ VP_mean + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h3_twA)
coeftest(h3_twA, vcov = vcovHC(h3_twA, type = 'HC1'))

# Modelo B
h3_twB <-
  plm(exp_w1 ~ KOFGI + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h3_twB)
coeftest(h3_twB, vcov = vcovHC(h3_twB, type = 'HC1'))

# Modelo C
h3_twC <-
  plm(exp_w1 ~ v2edteunionindp_ord + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + 
        eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h3_twC)
coeftest(h3_twC, vcov = vcovHC(h3_twC, type = 'HC1'))

# Modelo D
h3_twD <-
  plm(exp_w1 ~ VP_mean + KOFGI + v2edteunionindp_ord + 
        pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h3_twD)
coeftest(h3_twD, vcov = vcovHC(h3_twD, type = 'HC1'))

# Modelo E
h3_twE <-
  plm(exp_w1 ~ VP_mean + KOFGI + v2edteunionindp_ord + 
        (VP_mean * KOFGI * v2edteunionindp_ord) +
        pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h3_twE)
coeftest(h3_twE, vcov = vcovHC(h3_twE, type = 'HC1'))

# ---------- Modelos: gasto publico educativo en educacion basica ----------
# Modelo A
h4_twA <- plm(basic ~ VP_mean + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = 'within',
              effect = 'twoways')

summary(h4_twA)
coeftest(h4_twA, vcov = vcovHC(h4_twA, type = 'HC1'))

# Modelo B
h4_twB <- plm(basic ~ KOFGI + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + 
                eco_w16,
              data = h3,
              model = 'within',
              effect = 'individual')

summary(h4_twB)
coeftest(h4_twB, vcov = vcovHC(h4_twB, type = 'HC1'))

# Modelo C
h4_twC <- plm(basic ~ v2edteunionindp_ord + pop_w1_ln + pop_w2 + eco_w4 + 
                eco_w6 + eco_w16,
              data = h3,
              model = 'within',
              effect = 'individual')

summary(h4_twC)
coeftest(h4_twC, vcov = vcovHC(h4_twC, type = 'HC1'))

# Modelo D
h4_twD <- plm(basic ~ VP_mean + KOFGI + v2edteunionindp_ord +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = 'within',
              effect = 'individual')

summary(h4_twD)
coeftest(h4_twD, vcov = vcovHC(h4_twD, type = 'HC1'))

# Modelo E
h4_twE <-
  plm(basic ~ VP_mean + KOFGI + v2edteunionindp_ord +
        (VP_mean * KOFGI * v2edteunionindp_ord) + 
        pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h4_twE)
coeftest(h4_twE, vcov = vcovHC(h4_twE, type = 'HC1'))

# ---------- Modelos: gasto publico educativo en educacion superior ----------
# Modelo F
h4_twF <- plm(tertiary ~ VP_mean + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + 
                eco_w16,
              data = h3,
              model = 'within',
              effect = 'twoways')

summary(h4_twF)
coeftest(h4_twF, vcov = vcovHC(h4_twF, type = 'HC1'))

# Modelo G
h4_twG <- plm(tertiary ~ KOFGI + pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = 'within',
              effect = 'twoways')

summary(h4_twG)
coeftest(h4_twG, vcov = vcovHC(h4_twG, type = 'HC1'))

# Modelo H
h4_twH <- plm(tertiary ~ v2edteunionindp_ord + pop_w1_ln + pop_w2 + eco_w4 +
                eco_w6 + eco_w16,
              data = h3,
              model = 'within',
              effect = 'twoways')

summary(h4_twH)
coeftest(h4_twH, vcov = vcovHC(h4_twH, type = 'HC1'))

# Modelo I
h4_twI <- plm(tertiary ~ VP_mean + KOFGI + v2edteunionindp_ord +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h3,
              model = 'within',
              effect = 'twoways')

summary(h4_twI)
coeftest(h4_twI, vcov = vcovHC(h4_twI, type = 'HC1'))

# Modelo J
h4_twJ <-
  plm(tertiary ~ VP_mean + KOFTrGIdj + v2edteunionindp_ord +
        (VP_mean * KOFTrGIdj * v2edteunionindp_ord) +
        pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h4_twJ)
coeftest(h4_twJ, vcov = vcovHC(h4_twJ, type = 'HC1'))

# ---------- Grafica de efectos marginales condicionales ----------
# Fuentes de las graficas
font_add_google('EB Garamond')
showtext_auto()

# Configuro un tema para reducir espacio de codigo
theme <-
  theme_set(
    theme(
      plot.title = element_text(size = 12, colour = 'black', 
                                family = 'EB Garamond', hjust = -2),
      plot.caption = element_text(size = 10, colour = 'black', hjust = -0.70,
                                  family = 'EB Garamond'),
      axis.title.x = element_text(size = 12, colour = 'black',
                                  family = 'EB Garamond'),
      axis.title.y = element_text(size = 12, colour = 'black',
                                  family = 'EB Garamond'),
      axis.text.x = element_text(size = 10, colour = 'black',
                                 family = 'EB Garamond'),
      axis.text.y = element_text(size = 10, colour = 'black',
                                 family = 'EB Garamond'),
      strip.text = element_text(size = 12, colour = 'black', 
                                family = 'EB Garamond')  
    )
  )

### Grafica ###
# Establezco rango de valores posibles para Z y secuencia de estos
koffgi_values <-
  seq(from = 0, to = 100,
      by = 0.20)

# Establezco niveles particulares de sindicalismo
union_values <- 0:3

# Calculo efectos marginales promedios para valores en Z
c_me_unobserved_avg <-
  marginaleffects::avg_slopes(
    model = h3_twE,
    newdata = datagrid(
      KOFGI = koffgi_values,
      v2edteunionindp_ord = union_values
    ),
    variables = "VP_mean",
    by = c("KOFGI", "v2edteunionindp_ord"),
    vcov = 'HC1',
    conf_level = 0.90
  )

# ggplot
cme_h3twE <-
  c_me_unobserved_avg |>
  ggplot(aes(x = KOFGI, y = estimate)) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue3', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  facet_wrap(~ v2edteunionindp_ord,
             labeller = as_labeller(c('0' = 'Sindicalismo autónomo',
                                      '1' = 'Sindicalismo independiente',
                                      '2' = 'Sindicalismo moderado',
                                      '3' = 'Sindicalismo dependiente'))) +
  ggtitle('Figura 2.3. Efectos marginales condicionales: ideología, globalización y sindicalismo') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), V-Party (2022), y V-Indoc (2024).'
         )) +
  theme_apa() +
  theme_set(theme)

cme_h3twE

# ---------- Efectos marginales condicionales (desagregados) ----------
### Grafica para educacion basica ###
# Configuro un tema para reducir espacio de codigo
theme2 <-
  theme_set(
    theme(
      plot.title = element_text(size = 12, colour = 'black', 
                                family = 'EB Garamond', hjust = -0.25),
      plot.caption = element_text(size = 10, colour = 'black', hjust = -1,
                                  family = 'EB Garamond'),
      axis.title.x = element_text(size = 12, colour = 'black',
                                  family = 'EB Garamond'),
      axis.title.y = element_text(size = 12, colour = 'black',
                                  family = 'EB Garamond'),
      axis.text.x = element_text(size = 10, colour = 'black',
                                 family = 'EB Garamond'),
      axis.text.y = element_text(size = 10, colour = 'black',
                                 family = 'EB Garamond'),
      strip.text = element_text(size = 12, colour = 'black', 
                                family = 'EB Garamond')  
    )
  )

# Establezco rango de valores posibles para Z y secuencia de estos
koffgi_values <-
  seq(from = 0, to = 100,
      by = 0.20)

# Establezco niveles particulares de sindicalismo
union_values <- 0:3

# Calculo efectos marginales promedios para valores en Z
c_me_unobserved_avg2 <-
  marginaleffects::avg_slopes(
    model = h4_twE,
    newdata = datagrid(
      KOFGI = koffgi_values,
      v2edteunionindp_ord = union_values
    ),
    variables = "VP_mean",
    by = c("KOFGI", "v2edteunionindp_ord"),
    vcov = 'HC1',
    conf_level = 0.90
  )

# ggplot
cme_h4twE <-
  c_me_unobserved_avg2 |>
  ggplot(aes(x = KOFGI, y = estimate)) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue3', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  facet_wrap(~ v2edteunionindp_ord,
             labeller = as_labeller(c('0' = 'Sindicalismo autónomo',
                                      '1' = 'Sindicalismo independiente',
                                      '2' = 'Sindicalismo moderado',
                                      '3' = 'Sindicalismo dependiente'))) +
  ggtitle('Figura 2.4. Efectos marginales condicionales en la educación básica') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), V-Party (2022), y V-Indoc (2024).'
         )) +
  theme_apa() +
  theme_set(theme2)

cme_h4twE

### Grafica para educacion superior ###
# Configuro un tema para reducir espacio de codigo
theme3 <-
  theme_set(
    theme(
      plot.title = element_text(size = 12, colour = 'black', 
                                family = 'EB Garamond', hjust = -0.5),
      plot.caption = element_text(size = 10, colour = 'black', hjust = -2,
                                  family = 'EB Garamond'),
      axis.title.x = element_text(size = 12, colour = 'black',
                                  family = 'EB Garamond'),
      axis.title.y = element_text(size = 12, colour = 'black',
                                  family = 'EB Garamond'),
      axis.text.x = element_text(size = 10, colour = 'black',
                                 family = 'EB Garamond'),
      axis.text.y = element_text(size = 10, colour = 'black',
                                 family = 'EB Garamond'),
      strip.text = element_text(size = 12, colour = 'black', 
                                family = 'EB Garamond')  
    )
  )

# Establezco rango de valores posibles para Z y secuencia de estos
koffgi_values <-
  seq(from = 0, to = 100,
      by = 0.20)

# Establezco niveles particulares de sindicalismo
union_values <- 0:3

# Calculo efectos marginales promedios para valores en Z
c_me_unobserved_avg3 <-
  marginaleffects::avg_slopes(
    model = h4_twJ,
    newdata = datagrid(
      KOFTrGIdj = koffgi_values,
      v2edteunionindp_ord = union_values
    ),
    variables = "VP_mean",
    by = c("KOFTrGIdj", "v2edteunionindp_ord"),
    vcov = 'HC1',
    conf_level = 0.90
  )

# ggplot
cme_h4twJ <-
  c_me_unobserved_avg3 |>
  ggplot(aes(x = KOFTrGIdj, y = estimate)) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue3', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  facet_wrap(~ v2edteunionindp_ord,
             labeller = as_labeller(c('0' = 'Sindicalismo autónomo',
                                      '1' = 'Sindicalismo independiente',
                                      '2' = 'Sindicalismo moderado',
                                      '3' = 'Sindicalismo dependiente'))) +
  ggtitle('Figura 2.5. Efectos marginales condicionales para la formación superior') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), V-Party (2022), y V-Indoc (2024).'
         )) +
  theme_apa() +
  theme_set(theme2)

cme_h4twJ

# ---------- Resultados de visualizacion ----------
# Grafica 2.3 en tesis
ggsave(
  "cme_h3a.pdf",
  cme_h3twE,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.49,
  height = 9.75,
  units = "cm",
  dpi = "print"
)

# Grafica 2.4 en tesis
ggsave(
  "cme_h4a.pdf",
  cme_h4twE,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.49,
  height = 9.75,
  units = "cm",
  dpi = "print"
)

# Grafica 2.5 en tesis
ggsave(
  "cme_h4b.pdf",
  cme_h4twJ,
  device = "pdf",
  path = "/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/02.Plots",
  width = 16.49,
  height = 9.75,
  units = "cm",
  dpi = "print"
)