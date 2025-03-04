# Tesis doctoral
# Pruebas de hipotesis para la Tabla 3.1 y Figura 3.1
# MGAP. Pedro Ignacio Rosas-Medina
# Creacion: febrero de 2025

# ---------- Configuracion ----------

# Directorio de trabajo
setwd('/Users/pirm/Library/CloudStorage/GoogleDrive-prosasmed@gmail.com/Mi unidad/Academic/Tesis/tesis_doctorado/00.Data')

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
library(ggside)

# Datos
load('h1.RData')
load('h2.RData')
load('h3.RData')
promedios <- read.csv("promedios.csv")
fig_2.1 <- read.csv("Figure_protocol.csv")
df <- read.csv("tesis_short.csv")
first_democ <- read.csv("first_democ.csv")

# ---------- Limpieza previa ----------

# Datos en formato panel
h1 <- pdata.frame(h1,
                  index = c('country', 'year'))

h2 <- pdata.frame(h2,
                  index = c('country', 'year'))

h3 <- pdata.frame(h3,
                  index = c('country', 'year'))

df <- df |>
  dplyr::mutate(country_facet = dplyr::case_when(
    country == "República Dominicana" ~ "R. Dominicana",
    TRUE ~ country
  ))

first_democ <- 
  first_democ |>
  mutate(country_facet = ifelse(country == "República Dominicana", 
                                "R. Dominicana", country))

# ---------- Modelos econometricos ----------

# Modelo D - Grafica 1
h1_twD <-
  plm(exp_w1 ~ VP_mean + KOFGIdj + VP_mean * KOFGIdj + 
        pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h1,
      model = 'within',
      effect = 'twoways')

summary(h1_twD)
coeftest(h1_twD, vcov = vcovHC(h1_twD, type = 'HC1'))

# Modelo D - Grafica 2-A
h2_twD <- plm(basic ~ VP_mean + KOFFiGIdj + VP_mean * KOFFiGIdj +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twD)
coeftest(h2_twD, vcov = vcovHC(h2_twD, type = 'HC1'))

# Modelo H - Grafica 2-B
h2_twH <- plm(tertiary ~ VP_mean + KOFTrGI + VP_mean * KOFTrGI +
                pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
              data = h2,
              model = 'within',
              effect = 'twoways')

summary(h2_twH)
coeftest(h2_twH, vcov = vcovHC(h2_twH, type = 'HC1'))

# Modelo E - Grafica 3
h3_twE <-
  plm(exp_w1 ~ VP_mean + KOFGI + v2edteunionindp_ord + 
        (VP_mean * KOFGI * v2edteunionindp_ord) +
        pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h3_twE)
coeftest(h3_twE, vcov = vcovHC(h3_twE, type = 'HC1'))

# Modelo E - Grafica 4
h4_twE <-
  plm(basic ~ VP_mean + KOFGI + v2edteunionindp_ord +
        (VP_mean * KOFGI * v2edteunionindp_ord) + 
        pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h4_twE)
coeftest(h4_twE, vcov = vcovHC(h4_twE, type = 'HC1'))

# Modelo 5
h4_twJ <-
  plm(tertiary ~ VP_mean + KOFTrGIdj + v2edteunionindp_ord +
        (VP_mean * KOFTrGIdj * v2edteunionindp_ord) +
        pop_w1_ln + pop_w2 + eco_w4 + eco_w6 + eco_w16,
      data = h3,
      model = 'within',
      effect = 'twoways')

summary(h4_twJ)
coeftest(h4_twJ, vcov = vcovHC(h4_twJ, type = 'HC1'))

# ---------- Plots ----------
### Plot 1 ###
plot1 <- 
  promedios |>
  ggplot(aes(x = year, y = value, group = variable, linetype = variable, 
             color = variable)) +
  geom_line() +
  ggtitle("Evolución promedio del gasto público educativo:",
          subtitle = "Subsistemas de enseñanza básica, y formación superior") +
  labs(x = NULL, y = 'Gasto educativo\n(% del PIB)',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024).'
         )) +
  scale_x_continuous(limits = c(1970, 2018),
                     breaks = c(1970, 1980, 1990, 2000, 2010, 2018)) +
  scale_linetype_discrete(labels = c("Básica", "Superior")) +
  scale_color_manual(values = c("basic" = "dodgerblue", 
                                "superior" = "firebrick"),
                     labels = c("Básica", "Superior")) +
  theme_apa(legend.pos = 'bottom') +
  theme(plot.title = element_text(size = 16, colour = 'black'),
        plot.subtitle = element_text(size = 16, colour = 'black'),
        plot.caption = element_text(size = 12, colour = 'black', hjust = 0),
        legend.text = element_text(size = 14, colour = 'black'),
        axis.title.x = element_text(size = 14, colour = 'black'),
        axis.title.y = element_text(size = 14, colour = 'black'),
        axis.text.x = element_text(size = 12, colour = 'black'),
        axis.text.y = element_text(size = 12, colour = 'black'))

plot1

### Plot 2 ###
plot2 <- 
  fig_2.1 |>
  filter(!country %in% c('Jamaica', 'Trinidad y Tobago')) |>
  ggplot(aes(x = value, y = country, colour = variable, 
             shape = variable)) +
  geom_point(size = 2) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("firebrick", "dodgerblue", "forestgreen")) +
  ggtitle('Educación pública según el momento de democratización') +
  labs(x = NULL, y = NULL,
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos de Lee & Lee (2016), Paglayan (2021), y Polity V (2020).'
         )) +
  theme_apa(legend.pos = 'bottom', remove.y.gridlines = FALSE) +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.subtitle = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        legend.text = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black')) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

plot2

### Plot 3 ###
plot3 <- 
  df |>
  ggplot(aes(x = year, y = enr_w31)) +
  geom_line(aes(group = country_facet)) +
  geom_hline(yintercept = 90, linetype = "dotted", colour = "grey25", 
             size = 0.45) +
  geom_vline(aes(xintercept = year), first_democ,
             colour = "firebrick1",
             linetype = "dashed", size = 1) +
  scale_x_continuous(limits = c(1970, 2018), breaks = c(1978, 1998, 2018)) +
  facet_wrap(~ country_facet, nrow = 3, ncol = 6) +
  ggtitle('Cobertura educativa en primaria según primera democratización') +
  labs(x = NULL, y = "Cobertura en educación\nbásica (% bruto)",
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos de Lee & Lee (2016) y el Banco Mundial (2024).'
         )) +
  theme_apa(facet.title.size = 10) +
  theme(plot.title = element_text(size = 12, colour = 'black'),
        plot.subtitle = element_text(size = 12, colour = 'black'),
        plot.caption = element_text(size = 8, colour = 'black', hjust = 0),
        axis.title.y = element_text(size = 10, colour = 'black'),
        axis.text.x = element_text(size = 8, colour = 'black'),
        axis.text.y = element_text(size = 8, colour = 'black'))

plot3


### Grafica 1 de diapositivas###
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

load('h1.RData')
# ggplot
cme_h1_full <-
  c_me_unobserved_avg |>
  ggplot(aes(x = KOFGIdj, y = estimate)) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  ggtitle('Efectos marginales condicionales sobre el gasto público educativo total') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), y V-Party (2022).'
         )) +
  annotate(
    "text", x = 82, y = 0.53, 
    label = "Efecto marginal condicional:", 
    size = 3.5, fontface = "italic", hjust = 0
  ) +
  annotate(
    "text", x = 82, y = 0.43, 
    label = expression(frac(partialdiff * Y[it]^j, partialdiff * Ideo[it]) == beta[1] + beta[3] * Glb[it]), 
    parse = TRUE, size = 3.5, hjust = 0
  ) +
  theme_apa() +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        axis.title.x = element_text(size = 12, colour = 'black'),
        axis.title.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'))

cme_h1_full

# Segundo plot
cme_h1_full2 <-
  c_me_unobserved_avg |>
  ggplot(aes(x = KOFGIdj, y = estimate)) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  geom_xsidehistogram(
    data = h1, 
    aes(x = KOFGIdj),
    inherit.aes = TRUE,
    fill = "gray23",
    bins = 45, 
    colour = "black",
    linewidth = 0.5
  ) +
  ggtitle('Efectos marginales condicionales sobre el gasto público educativo total') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), y V-Party (2022).'
         )) +
  annotate(
    "text", x = 82, y = 0.53, 
    label = "Efecto marginal condicional:", 
    size = 3.5, fontface = "italic", hjust = 0
  ) +
  annotate(
    "text", x = 82, y = 0.43, 
    label = expression(frac(partialdiff * Y[it]^j, partialdiff * Ideo[it]) == beta[1] + beta[3] * Glb[it]), 
    parse = TRUE, size = 3.5, hjust = 0
  ) +
  theme_apa() +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        axis.title.x = element_text(size = 12, colour = 'black'),
        axis.title.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        ggside.panel.scale.x = 0.2)

cme_h1_full2

# Tercer plot
cme_h1_full3 <-
  c_me_unobserved_avg |>
  ggplot(aes(x = KOFGIdj, y = estimate)) +
  geom_vline(xintercept = 65.04, colour = "firebrick2", linewidth = 0.5,
             linetype = 'dotted') +
  annotate(
    "text", x = 50, y = 0.50, 
    label = "Globalización 3er\ncuartil = 60.04%", 
    colour = "firebrick", size = 3.5, hjust = 0
  ) +
  geom_vline(xintercept = 65.86, colour = "forestgreen", linewidth = 0.5,
             linetype = 'dotted') +
  annotate(
    "text", x = 69, y = 0.32, 
    label = expression(ME(X * "|" * Z) == 0), 
    colour = "forestgreen", size = 3.5, parse = TRUE, hjust = 0
  ) +
  annotate(
    "text", x = 69, y = 0.22, 
    label = expression(
      Glb[it] == -frac(0.38, 0.01) ~ "=" ~ 64.86
    ), 
    colour = "forestgreen", size = 3.5, parse = TRUE, hjust = 0
  ) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  geom_xsidehistogram(
    data = h1, 
    aes(x = KOFGIdj),
    inherit.aes = TRUE,
    fill = "gray23",
    bins = 45, 
    colour = "black", # Añadido para el color del borde
    linewidth = 0.5 # Ajusta el grosor del contorno
  ) +
  ggtitle('Efectos marginales condicionales sobre el gasto público educativo total') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), y V-Party (2022).'
         )) +
  annotate(
    "text", x = 82, y = 0.53, 
    label = "Efecto marginal condicional:", 
    size = 3.5, fontface = "italic", hjust = 0
  ) +
  annotate(
    "text", x = 82, y = 0.43, 
    label = expression(frac(partialdiff * Y[it]^j, partialdiff * Ideo[it]) == beta[1] + beta[3] * Glb[it]), 
    parse = TRUE, size = 3.5, hjust = 0
  ) +
  theme_apa() +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        axis.title.x = element_text(size = 12, colour = 'black'),
        axis.title.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        ggside.panel.scale.x = 0.2)

cme_h1_full3

### Grafica 2-A diapositivas ###
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

load('h2.RData')
# Grafica individual
cme_h2twD <-
  c_me_unobserved_avg |>
  ggplot(aes(x = KOFFiGIdj, y = estimate)) +
  geom_hline(yintercept = 0.00, linetype = 'dashed', colour = 'grey25') +
  geom_line(colour = 'dodgerblue', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  ggtitle('A. Educación básica') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología') +
  theme_apa() +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        axis.title.x = element_text(size = 12, colour = 'black'),
        axis.title.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'))

### Grafica 2-B diapositivas ###
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
  geom_line(colour = 'firebrick1', linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, colour = 'black', linetype = 'dotdash',
              fill = 'darkgrey', linewidth = 0.5) +
  ggtitle('B. Educación superior') +
  labs(x = 'Nivel de globalización', y = NULL) +
  theme_apa() +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        axis.title.x = element_text(size = 12, colour = 'black'),
        axis.title.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'))

### Graficos juntos (2-A, y 2-B) ###
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
    size = 10, x = 0.38)
)

cme_h2

### Grafica 3 diapositivas ###
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
  ggtitle('Efectos marginales condicionales: ideología, globalización y sindicalismo') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), V-Party (2022), y V-Indoc (2024).'
         )) +
  theme_apa() +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        axis.title.x = element_text(size = 12, colour = 'black'),
        axis.title.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'))

cme_h3twE

### Grafica 4 diapositivas ###
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
  ggtitle('Efectos marginales condicionales en la educación básica') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), V-Party (2022), y V-Indoc (2024).'
         )) +
  theme_apa() +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        axis.title.x = element_text(size = 12, colour = 'black'),
        axis.title.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'))

cme_h4twE

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
  ggtitle('Efectos marginales condicionales para la formación superior') +
  labs(x = 'Nivel de globalización', y = 'Efecto de la ideología',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024), V-Party (2022), y V-Indoc (2024).'
         )) +
  theme_apa() +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        axis.title.x = element_text(size = 12, colour = 'black'),
        axis.title.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'))

cme_h4twJ

load('h2.RData')
h2_alt <-
  h2 |>
  select(country, scode, year, basic, tertiary, KOFGIdj) |>
  group_by(country, scode) |>
  summarise(
    basic_prom1 = mean(basic[year >= 1985 & year <= 1989], na.rm = TRUE),
    basic_prom2 = mean(basic[year >= 1990 & year <= 2010], na.rm = TRUE),
    tertiary_prom1 = mean(tertiary[year >= 1985 & year <= 1989], na.rm = TRUE),
    tertiary_prom2 = mean(tertiary[year >= 1990 & year <= 2010], na.rm = TRUE),
    KOFGIdj_prom1 = mean(KOFGIdj[year >= 1985 & year <= 1989], na.rm = TRUE),
    KOFGIdj_prom2 = mean(KOFGIdj[year >= 1990 & year <= 2010], na.rm = TRUE)
  ) |>
  ungroup()

h2_global <- 
  h2 |>
  filter(year %in% c(1980, 2010)) |>
  filter(!country %in% c("Bolivia", "Ecuador"))

# Convertimos las medias calculadas en formato largo
h2_alt <- h2_alt |>
  pivot_longer(
    cols = starts_with("basic") | starts_with("tertiary") | starts_with("KOFGIdj"),
    names_to = c("variable", "periodo"),
    names_pattern = "(.*)_(.*)",
    values_to = "valor"
  ) |>
  pivot_wider(
    names_from = variable,  # Convertimos las variables en columnas
    values_from = valor  # Los valores de las medias
  )

# Excluyo Bolivia
h2_alt <-
  h2_alt |>
  filter(!country %in% c("Bolivia", "Ecuador"))

h2_alt$KOFGIdj2 <- h2_global$KOFGIdj

plot_semi <-
  h2_alt |>
  filter(!country %in% c("Brazil", "República Dominicana", "Ecuador")) |>
  ggplot(aes(x = KOFGIdj2, y = basic)) +
  geom_point(data = subset(h2_alt, !scode %in% c("CHL", "MEX", "PAR")), 
             color = "black") +
  geom_smooth(method = "lm", color = "darkorange", se = FALSE) +
  geom_text(data = subset(h2_alt, scode %in% c("CHL", "MEX", "PAR")), 
            aes(label = scode, color = scode)) +
  scale_color_manual(values = c("CHL" = "firebrick", 
                                "MEX" = "forestgreen", "PAR" = "dodgerblue")) +
  ggtitle('Globalización y gasto público educativo',
          subtitle = 'Chile, Paraguay y México') +
  labs(x = 'Nivel de globalización', 
       y = 'Subsistema básico\n(gasto % del PIB)',
       caption =
         expression(
           italic('Fuente')*': elaboración propia con base en datos del Banco Mundial (2024).'
         )) +
  theme_apa() +
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.subtitle = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        legend.text = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        legend.position = "none")

plot_semi

plot_final <-
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
  theme(plot.title = element_text(size = 14, colour = 'black'),
        plot.subtitle = element_text(size = 14, colour = 'black'),
        plot.caption = element_text(size = 10, colour = 'black', hjust = 0),
        legend.text = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 10, colour = 'black'),
        axis.text.y = element_text(size = 10, colour = 'black'),
        legend.position = "none")

plot_final