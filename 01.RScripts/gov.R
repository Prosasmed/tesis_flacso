setwd("/Volumes/Pedro Rosas/Tesis doctoral/Tesis/tesis_doctorado/00.Data")


library(tidyverse)
df <- readRDS("V-Dem-CPD-Party-V2.rds")

df <- df %>%
  filter(e_regiongeo %in% c(16, 17, 18, 19)) %>%
  select(country_name, COWcode, country_text_id, country_id, year, v2pashname,
         v2paid, v2paseatshare, v2panumbseat, v2patotalseat, v2pavote, 
         v2paallian, v2panaallian, v2pavallian, v2panoallian, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd)

# Argentina
df_arg <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Argentina",
         year >= 1970,
         v2pagovsup == 0) %>%
  arrange(year)

# Bolivia
df_bol <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Bolivia",
         year >= 1969, 
         v2pagovsup == 0) %>%
  arrange(year) # 1982

# Brazil
df_bra <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  group_by(v2pashname) %>%
  filter(country_name == "Brazil",
         year >= 1970,
         v2pagovsup %in% c(0, 1, 2, 3)) %>%
  arrange(year)

# Chile
df_chi <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Chile",
         year >=1969) %>%
  filter(year %in% c(1989, 1993, 1997, 2001, 2005, 2009, 2013, 2017),
         v2pagovsup == 0) %>%
  arrange(year)

# Colombia
df_col <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Colombia",
         year >= 1970,
         v2pagovsup %in% c(0, 1)) %>%
  arrange(year, v2pagovsup)

# Costa Rica
df_cos <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Costa Rica",
         year >= 1970,
         v2pagovsup == 0) %>%
  arrange(year)

# Republica Dominicana
df_rd <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Dominican Republic",
         year >= 1970,
         v2pagovsup == 0) %>%
  arrange(year)

# Ecuador
df_ecu <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Ecuador",
         year >= 1970,
         v2pagovsup == 0) %>%
  arrange(year)

# El Salvador
df_sav <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "El Salvador",
         year >= 1980,
         v2pagovsup == 0) %>%
  arrange(year)

# Guatemala
df_gua <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Guatemala",
         year >= 1990,
         v2pagovsup == 0) %>%
  arrange(year)

# Honduras
df_hon <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Honduras",
         year >= 1970,
         v2pagovsup == 0) %>%
  arrange(year)

# Mexico
df_mex <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Mexico",
         year >= 1970,
         v2pagovsup == 0) %>%
  arrange(year)

# Nicaragua
df_nic <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Nicaragua",
         year >= 1970,
         v2pagovsup == 0) %>%
  arrange(year)

# Panama
df_pan <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Panama",
         year >= 1980,
         v2pagovsup == 0) %>%
  arrange(year)

# Paraguay
df_par <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Paraguay",
         year >= 1970,
         v2pagovsup == 0) %>%
  arrange(year)

# Peru
df_pru <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Peru",
         year >= 1975,
         v2pagovsup == 0) %>%
  arrange(year)

# Uruguay
df_uru <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Uruguay",
         year >= 1980,
         v2pagovsup == 0) %>%
  arrange(year)

# Venezuela
df_ven <- df %>%
  select(country_name, country_text_id, COWcode, year, v2pashname, v2pagovsup,
         v2pariglef, v2pariglef_mean, v2pariglef_ord, v2pariglef_osp, 
         v2pariglef_sd, v2pariglef_osp_sd) %>%
  filter(country_name == "Venezuela",
         year >= 1968,
         v2pagovsup  == 0) %>%
  arrange(year)


max(df$v2pariglef_mean, na.rm = TRUE)
