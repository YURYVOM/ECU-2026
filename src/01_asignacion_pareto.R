############################################################
# Proyecto: Diseño muestral ENEMDU - Ecuador                 #
#           Esquema de rotación panel                     #
# Autor:    CEPAL                                         #
# Fecha:    2026                                          #
############################################################


# Environment setup -------------------------------------------------------

rm(list = ls(all.names = TRUE))
gc()
options(scipen = 999, digits = 4, stringsAsFactors = FALSE)


# Librerías ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(SamplingCoordination)
select <- dplyr::select


# Lectura de datos --------------------------------------------------------

marco <- readRDS("input/marco_empleo.rds") %>% dplyr::select(id_upm, Mi, pro, area, estrato, nap)

tabla_muestra <- read_excel("input/UPM_tablas.xlsx",
                            sheet = "tabla_estrato") %>%
  mutate(estrato = as.character(estrato))

# Vista del marco ---------------------------------------------------------
labelled::generate_dictionary(marco)

# Muestra mensual por estrato ---------------------------------------------

muestra <- tabla_muestra %>%
  select(estrato, nh) %>%
  mutate(estrato = as.character(estrato))


sum(muestra$nh)


# Revisión Marco ----------------------------------------------------------


marco %>% 
  filter(Mi == 0) %>% 
  select(id_upm, estrato, Mi, nap, pro, area)

marco <- marco %>%
  filter(Mi != 0)

# Verificar
sum(marco$Mi == 0)

# Generar números Pareto por estrato --------------------------------------
set.seed(24062026)

lista_estratos <- split(marco, marco$estrato)

upm_pareto <- lapply(names(lista_estratos), function(est) {
  
  df <- lista_estratos[[est]]
  
  n_sample <- muestra %>%
    filter(estrato == est) %>%
    pull(nh)
  
  if (length(n_sample) == 0 || n_sample == 0) return(NULL)
  
  df_out <- generate_random_frame(
    data     = df,
    id_psu   = id_upm,   
    strata   = estrato,  
    size_var = Mi, 
    permanent_random =  df$nap,
    n_sample = n_sample, 
    method   = "Pareto"
  ) %>%
    arrange(Xi_Pareto)
  
  return(df_out)
})

upm_pareto <- bind_rows(upm_pareto)

upm_pareto <- upm_pareto %>% 
  select(-c("Xi_Perman")) %>%
  bind_rows(
    marco %>% filter(estrato == "2099")
  )
# Exportar salidas --------------------------------------------------------

saveRDS(upm_pareto, "output/marco_pareto_new.rds")
saveRDS(muestra, "output/asignacion_muestra_mensual_enemdu.rds")
