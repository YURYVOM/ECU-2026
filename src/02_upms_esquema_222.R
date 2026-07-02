############################################################
# Proyecto: Diseño muestral enemdu - Ecuador                 #
#           Esquema de rotación panel 2-2-2               #
#           40 trimestres                                  #
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
library(labelled)
library(writexl)
library(readxl)
library(SamplingCoordination)
select <- dplyr::select

# Lectura de datos --------------------------------------------------------
marco_pareto <- readRDS("output/marco_pareto_new.rds")

muestra_mensual <- readRDS("output/asignacion_muestra_mensual_enemdu.rds")
  

# Rankear por estrato -----------------------------------------------------
marco_pareto <- marco_pareto %>%
  arrange(estrato, Xi_Pareto) %>%
  group_by(estrato) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Rotación teórica 2-2-2 --------------------------------------------------

period <- 40

rotacion_222 <- cbind(
  rotating_panel_222(n_periods = period, value_initial = "A"),
  rotating_panel_222(n_periods =  period, value_initial = "E"),
  rotating_panel_222(n_periods =  period, value_initial = "I")
) %>%
  as.data.frame() %>%
  mutate(trimestre = paste0("Trimestre ", 1:period)) %>%
  pivot_longer(
    cols      = -trimestre,
    names_to  = "posicion",
    values_to = "minipanel_teorico"
  ) %>%
  mutate(
    mes = case_when(
      posicion %in% c("A", "B", "C", "D") ~ "Mes 1",
      posicion %in% c("E", "F", "G", "H") ~ "Mes 2",
      posicion %in% c("I", "J", "K", "L") ~ "Mes 3"
    )
  )

mp_teoricos <- unique(rotacion_222$minipanel_teorico)
length(mp_teoricos)


# Demand table -----------------------------------------------------

demand_table <- muestra_mensual %>%
  mutate(n_por_panel = as.integer(nh / 4)) %>%
  crossing(mes = c("Mes 1", "Mes 2", "Mes 3")) %>%
  mutate(
    letra_base = case_when(
      mes == "Mes 1" ~ list(c("A", "B", "C", "D")),
      mes == "Mes 2" ~ list(c("E", "F", "G", "H")),
      mes == "Mes 3" ~ list(c("I", "J", "K", "L"))
    )
  ) %>%
  unnest(letra_base) %>%
  rename(
    panel_letter = letra_base,
    n_assigned   = n_por_panel
  )

# Asignar UPMs a mini-paneles ---------------------------------------------
marco_con_mp <- bind_rows(lapply(unique(demand_table$estrato), function(est) {
  
  assign_PSUs_to_panels(
    DF              = filter(marco_pareto, estrato == est) %>%
      arrange(rank),
    stratum_column  = "estrato",
    PSU_column      = "id_upm",
    order_column    = "rank",
    demand_table    = filter(demand_table, estrato == est),
    panels_sequence = mp_teoricos
  ) %>%
    rename(minipanel_real = panel) %>%
    mutate(estrato = est,
           esquema = "222")
}))

write_xlsx(marco_con_mp, "output/marco_con_minipaneles_222.xlsx")

# Ajuste cíclico ----------------------------------------------------------

marco_con_mp <- marco_con_mp %>%
  mutate(estrato_nse = "1")

demand_table <- demand_table %>%
  mutate(estrato_nse = "1")

asignacion_ciclica <- cyclic_panel_adjustment(
  assigned_frame   = marco_con_mp %>% rename(panel = minipanel_real),
  demand_table     = demand_table,
  panels_by_scheme = list("222" = mp_teoricos),
  scheme_column    = "esquema",
  geo_column       = "estrato",
  stratum_column = "estrato_nse",
  PSU_column       = "id_upm",
  order_column     = "rank"
) %>%
  rename(minipanel_ciclo = panel_cyclic)

# Matriz final ------------------------------------------------------------

muestra_mensual <- muestra_mensual %>%
  mutate(esquema = "222")

result <- build_panel_matrix(
  assigned_frame      = marco_con_mp,
  cyclic_frame        = asignacion_ciclica,
  rotation_schemes    = list("222" = rotacion_222),
  sample_table        = muestra_mensual,
  period              = period,
  geo_column          = "estrato",
  ses_column          = "estrato",
  psu_column          = "id_upm",
  panel_column        = "minipanel_real",
  cyclic_panel_column = "minipanel_ciclo",
  scheme_column       = "esquema",
  quarter_label       = "trimestre",
  month_label         = "mes",
  theoretical_panel   = "minipanel_teorico"
)

write_xlsx(
  result$panel_matrix,
  "output/upms_seleccionadas_ENEMDU_periodo_intercensal.xlsx"
)

# Verificación ------------------------------------------------------------
result$verification %>% View()

