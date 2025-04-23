# Instalar los paquetes necesarios
# install.packages("janitor")
# install.packages("DT")
# install.packages("dplyr)

# Se cargan los paquetes que se van a usar
library(dplyr)
library(janitor)
library(DT)

#########################
# Tablas usando janitor #
#########################

# Tablas de contingencia


# Relación entre tipo de desagüe y la presencia de plagas
tablaPlagasDesague <-
  datos_limpios %>%
  filter(!is.na(presencia_plagas)) %>%      # Se filtran las observaciones con NA en la variable presencia_plagas
  tabyl(tipo_desague, presencia_plagas) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1)

# Formato gráfico de la tabla
datatable(tablaPlagasDesague, rownames = FALSE, colnames = c("Tipo de desagüe", "Sin plagas", "Con plagas", "Total"))


# Relación entre tipo de desagüe y la presencia de cucarachas
tablaCucarachasDesague <-
  datos_limpios %>%
  filter(!is.na(presencia_plagas)) %>%      # Se filtran las observaciones con NA en la variable presencia_plagas
  tabyl(tipo_desague, cucarachas) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1)

# Formato gráfico de la tabla
datatable(tablaCucarachasDesague, rownames = FALSE, colnames = c("Tipo de desagüe", "Sin cucarachas", "Con cucarachas", "Total"))


# Relación entre tipo de desagüe y la presencia de mosquitos
tablaMosquitosDesague <-
  datos_limpios %>%
  filter(!is.na(presencia_plagas)) %>%      # Se filtran las observaciones con NA en la variable presencia_plagas
  tabyl(tipo_desague, mosquitos) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1)

# Formato gráfico de la tabla
datatable(tablaMosquitosDesague, rownames = FALSE, colnames = c("Tipo de desagüe", "Sin mosquitos", "Con mosquitos", "Total"))

# Relación entre tipo de desagüe y la presencia de ratas
tablaRatasDesague <-
  datos_limpios %>%
  filter(!is.na(presencia_plagas)) %>%      # Se filtran las observaciones con NA en la variable presencia_plagas
  tabyl(tipo_desague, ratas) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1)

# Formato gráfico de la tabla
datatable(tablaRatasDesague, rownames = FALSE, colnames = c("Tipo de desagüe", "Sin ratas", "Con ratas", "Total"))
