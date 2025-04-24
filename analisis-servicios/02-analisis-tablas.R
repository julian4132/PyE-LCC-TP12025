# Instalar los paquetes necesarios
# install.packages("janitor")
# install.packages("DT")
# install.packages("dplyr")
# install.packages("gridExtra")
# install.packages("grid")
# install.packages("gtable")

# Se cargan los paquetes que se van a usar
library(dplyr)
library(janitor)
library(DT)
library(gridExtra)
library(grid)
library(gtable)


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

# Definir nombres de columnas
colnames(tablaPlagasDesague) <- c("Tipo de desagüe", "Sin plagas", "Con plagas", "Total")

# Primer formato gráfico
datatable(tablaPlagasDesague, rownames = FALSE)

# Segundo formato gráfico
tablaPlagasDesague <- tableGrob(tablaPlagasDesague, rows = NULL)

# Crear un título
titulo <- textGrob("Distribución condicional de plagas según tipo de desagüe.\nBarrios populares de Argentina. Año 2022.", 
                   gp = gpar(fontsize = 16), 
                   y = 0)

# Mostrar el título junto con la tabla
grid.arrange(titulo, tablaPlagasDesague, nrow = 2, heights = c(0.2, 1))




# Relación entre tipo de desagüe y la presencia de cucarachas
tablaCucarachasDesague <-
  datos_limpios %>%
  filter(!is.na(presencia_plagas)) %>%      # Se filtran las observaciones con NA en la variable presencia_plagas
  tabyl(tipo_desague, cucarachas) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1)

# Definir nombres de columnas
colnames(tablaCucarachasDesague) <- c("Tipo de desagüe", "Sin cucarachas", "Con cucarachas", "Total")

# Primer formato gráfico
datatable(tablaCucarachasDesague, rownames = FALSE)

# Segundo formato gráfico
tablaCucarachasDesague <- tableGrob(tablaCucarachasDesague, rows = NULL)

# Crear un título
titulo <- textGrob("Distribución condicional de la presencia de cucarachas según\ntipo de desagüe. Barrios populares de Argentina. Año 2022.", 
                   gp = gpar(fontsize = 16), 
                   y = 0)

# Mostrar el título junto con la tabla
grid.arrange(titulo, tablaCucarachasDesague, nrow = 2, heights = c(0.2, 1))




# Relación entre tipo de desagüe y la presencia de mosquitos
tablaMosquitosDesague <-
  datos_limpios %>%
  filter(!is.na(presencia_plagas)) %>%      # Se filtran las observaciones con NA en la variable presencia_plagas
  tabyl(tipo_desague, mosquitos) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1)

# Definir nombres de columnas
colnames(tablaMosquitosDesague) <- c("Tipo de desagüe", "Sin mosquitos", "Con mosquitos", "Total")

# Primer formato gráfico
datatable(tablaMosquitosDesague, rownames = FALSE)

# Segundo formato gráfico
tablaMosquitosDesague <- tableGrob(tablaMosquitosDesague, rows = NULL)

# Crear un título
titulo <- textGrob("Distribución condicional de la presencia de mosquitos según\ntipo de desagüe. Barrios populares de Argentina. Año 2022.", 
                   gp = gpar(fontsize = 16), 
                   y = 0)

# Mostrar el título junto con la tabla
grid.arrange(titulo, tablaMosquitosDesague, nrow = 2, heights = c(0.2, 1))




# Relación entre tipo de desagüe y la presencia de ratas
tablaRatasDesague <-
  datos_limpios %>%
  filter(!is.na(presencia_plagas)) %>%      # Se filtran las observaciones con NA en la variable presencia_plagas
  tabyl(tipo_desague, ratas) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1)

# Definir nombres de columnas
colnames(tablaRatasDesague) = c("Tipo de desagüe", "Sin ratas", "Con ratas", "Total")

# Primer formato gráfico
datatable(tablaRatasDesague, rownames = FALSE)

# Segundo formato gráfico
tablaRatasDesague <- tableGrob(tablaRatasDesague, rows = NULL)

# Crear un título
titulo <- textGrob("Distribución condicional de la presencia de ratas según\ntipo de desagüe. Barrios populares de Argentina. Año 2022.", 
                   gp = gpar(fontsize = 16), 
                   y = 0)

# Mostrar el título junto con la tabla
grid.arrange(titulo, tablaRatasDesague, nrow = 2, heights = c(0.2, 1))
