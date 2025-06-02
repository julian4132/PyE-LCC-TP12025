# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

######################
# Gráficos de Barras #
######################

## Porcentaje de viviendas por cada tipo de conexión eléctrica ##

# Cálculo de porcentajes
porcentajes_tipo_conexion_electrica <- datos_limpios %>%
  count(tipo_conexion_electrica) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Se imprimen los porcentajes exactos
porcentajes_tipo_conexion_electrica

# Gráfico
ggplot(porcentajes_tipo_conexion_electrica) + 
  aes(x = reorder(tipo_conexion_electrica, n), y = porcentaje) + # Ordenar según frecuencia
  geom_bar(stat = "identity",
           width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black") +  # Color de línea
  labs(y = "Porcentaje de viviendas (%)", x = "Tipo de conexión eléctrica") + # Nombres de ejes
  ggtitle("Distribución de hogares de acuerdo al tipo de conexión eléctrica.\nBarrios populares de Argentina. Año 2022.") +
  coord_flip() + # Barras horizontales o verticales
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título


## Porcentaje de frecuencias de cortes de suministro eléctrico en el verano ##

# Cálculo de porcentajes
porcentajes_frecuencia_cortes_de_luz_verano <- datos_limpios %>%
  count(frecuencia_cortes_de_luz_verano) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Se imprimen los porcentajes exactos
porcentajes_frecuencia_cortes_de_luz_verano

# Gráfico
ggplot(porcentajes_frecuencia_cortes_de_luz_verano) + 
  aes(x = frecuencia_cortes_de_luz_verano, y = porcentaje) +
  geom_bar(stat = "identity",
           width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black") +  # Color de línea
  labs(y = "Porcentaje de viviendas (%)", x = "Frecuencias de corte de suministro eléctrico en el verano.") + # Nombres de ejes
  ggtitle("Distribución de hogares según las frecuencias de corte de suministro eléctrico en el verano.\nBarrios populares de Argentina. Año 2022.") +
  scale_y_continuous(
    limits = c(0,40),
    breaks = seq(0, 40, by = 10))  +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(
    axis.text.x = element_text(size = 8),   # Tamaño de los valores del eje X
    plot.title = element_text(hjust = 0.5)  # Centrar título
  )


## Porcentaje de frecuencias de cortes de suministro eléctrico en el invierno ##

# Cálculo de porcentajes
porcentajes_frecuencia_cortes_de_luz_invierno <- datos_limpios %>%
  count(frecuencia_cortes_de_luz_invierno) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Se imprimen los porcentajes exactos
porcentajes_frecuencia_cortes_de_luz_invierno

# Gráfico
ggplot(porcentajes_frecuencia_cortes_de_luz_invierno) + 
  aes(x = frecuencia_cortes_de_luz_invierno, y = porcentaje) +
  geom_bar(stat = "identity",
           width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black") +  # Color de línea
  labs(y = "Porcentaje de viviendas (%)", x = "Frecuencias de corte de suministro eléctrico en el invierno.") + # Nombres de ejes
  ggtitle("Distribución de hogares según las frecuencias de corte de suministro eléctrico en el invierno.\nBarrios populares de Argentina. Año 2022.") +
  scale_y_continuous(
    limits = c(0,40),
    breaks = seq(0, 40, by = 10))  +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(
    axis.text.x = element_text(size = 8),   # Tamaño de los valores del eje X
    plot.title = element_text(hjust = 0.5)  # Centrar título
  )


## Pérdida de electrodomésticos o herramientas de trabajo en el último año a causa de la instalación eléctrica ##

# Cálculo de porcentajes
porcentajes_perdida_electrodomesticos <- datos_limpios %>%
  mutate(
    perdida_electrodomesticos = fct_recode(perdida_electrodomesticos,
                                           "Ninguno" = "No",
                                           "Uno o más" = "Si, al menos un electrodoméstico o herramientas de trabajo",
                                           "Dos o más" = "Si, al menos dos electrodomésticos" ,
                                           "Tres o más" = "Si, tres o más"
    ) #     Renombra los niveles
  ) %>% 
  count(perdida_electrodomesticos) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Se imprimen los porcentajes exactos
porcentajes_perdida_electrodomesticos

# Gráfico
ggplot(porcentajes_perdida_electrodomesticos) + 
  aes(x = perdida_electrodomesticos, y = porcentaje) +
  geom_bar(stat = "identity",
           width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black") +  # Color de línea
  labs(y = "Porcentaje de viviendas (%)", x = "Electrodomésticos perdidos.") + # Nombres de ejes
  ggtitle("Pérdida de electrodomésticos y herramientas de trabajo en el plazo de un año.\nBarrios populares de Argentina. Año 2022.") +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(
    axis.text.x = element_text(size = 8),   # Tamaño de los valores del eje X
    plot.title = element_text(hjust = 0.5)  # Centrar título
  )


## Porcentaje de viviendas por cada método de obtención del agua ##

# Cálculo de porcentajes
porcentajes_modo_obtencion_agua <- datos_limpios %>%
  count(modo_obtencion_agua) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Se imprimen los porcentajes exactos
porcentajes_modo_obtencion_agua

# Gráfico
ggplot(porcentajes_modo_obtencion_agua) + 
  aes(x = reorder(modo_obtencion_agua, n), y = porcentaje) +
  geom_bar(stat = "identity",
           width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black") +  # Color de línea
  labs(y = "Porcentaje de viviendas (%)", x = "Método de obtención de agua") + # Nombres de ejes
  ggtitle("Distribución de hogares en base al método de obtención de agua.\nBarrios populares de Argentina. Año 2022.") +
  coord_flip() + # Barras horizontales o verticales
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(
    axis.text.x = element_text(size = 8),   # Tamaño de los valores del eje X
    plot.title = element_text(hjust = 0.5)  # Centrar título
  )


## Porcentaje de viviendas por cada tipo de desagüe ##

# Cálculo de porcentajes
porcentajes_tipo_desague <- datos_limpios %>%
  count(tipo_desague) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Se imprimen los porcentajes exactos
porcentajes_tipo_desague

# Gráfico
ggplot(porcentajes_tipo_desague) + 
  aes(x = reorder(tipo_desague, n), y = porcentaje) +
  geom_bar(stat = "identity",
           width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black") +  # Color de línea
  labs(y = "Porcentaje de viviendas (%)", x = "Tipo de desagüe") + # Nombres de ejes
  ggtitle("Distribución de hogares en base al tipo de desagüe.\nBarrios populares de Argentina. Año 2022.") +
  coord_flip() + # Barras horizontales o verticales
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(
    axis.text.x = element_text(size = 8),   # Tamaño de los valores del eje X
    plot.title = element_text(hjust = 0.5)  # Centrar título
  )


## Porcentaje de viviendas por cada tipo de plaga ##

# Cálculo de porcentajes
porcentajes_presencia_plagas <-
  datos_limpios %>%
    mutate(
      alguna = cucarachas + mosquitos + ratas,
      ninguna = ifelse(alguna == 0, 1, 0)
    ) %>%
    summarize(cucarachas = (sum(cucarachas)/length(cucarachas))*100,
              mosquitos = (sum(mosquitos)/length(mosquitos))*100,
              ratas = (sum(ratas)/length(ratas))*100,
              ninguna = (sum(ninguna)/length(ninguna))*100) %>%
    pivot_longer(cols = c(cucarachas, mosquitos, ratas, ninguna),
                 names_to = "plaga",
                 values_to = "cant") %>%
    mutate(
      plaga = fct_reorder(plaga, cant),                   # Ordena por frecuencia
      plaga = fct_recode(plaga,
                         Cucarachas = "cucarachas",
                         Mosquitos = "mosquitos",
                         Ratas = "ratas",
                         "Ninguna plaga" = "ninguna")     # Renombra los niveles
    )

# Se imprimen los porcentajes exactos
porcentajes_presencia_plagas

# Gráfico
porcentajes_presencia_plagas %>% 
  ggplot(aes(x = plaga,
             y = cant)) + 
  geom_bar(stat = "identity", # Argumento necesario si partimos de una tabla
           width = 0.75, 
           fill="#8EC6F0",
           col = "black") +
  labs(y = "Porcentaje de viviendas (%)", x = "Presencia de plagas") +
  ggtitle("Distribución de viviendas en base a la presencia de plagas.\nBarrios populares de Argentina. Año 2022.") +
  coord_flip() +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título


# Porcentaje de viviendas por cada tipo de fuente de energía para la cocina

# Cálculo de porcentajes
porcentajes_fuente_energia_cocina <-
  datos_limpios %>%
  mutate(
    alguna = gas_natural_en_cocina + gas_envasado_en_cocina + electricidad_en_cocina + leña_carbon_en_cocina + sin_fuente_energia_en_cocina
  ) %>%
  summarize(gas_natural_en_cocina = (sum(gas_natural_en_cocina)/length(gas_natural_en_cocina))*100,
            gas_envasado_en_cocina = (sum(gas_envasado_en_cocina)/length(gas_envasado_en_cocina))*100,
            electricidad_en_cocina = (sum(electricidad_en_cocina)/length(electricidad_en_cocina))*100,
            leña_carbon_en_cocina = (sum(leña_carbon_en_cocina)/length(leña_carbon_en_cocina))*100,
            sin_fuente_energia_en_cocina = (sum(sin_fuente_energia_en_cocina)/length(sin_fuente_energia_en_cocina))*100) %>%
  pivot_longer(cols = c(gas_natural_en_cocina, gas_envasado_en_cocina, electricidad_en_cocina, leña_carbon_en_cocina, sin_fuente_energia_en_cocina),
               names_to = "fuente_electricidad_cocina",
               values_to = "cant") %>%
  mutate(
    fuente_electricidad_cocina = fct_reorder(fuente_electricidad_cocina, cant),                   # Ordena por frecuencia
    fuente_electricidad_cocina = fct_recode(fuente_electricidad_cocina,
                                            "Gas natural" = "gas_natural_en_cocina",
                                            "Gas envasado" = "gas_envasado_en_cocina",
                                            "Electricidad" = "electricidad_en_cocina",
                                            "Leña/carbón" = "leña_carbon_en_cocina",
                                            "No tengo para cocinar en mi vivienda" = "sin_fuente_energia_en_cocina"
    )     # Renombra los niveles
  )

# Se imprimen los porcentajes exactos
porcentajes_fuente_energia_cocina

# Gráfico
porcentajes_fuente_energia_cocina %>% 
  ggplot(aes(x = fuente_electricidad_cocina,
             y = cant)) + 
  geom_bar(stat = "identity", # Argumento necesario si partimos de una tabla
           width = 0.75, 
           fill="#8EC6F0",
           col = "black") +
  labs(y = "Porcentaje de viviendas (%)", x = "Fuentes de energia para la cocina") +
  ggtitle("Distribución de hogares según fuentes de energía para la cocina.\nBarrios populares de Argentina. Año 2022.") +
  coord_flip() +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título


## Porcentaje de viviendas por cada tipo de fuente de energía para la calefacción ##

# Cálculo de porcentajes
porcentajes_fuente_energia_calefaccion <-
  datos_limpios %>%
  mutate(
    alguna = gas_natural_en_calefaccion + gas_envasado_en_calefaccion + electricidad_en_calefaccion + leña_carbon_en_calefaccion + no_tengo_calefaccion + no_necesito_calefaccion
  ) %>%
  summarize(gas_natural_en_calefaccion = (sum(gas_natural_en_calefaccion)/length(gas_natural_en_calefaccion))*100,
            gas_envasado_en_calefaccion = (sum(gas_envasado_en_calefaccion)/length(gas_envasado_en_calefaccion))*100,
            electricidad_en_calefaccion = (sum(electricidad_en_calefaccion)/length(electricidad_en_calefaccion))*100,
            leña_carbon_en_calefaccion = (sum(leña_carbon_en_calefaccion)/length(leña_carbon_en_calefaccion))*100,
            no_tengo_calefaccion = (sum(no_tengo_calefaccion)/length(no_tengo_calefaccion))*100,
            no_necesito_calefaccion = (sum(no_necesito_calefaccion)/length(no_necesito_calefaccion))*100) %>%
  pivot_longer(cols = c(gas_natural_en_calefaccion, gas_envasado_en_calefaccion, electricidad_en_calefaccion, leña_carbon_en_calefaccion, no_tengo_calefaccion, no_necesito_calefaccion),
               names_to = "fuente_electricidad_calefaccion",
               values_to = "cant") %>%
  mutate(
    fuente_electricidad_calefaccion = fct_reorder(fuente_electricidad_calefaccion, cant),                   # Ordena por frecuencia
    fuente_electricidad_calefaccion = fct_recode(fuente_electricidad_calefaccion,
                                                 "Gas natural" = "gas_natural_en_calefaccion",
                                                 "Gas envasado" = "gas_envasado_en_calefaccion",
                                                 "Electricidad" = "electricidad_en_calefaccion",
                                                 "Leña/carbón" = "leña_carbon_en_calefaccion",
                                                 "No tengo para calefaccionar mi vivienda" = "no_tengo_calefaccion",
                                                 "No necesito calefaccionar mi vivienda" = "no_necesito_calefaccion"
    )     # Renombra los niveles
  ) 

# Se imprimen los porcentajes exactos
porcentajes_fuente_energia_calefaccion

# Gráfico
porcentajes_fuente_energia_calefaccion %>% 
  ggplot(aes(x = fuente_electricidad_calefaccion,
             y = cant)) + 
  geom_bar(stat = "identity", # Argumento necesario si partimos de una tabla
           width = 0.75, 
           fill="#8EC6F0",
           col = "black") +
  labs(y = "Porcentaje de viviendas (%)", x = "Fuentes de energía para calefaccion") +
  ggtitle("Distribución de hogares según fuentes de energía para la calefacción.\nBarrios populares de Argentina. Año 2022.") +
  coord_flip() +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título


## Tipo de conexión a internet ##

# Cálculo de porcentajes
porcentajes_internet_banda_ancha <- datos_limpios %>%
  count(internet_banda_ancha) %>%
  mutate(porcentaje = n / sum(n) * 100)

# Se imprimen los porcentajes exactos
porcentajes_internet_banda_ancha

# Gráfico
ggplot(porcentajes_internet_banda_ancha) + 
  aes(x = reorder(internet_banda_ancha, n), y = porcentaje) + # Ordenar según frecuencia
  geom_bar(stat = "identity",
           width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black") +  # Color de línea
  labs(y = "Porcentaje de viviendas (%)", x = "Tipo de conexión eléctrica") + # Nombres de ejes
  ggtitle("Distribución de hogares por tipo de acceso a internet de banda ancha.\nBarrios populares de Argentina. Año 2022.") +
  coord_flip() + # Barras horizontales o verticales
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título

#######################
# Gráfico de bastones #
#######################

## Número de abonos/prepagos de datos móviles por cada vivienda ##
ggplot(datos_limpios) +
  aes(x = cantidad_abonos_datos_moviles) +
  ggtitle("Distribución de hogares según la cantidad de abonos prepagos de datos móviles.\nBarrios populares de Argentina. Año 2022.") +
  geom_bar(width = 0.10, 
           fill="#8EC6F0",
           col = "black") +
  scale_x_continuous() +
  labs(y = "Número de viviendas", 
       x = "Cantidad de abonos de datos móviles")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título

##############
# Histograma #
##############

## Distribución de la edad del jefe del hogar ##
ggplot(datos_limpios) +
  aes(x = edad_jefe_hogar) +
  ggtitle("Distribución de la edad del jefe de hogar en las viviendas.\nBarrios populares de Argentina. Año 2022.") +
  geom_histogram(fill = "#8EC6F0", col = "black", 
                 breaks = seq(10, 100, 10)) +
  scale_x_continuous(breaks = seq(10, 100, 10)) +
  labs(x = "Edad del jefe de hogar", y = "Número de viviendas") +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título





