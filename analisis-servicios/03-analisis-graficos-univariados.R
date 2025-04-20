# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

######################
# Gráficos de Barras #
######################

# Número de viviendas por cada tipo de conexión eléctrica
datos_limpios %>%
  ggplot() + 
  aes(x = reorder(tipo_conexion_electrica, tipo_conexion_electrica, function(x) length(x))) + # Ordenar según frecuencia
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  labs(y = "Número de viviendas", x = "Tipo de conexión eléctrica") + # Nombres de ejes
  ggtitle("Acceso a la red eléctrica") +
  coord_flip() + # Barras horizontales o verticales
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título

# Frecuencias de cortes de suministro eléctrico en el verano
datos_limpios %>%
  ggplot() + 
  aes(x = frecuencia_cortes_de_luz_verano) +
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  labs(y = "Número de viviendas", x = "Frecuencias") + # Nombres de ejes
  ggtitle("Frecuencias de corte de suministro eléctrico en el verano") +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(
    axis.text.x = element_text(size = 6),   # Tamaño de los valores del eje X
    plot.title = element_text(hjust = 0.5)  # Centrar título
  ) 
  
# Frecuencias de cortes de suministro eléctrico en el invierno
datos_limpios %>%
  ggplot() + 
  aes(x = frecuencia_cortes_de_luz_invierno) +
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  labs(y = "Número de viviendas", x = "Frecuencias") + # Nombres de ejes
  ggtitle("Frecuencias de corte de suministro eléctrico en el invierno") +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(
    axis.text.x = element_text(size = 6),   # Tamaño de los valores del eje X
    plot.title = element_text(hjust = 0.5)  # Centrar título
    ) 

# Número de viviendas por cada método de obtención del agua
datos_limpios %>%
  ggplot() + 
  aes(x = reorder(modo_obtencion_agua, modo_obtencion_agua, function(x) +length(x))) + # Ordenar según frecuencia
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  labs(y = "Número de viviendas", x = "Método de obtención de agua") + # Nombres de ejes
  ggtitle("Métodos de obtención de agua") +
  coord_flip() + # Barras horizontales o verticales
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título

# Número de viviendas por cada tipo de plaga
datos_limpios %>%
  mutate(
    alguna = cucarachas + mosquitos + ratas,
    ninguna = ifelse(alguna == 0, 1, 0)
  ) %>%
  summarize(cucarachas = sum(cucarachas),
            mosquitos = sum(mosquitos),
            ratas = sum(ratas),
            ninguna = sum(ninguna)) %>%
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
    ) %>% 
  ggplot(aes(x = plaga,
             y = cant)) + 
  geom_bar(stat = "identity", # Argumento necesario si partimos de una tabla
           width = 0.75, 
           fill="#8EC6F0",
           col = "black") +
  labs(y = "Número de viviendas", x = "Presencia de plagas") +
  ggtitle("Presencia de plagas") +
  coord_flip() +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título


# Porcentaje de viviendas por cada tipo de fuente de energía para la cocina
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
  ) %>% 
  ggplot(aes(x = fuente_electricidad_cocina,
             y = cant)) + 
  geom_bar(stat = "identity", # Argumento necesario si partimos de una tabla
           width = 0.75, 
           fill="#8EC6F0",
           col = "black") +
  labs(y = "Porcentaje de viviendas", x = "Fuentes de energia para la cocina") +
  ggtitle("Fuentes de energía para la cocina") +
  coord_flip() +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título

# Porcentaje de viviendas por cada tipo de fuente de energía para la calefacción
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
  ) %>% 
  ggplot(aes(x = fuente_electricidad_calefaccion,
             y = cant)) + 
  geom_bar(stat = "identity", # Argumento necesario si partimos de una tabla
           width = 0.75, 
           fill="#8EC6F0",
           col = "black") +
  labs(y = "Porcentaje de viviendas", x = "Fuentes de energía para calefaccion") +
  ggtitle("Fuentes de energía para la calefacción") +
  coord_flip() +
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título

# Tipo de conexión a internet
datos_limpios %>%
  ggplot() + 
  aes(x = reorder(internet_banda_ancha, internet_banda_ancha, function(x) length(x))) + # Ordenar según frecuencia
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#8EC6F0',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  labs(y = "Número de viviendas", x = "Tipo de conexión a internet") + # Nombres de ejes
  ggtitle("Acceso a internet de banda ancha") +
  coord_flip() + # Barras horizontales o verticales
  theme_classic() + # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título

#######################
# Gráfico de bastones #
#######################

# Número de abonos/prepagos de datos móviles por cada vivienda
ggplot(datos_limpios) +
  aes(x = cantidad_abonos_datos_moviles) +
  ggtitle("Cantidad de abonos de datos móviles en barrios populares de Argentina") +
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

# Distribución de la edad del jefe del hogar
ggplot(datos_limpios) +
  aes(x = edad_jefe_hogar) +
  ggtitle("Edad del jefe de hogar en las viviendas") +
  geom_histogram(fill = "#8EC6F0", col = "black", 
                 breaks = seq(10, 100, 10)) +
  scale_x_continuous(breaks = seq(10, 100, 10)) +
  labs(x = "Edad del jefe de hogar", y = "Número de viviendas") +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título






