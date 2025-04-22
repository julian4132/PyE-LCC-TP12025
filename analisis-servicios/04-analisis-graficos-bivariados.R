# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

######################
# Gráficos de Barras #
######################

# Tipo de conexión eléctrica con frecuencia de cortes de suministro eléctrico en verano  
datos_limpios %>%
  filter(tipo_conexion_electrica != "No posee conexión electrica") %>%
  ggplot() + 
  aes(x = tipo_conexion_electrica, fill = frecuencia_cortes_de_luz_verano) +
  labs(x = "Tipo de conexión eléctrica", 
       y = "Frecuencia de cortes de suministro eléctrico", 
       fill = "Frecuencia de cortes de suministro eléctrico") +
  geom_bar(position = "fill") +
  ggtitle("Relación entre el tipo de conexión eléctrica y la frecuencia de cortes\nde suministro eléctrico. Barrios populares de Argentina. Verano 2022.") +
  theme(axis.text.x = element_text(size = 8)) # Tamaño de los valores del eje X

# Tipo de conexión eléctrica con frecuencia de cortes de suministro eléctrico en invierno
datos_limpios %>%
  filter(tipo_conexion_electrica != "No posee conexión electrica") %>%
  ggplot() + 
  aes(x = tipo_conexion_electrica, fill = frecuencia_cortes_de_luz_invierno) +
  labs(x = "Tipo de conexión eléctrica", 
       y = "Frecuencia de cortes de suministro eléctrico", 
       fill = "Frecuencia de cortes de suministro eléctrico") +
  geom_bar(position = "fill") +
  ggtitle("Relación entre el tipo de conexión eléctrica y la frecuencia de cortes\nde suministro eléctrico. Barrios populares de Argentina. Invierno 2022.") +
  theme(axis.text.x = element_text(size = 8)) # Tamaño de los valores del eje X

# Tipo de conexión eléctrica con pérdida de electrodomésticos y/o herramientas de trabajo
datos_limpios %>%
  filter(tipo_conexion_electrica != "No posee conexión electrica") %>%
  ggplot() + 
  aes(x = tipo_conexion_electrica, fill = perdida_electrodomesticos) +
  labs(x = "Tipo de conexión eléctrica", 
       y = "Pérdida de electrodomésticos", 
       fill = "Pérdida de electrodomésticos") +
  geom_bar(position = "fill") +
  ggtitle("Relación entre el tipo de conexión eléctrica y la pérdida de electrodomésticos\ny/o herramientas de trabajo. Barrios populares de Argentina. Año 2022.") +
  theme(axis.text.x = element_text(size = 8)) # Tamaño de los valores del eje X

##########################
# Diagrama de dispersión #
##########################

# Edad jefe del hogar y abonos prepagos por persona
ggplot(datos_limpios) +
  aes(x = edad_jefe_hogar, y = abonos_prepagos_por_persona) +
  geom_point() +
  labs(x = "Edad jefe del hogar", y = "Abonos prepagos por persona") +
  ggtitle("Relación entre la edad del jefe de hogar y la cantidad de abonos prepagos\nde datos móviles por persona. Barrios populares de Argentina. Año 2022.") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título

########################
# Boxplot comparativos #
########################

# Edad del jefe de hogar y tipo de conexión a internet
ggplot(datos_limpios) +
  aes(x = edad_jefe_hogar, y = internet_banda_ancha) +
  geom_boxplot(show.legend = F, fill = "lightblue") +
  labs(x = "Edad del jefe de hogar", y = "Acceso a internet") +
  coord_flip() +
  ggtitle("Relación entre la edad del jefe de hogar y el tipo de conexión a internet.\nBarrios populares de Argentina. Año 2022.") +
  theme_light() +
  scale_x_continuous(breaks = seq(0, 100, 10)) + 
  theme(
    axis.text.x = element_text(size = 8),    # Tamaño de los valores del eje X
    plot.title = element_text(hjust = 0.5)   # Centrar título
  )

  