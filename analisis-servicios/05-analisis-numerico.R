# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyselect")
# install.packages("janitor")

library(tidyselect)
library(janitor)

# Se fija el dataset limpio
attach(datos_limpios)

###############################
# Posición: tendencia central #
###############################

# Moda para el tipo de conexión a la red eléctrica
names(sort(table(tipo_conexion_electrica), decreasing = TRUE))[1]

# Mediana de la frecuencia de cortes del suministro eléctrico en verano
cortes_verano_ord <- sort(frecuencia_cortes_de_luz_verano)
cortes_verano_ord[ceiling(length(cortes_verano_ord) / 2)]

# Mediana de la frecuencia de cortes del suministro eléctrico en invierno
cortes_invierno_ord <- sort(frecuencia_cortes_de_luz_invierno)
cortes_invierno_ord[ceiling(length(cortes_invierno_ord) / 2)]

# Mediana de la pérdida de electrodomésticos en el último año a causa de la instalación eléctrica
perdida_electrodomesticos_ord <- sort(perdida_electrodomesticos)
perdida_electrodomesticos_ord[ceiling(length(perdida_electrodomesticos_ord) / 2)]

# Moda para la forma de obtención de agua en la vivienda
names(sort(table(modo_obtencion_agua), decreasing = TRUE))[1]

# Moda para el tipo de plaga
ninguna = ifelse((cucarachas + mosquitos + ratas) == 0, 1, 0)
tabla_plagas <- data.frame(cucarachas, ratas, mosquitos, ninguna)
which.max(colSums(tabla_plagas == 1))

# Moda y porcentajes para el tipo de fuente de energía utilizada para la cocina
tabla_fuentes <- data.frame(gas_natural_en_cocina, gas_envasado_en_cocina, electricidad_en_cocina, leña_carbon_en_cocina, sin_fuente_energia_en_cocina)
which.max(colSums(tabla_fuentes == 1))
sum(gas_natural_en_cocina)/length(gas_natural_en_cocina) # porcentaje que usa gas natural
sum(gas_envasado_en_cocina)/length(gas_envasado_en_cocina) # porcentaje que usa gas envasado

# Moda y porcentajes para el tipo de fuente de energía utilizada para la calefacción
tabla_fuentes <- data.frame(gas_natural_en_calefaccion, gas_envasado_en_calefaccion, electricidad_en_calefaccion, leña_carbon_en_calefaccion, no_tengo_calefaccion, no_necesito_calefaccion)
which.max(colSums(tabla_fuentes == 1))
sum(electricidad_en_calefaccion)/length(electricidad_en_calefaccion) # porcentaje que usa electricidad

# Promedio de número de abonos/prepagos de datos móviles por vivienda
mean(cantidad_abonos_datos_moviles)

# Mediana de la edad del jefe de hogar
median(edad_jefe_hogar)

##############
# Dispersión #
##############

# Coeficiente de variación del número de abonos/prepagos de datos móviles por vivienda
sd(cantidad_abonos_datos_moviles) / mean(cantidad_abonos_datos_moviles)

# Rango intercuartílico de la edad del jefe de hogar
IQR(edad_jefe_hogar)

# Mínimo, máximo, cuartiles y media de la edad del jefe de hogar
summary(edad_jefe_hogar)

# Covarianza y correlación de la relación entre la edad del jefe del hogar y la 
# cantidad de abonos/prepagos de datos móviles por persona
var(edad_jefe_hogar,abonos_prepagos_por_persona) # Covariancia
cor(edad_jefe_hogar,abonos_prepagos_por_persona) # Correlación

