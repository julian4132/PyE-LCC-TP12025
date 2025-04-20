# Instalar los paquetes necesarios
# install.packages("tidyverse")

# Se cargan los paquetes que se van a usar
library(tidyverse)

#####################################
# Filtramos las columnas de interés #
#####################################

datos_limpios <- datos[, c(50, 54, 55, 52, 24, 92, 93, 94, 95, 33, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 56, 58, 4, 6)]

######################
# Renombrar columnas #
######################

colnames(datos_limpios) <- c("tipo_conexion_electrica",
                             "frecuencia_cortes_de_luz_verano",
                             "frecuencia_cortes_de_luz_invierno",
                             "perdida_electrodomesticos",
                             "modo_obtencion_agua",
                             "presencia_plagas",
                             "cucarachas",
                             "mosquitos",
                             "ratas",
                             "tipo_desague",
                             "gas_natural_en_cocina",
                             "gas_envasado_en_cocina",
                             "electricidad_en_cocina",
                             "leña_carbon_en_cocina",
                             "sin_fuente_energia_en_cocina",
                             "gas_natural_en_calefaccion",
                             "gas_envasado_en_calefaccion",
                             "electricidad_en_calefaccion",
                             "leña_carbon_en_calefaccion",
                             "no_tengo_calefaccion",
                             "no_necesito_calefaccion",
                             "internet_banda_ancha",
                             "cantidad_abonos_datos_moviles",
                             "edad_jefe_hogar",
                             "cantidad_personas_hogar"
                             )

####################
# Establecer tipos #
####################

# Vector con los tipos de cada variable.
tipos <- c("factor", "factor", "factor", "factor", "factor", 
           "factor", "character", "character", "character", 
           "factor", "character", "character", "character", 
           "character", "character", "character", "character",
           "character", "character", "character", "character", 
           "factor", "numeric", "numeric", "numeric")

# Se genera un vector con las funciones de parseo
conversiones <- lapply(tipos, function(t) match.fun(paste0("as.", t)))

# Se parsean las variables de dataset
datos_limpios[] <- Map(function(col, fun) fun(col), datos_limpios, conversiones)

###################
# Modificar datos #
###################

datos_limpios <- datos_limpios %>% # Los pipelines permiten encadenar acciones
  
  mutate(   # Para crear nuevas variables y editar las ya existentes
    
    # Se recodifica la variable gas_envasado_en_cocina debido a error en el dataset
    gas_envasado_en_cocina = recode(gas_envasado_en_cocina,
                                    "Gas natural (red de gas)" = "Gas envasado (garrafa)"),
    
    # Se crea una nueva variable: número de abonos prepagos por persona
    abonos_prepagos_por_persona = cantidad_abonos_datos_moviles / cantidad_personas_hogar,

    
    # Se modifican las columnas de la variable de respuesta múltiple
    # para dejarlas como indicadoras con valores 1 (en caso de presentar
    # el atributo) y 0 (en caso de no presentarlo)
    cucarachas = ifelse( cucarachas == "Cucarachas", 1, 0),
    mosquitos = ifelse( mosquitos == "Mosquitos", 1, 0),
    ratas = ifelse( ratas == "Ratas", 1, 0),
    
    gas_natural_en_cocina = ifelse( gas_natural_en_cocina == "Gas natural (red de gas)", 1, 0),
    gas_envasado_en_cocina = ifelse( gas_envasado_en_cocina == "Gas envasado (garrafa)", 1, 0),
    electricidad_en_cocina = ifelse( electricidad_en_cocina == "Electricidad", 1, 0),
    leña_carbon_en_cocina = ifelse( leña_carbon_en_cocina == "Leña/Carbón", 1, 0),
    sin_fuente_energia_en_cocina = ifelse( sin_fuente_energia_en_cocina == "No tengo para cocinar en mi vivienda", 1, 0),
    
    gas_natural_en_calefaccion = ifelse( gas_natural_en_calefaccion == "Gas natural (red de gas)", 1, 0),
    gas_envasado_en_calefaccion = ifelse( gas_envasado_en_calefaccion == "Gas envasado (garrafa)", 1, 0),
    electricidad_en_calefaccion = ifelse( electricidad_en_calefaccion == "Electricidad", 1, 0),
    leña_carbon_en_calefaccion = ifelse( leña_carbon_en_calefaccion == "Leña/Carbón", 1, 0),
    no_tengo_calefaccion = ifelse( no_tengo_calefaccion == "No tengo para calefaccionar mi vivienda", 1, 0),
    no_necesito_calefaccion = ifelse ( no_necesito_calefaccion == "No necesito calefaccionar mi vivienda en ninguna época del año", 1, 0),
    
    # Notar que los NA no entran dentro de la categoría "no presentar 
    # el atributo", por lo que requieren un tratamiento particular:
    cucarachas = ifelse(is.na(cucarachas), 0, 1),
    mosquitos = ifelse(is.na(mosquitos), 0, 1),
    ratas = ifelse(is.na(ratas), 0, 1),
    
    gas_natural_en_cocina = ifelse(is.na(gas_natural_en_cocina), 0, 1),
    gas_envasado_en_cocina = ifelse(is.na(gas_envasado_en_cocina), 0, 1),
    electricidad_en_cocina = ifelse(is.na(electricidad_en_cocina), 0, 1),
    leña_carbon_en_cocina = ifelse(is.na(leña_carbon_en_cocina), 0, 1),
    sin_fuente_energia_en_cocina = ifelse(is.na(sin_fuente_energia_en_cocina), 0, 1),
    
    gas_natural_en_calefaccion = ifelse(is.na(gas_natural_en_calefaccion), 0, 1),
    gas_envasado_en_calefaccion = ifelse(is.na(gas_envasado_en_calefaccion), 0, 1),
    electricidad_en_calefaccion = ifelse(is.na(electricidad_en_calefaccion), 0, 1),
    leña_carbon_en_calefaccion = ifelse(is.na(leña_carbon_en_calefaccion), 0, 1),
    no_tengo_calefaccion = ifelse(is.na(no_tengo_calefaccion), 0, 1),
    no_necesito_calefaccion = ifelse(is.na(no_necesito_calefaccion), 0, 1),
    
    # Se recodifican las etiquetas de ciertas variables categóricas
    modo_obtencion_agua = recode(modo_obtencion_agua, "No sabe" = "No sabe",
                                 "A través de una conexión con medidor a la red pública" = "Conexión con medidor a la red pública",
                                 "A través de una conexión sin medidor, es decir “informalmente”, sea a través de una conexión directa a la red pública o a través de una conexión indirecta a través de un vecinx “informalmente”" =  "Conexión sin medidor (conexión informal)",
                                 "A través de un camión cisterna" = "Camión cisterna",
                                 "No poseo agua dentro de la vivienda y/o tengo que acarrear desde fuera del terreno en que se ubica mi vivienda" =  "Sin agua dentro de la vivienda",
                                 "A través de un pozo" = "Pozo",
                                 "Conexión a un tanque comunitario" = "Conexión a un tanque comunitario"),
    
    internet_banda_ancha = recode(internet_banda_ancha,
                                  "No poseo internet de banda ancha" = "No poseo",
                                  "Si inálambrico/satelital" = "Inalámbrico/satelital",
                                  "Si a través de cable (coaxial o ADSL)" = "Por cable",
                                  "Si a través de fibra óptica" = "Por fibra óptica",
                                  "Sí pero no sé qué tipo de servicio tengo en mi vivienda" = "Desconozco el tipo de servicio"),
    
    tipo_desague = recode(tipo_desague,
                          "Desagüe a red cloacal informal/vecinal" = "A red cloacal informal",
                          "Desagüe a red cloacal pública" = "A red cloacal pública",
                          "A cámara séptica" = "A cámara séptica",
                          "A pozo negro/ciego" = "A pozo negro",
                          "No sabe" = "No sabe"),
    
    tipo_conexion_electrica = recode(tipo_conexion_electrica,
                                     "Conexión a través de un medidor a la red eléctrica" = "Medidor",
                                     "Conexión a través de un medidor comunitario a la red eléctrica" = "Medidor comunitario",
                                     "Conexión sin medidor a una red eléctrica (“informal”)" = "Sin medidor",
                                     "No posee conexión a la red eléctrica en la vivienda" = "No posee conexión electrica"),
    
    # Se especifica ordinalidad a las categorías de las variable cualitativas de escala ordinal
    frecuencia_cortes_de_luz_verano = factor(frecuencia_cortes_de_luz_verano,
                                             labels = c("No son frecuentes", "Por lo menos 1 corte en el mes", "Por lo menos 2 cortes en el mes",
                                                        "Por lo menos 3 cortes en el mes", "Más de 4 cortes mensuales"), 
                                             ordered = TRUE),
    
    frecuencia_cortes_de_luz_invierno = factor(frecuencia_cortes_de_luz_invierno,
                                               labels = c("No son frecuentes", "Por lo menos 1 corte en el mes", 
                                                          "Por lo menos 2 cortes en el mes",
                                                          "Por lo menos 3 cortes en el mes", "Más de 4 cortes mensuales"), 
                                               ordered = TRUE),
    
    perdida_electrodomesticos = factor(perdida_electrodomesticos,
                                       labels=c("No", "Si, al menos un electrodoméstico o herramientas de trabajo", 
                                                "Si, al menos dos electrodomésticos", "Si, tres o más"),
                                       ordered = TRUE)
  )

