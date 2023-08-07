library(eph)
library(tidyverse) 
library(openxlsx)



variables <- c('CODUSU','NRO_HOGAR',
               'REGION', 'AGLOMERADO', 'PONDERA',
               'ANO4', 'TRIMESTRE',
               'CH04', 'CH06',
               'NIVEL_ED', 'CAT_OCUP', 'ESTADO', 'INTENSI',
               'PP04A', # publico o privado?
               'PP04B_COD', # CAES
               'PP04C99', # tamaño establecimiento
               'PP04D_COD', # CNO
               
               # legalidad
               'PP07C', 'PP07G1', 'PP07G2', 'PP07G3', 'PP07G4',
               'PP07H', 'PP07I', 'PP07K',
               
               # ingresos
               'P21', 'RDECOCUR'
               )


## ESTRATEGIAS PARA CONSEGUIR EMPLEO -- sin usar
vars_estrategias <- c('PP02C1', 'PP02C2', 'PP02C3', 'PP02C4',
                      'PP02C5', 'PP02C6', 'PP02C7', 'PP02C8',
                      'PP02H')

##### descargar datos #####

d <- get_microdata(year = 2017:2022,
              trimester = 1:4,
              type = 'individual',
              vars = variables)




A <- d 



###### filtros #####

B <- A %>% filter(REGION == 1 &      # GRAN BUENOS AIRES
                    CH06 >= 16 & CH06 <= 34) %>% # EDAD
  mutate(
    EDAD_CAT = case_when(               # Edad categórica
    CH06 >= 16  & CH06 <= 24 ~ "16 a 24 años",
    CH06 >= 25  & CH06 <= 34 ~ "25 a 34 años",
    TRUE ~ NA_character_   # Valor NA para otros casos de edad
    ),
    asalariado = case_when(
    P21 > 0 & ESTADO == 1 & CAT_OCUP == 3 ~ 1,  # Obreros o empleados con ingresos mayores a 0
    ESTADO == 1 & CAT_OCUP == 3 ~ 0,           # Obreros o empleados sin remuneración
    TRUE ~ NA_integer_                        # No son tenidos en cuenta, valor NA
  )) %>% 
  organize_cno() %>% organize_caes() %>% 
  select(-c(30:32, 34:39))


# chequear que sea Buenos Aires

unique(B$AGLOMERADO)

##### tasas #####

tasas <- B %>%   group_by(ANO4, CH04) %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==2]),
            Subocupados       = Suboc_demandante + Suboc_no_demand ,
            Asalariados       = sum(PONDERA[asalariado == 1], na.rm = T),
            # También podemos llamar a las variables entre comillas, incluyendo nombres compuestos
            # A su vez, podemos utilizar la variable recién creada en la definción de otra varible
            'Tasa Actividad'                  = PEA/Poblacion,
            'Tasa Empleo'                     = Ocupados/Poblacion,
            'Tasa Desocupacion'               = Desocupados/PEA,
            'Tasa Subocupación'               = Subocupados/PEA,
            'Tasa Subocupación demandante'    = Suboc_demandante/PEA,
            'Tasa Subocupación no demandante' = Suboc_no_demand/PEA,
            'Tasa de Asalarizacion'           = Asalariados/Ocupados) %>% 
  select(-c(3:10)) %>% organize_labels()
  
  
  write.xlsx(tasas, file = "tasas.xlsx", rowNames = FALSE)

##### Precariedad #####


E <- B %>% 
  mutate(
    prec_index = case_when(
      ESTADO != 1 ~ NA_integer_,  # Si no está ocupado (ESTADO != 1), asignar NA
      TRUE ~ 0                    # Si está ocupado, asignar 0
    )
  ) %>% 
  mutate(
    prec_index = prec_index + as.integer(INTENSI == 1),   # Sumar 1 si cumple trabajo part-time involuntario
    prec_index = prec_index + as.integer(PP07C == 1),     # Sumar 1 si cumple empleo de duración determinada
    prec_index = prec_index + as.integer(PP07H == 2 & PP07I == 2)  # Sumar 1 si cumple ausencia de descuento jubilatorio
  ) %>% 
  mutate(
    prec_index = prec_index   # Calcular el índice de precariedad dividiendo por 3
  ) %>% 
  mutate(
    precariedad = ifelse(!is.na(prec_index), 
                         ifelse(prec_index > 0.5, 1, 0), 
                         NA_integer_),  # Crear columna "precariedad"
  )


### Chequeo si los NA son iguales
isTRUE(
sum(is.na(E$prec_index)) == sum(is.na(E$precariedad))
)


### calculo tasa precariedad por año y sexo

E %>%  group_by(ANO4, CH04) %>% 
  summarise(Ocupados     = sum(PONDERA[ESTADO == 1]),
                Precarizados = sum(PONDERA[precariedad == 1], na.rm = T),
                Tasa         = Precarizados/Ocupados,
                )


### Recalculo precariedad con dos condiciones cumplidas
Ebis <- E %>% mutate(
  precariedad = ifelse(!is.na(prec_index), 
                       ifelse(prec_index > 1, 1, 0), 
                       NA_integer_)
  )


Ebis %>%  group_by(ANO4, CH04) %>% 
  summarise(Ocupados     = sum(PONDERA[ESTADO == 1]),
            Precarizados = sum(PONDERA[precariedad == 1], na.rm = T),
            Tasa         = Precarizados/Ocupados,
  )

##### Informalidad #####

H <- E %>% 
  mutate(
    info_index = case_when(
      ESTADO != 1 & ESTADO != 2 ~ NA_integer_,  # Si no está Activo (ESTADO != 1 | 2), asignar NA
      TRUE ~ 0                    # Si está activo, asignar 0
    )
  ) %>% 
  mutate(
    info_index = info_index + as.integer(precariedad == 1), # Cumple con la condición de precariedad   # Sumar 1 si cumple trabajo part-time involuntario
    info_index = info_index + as.integer(asalariado == 0),   # es ocupado y no percibe salario
    info_index = info_index + as.integer(PP07G1 == 2),     #  No recibe vacaciones pagas
    info_index = info_index + as.integer(PP07G2 == 2),  # No recibe aguinaldo
    info_index = info_index + as.integer(PP07G3 == 2),   # No cuenta con cobertura por enfermedad
    info_index = info_index + as.integer(PP07G4 == 2),     # No tiene obra social
    info_index = info_index + as.integer(!(PP07K == 1 | PP07K == 3)),  # No recibe recibo de sueldo
  ) %>% 
  mutate(
    informalidad = ifelse(!is.na(info_index), 
                         ifelse(info_index >= 3, 1, 0), 
                         NA_integer_)  # Crear columna "informalidad"
  ) 


H %>%  group_by(ANO4, CH04) %>% 
  summarise(Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            Informal          = sum(PONDERA[informalidad == 1], na.rm = T),
            Tasa              = Informal/PEA)


hist(H$info_index)


##### data viz #####

library(ggplot2)

# Cargar la librería RColorBrewer
library(RColorBrewer)


###### Tasa de informalidad #####


# Calcular la tasa de informalidad por sexo, trimestre y año
tasa_informalidad <- H %>% 
  group_by(ANO4, TRIMESTRE, CH04) %>% 
  summarise(Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Informal = sum(PONDERA[informalidad == 1], na.rm = TRUE),
            Tasa = Informal / PEA)


# Combina las variables TRIMESTRE y ANO4 en una sola columna para el eje x
tasa_informalidad$Periodo <- as.Date(paste(tasa_informalidad$ANO4, tasa_informalidad$TRIMESTRE*3, "01", sep = "-"))


tasa_general <- tasa_informalidad %>% 
  group_by(ANO4, TRIMESTRE) %>% 
  summarise(Tasa_gral = mean(Tasa))

tasa_informalidad <- left_join(tasa_informalidad, tasa_general, 
                               by = c("ANO4", "TRIMESTRE"))



tasa_informalidad <- organize_labels(tasa_informalidad)


# Gráfico de puntos con líneas conectadas
ggplot(tasa_informalidad, aes(x = Periodo, y = Tasa, group = as.factor(CH04), color = as.factor(CH04))) +
  geom_point() +
  geom_line() +
  
  # la linea puede comentarse para hacer el gráfico sin la tasa general
  geom_line(aes(y = Tasa_gral, color = "Z-Tasa General"), linetype = "dashed") +  # Agregar la tercera línea

    labs(x = "Trimestre - Año", y = "Tasa de Informalidad", color = "Sexo",
       caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  ggtitle("Gráfico 1. Tasa de Informalidad por Sexo a través del Tiempo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months")  # Etiquetas y separación de meses



###### Gráfico Calificación Ocupacional ######

# Filtrar las categorías deseadas en CALIFICACION
calificaciones_deseadas <- c("Técnicos", "Profesionales", "Operativos", "No calificados")
H_filtrado <- H %>% 
  filter(CALIFICACION %in% calificaciones_deseadas) %>%
  na.omit()  # Eliminar filas con valores NA en la variable informalidad

# Calcular el porcentaje de formalidad e informalidad en cada categoría de calificación ocupacional

porcentajes <- H_filtrado %>% 
  group_by(ANO4, CALIFICACION, informalidad) %>%
  summarise(count = n()) %>%
  group_by(ANO4, CALIFICACION) %>%
  mutate(percentage = count / sum(count) * 100) %>% 
  filter(ANO4 == 2018 | ANO4 == 2020 |ANO4 == 2022)


# Gráfico de columnas apiladas para Calificación Ocupacional con porcentajes
ggplot(porcentajes, aes(x = CALIFICACION, y = percentage, fill = factor(informalidad))) +
  geom_col(position = "stack") +  # Columnas apiladas
  labs(x = "Calificación Ocupacional", y = "Porcentaje", fill = "Tipo de Empleo",
       caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  ggtitle("Gráfico 2. Incidencia de Informalidad por Calificación Ocupacional") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("0" = "Formal", "1" = "Informal")) +
  facet_wrap(~ANO4)


###### Gráfico  Tamaño del Establecimiento y Sexo #####

# Filtrar las categorías deseadas en PP04C99 (Tamaño del Establecimiento)
tamanios_deseados <- c(1, 2, 3)
H_filtrado_tamanios <- H %>% 
  filter(PP04C99 %in% tamanios_deseados) %>%
  na.omit()  # Eliminar filas con valores NA en la variable informalidad

# Calcular la incidencia de formalidad e informalidad en cada segmento de Tamaño del Establecimiento por sexo y año
incidencia_tamanios_sexo <- H_filtrado_tamanios %>%
  group_by(ANO4, PP04C99, CH04, informalidad) %>%
  summarise(count = n()) %>%
  group_by(ANO4, PP04C99, CH04) %>%
  mutate(percentage = count/sum(count) * 100) %>% organize_labels()

# Ajustar el valor de ANO4 según el filtro seleccionado
filtro_ANO4 <- c(2018, 2022)  # Puedes cambiar el año aquí
incidencia_tamanios_sexo_filtrado <- incidencia_tamanios_sexo %>% 
  filter(ANO4 %in% filtro_ANO4)

# Gráfico de barras agrupadas por Tamaño del Establecimiento y Sexo con filtro seleccionable para ANO4
ggplot(incidencia_tamanios_sexo_filtrado, aes(x = factor(PP04C99), y = percentage, fill = factor(informalidad))) +
  geom_col(position = "dodge") +  # Barras agrupadas
  labs(x = "Tamaño del Establecimiento", y = "Porcentaje", fill = "Tipo de Empleo",
       caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  ggtitle(paste("Gráfico 3. Incidencia de Informalidad según Tamaño del Establecimiento y Sexo - Año", filtro_ANO4)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("0" = "Formal", "1" = "Informal")) +
  facet_wrap(~CH04 + ANO4)  # Generar un gráfico por cada categoría de sexo



par(mfrow = c(1, 2))


###### Gráfico de barras para Rama de Actividad ######

# Filtrar las filas con valores NA en la variable "informalidad"
H_filtrado_actividad <- H %>% na.omit()

# Calcular el porcentaje en cada Rama de Actividad
porcentajes_actividad <- H %>% na.omit() %>%
  group_by(caes_eph_label) %>%
  filter(ANO4 == 2022) %>% 
  summarise(percentage = mean(informalidad, na.rm = TRUE) * 100) %>%
  arrange(desc(percentage))

# Reordenar las filas por el porcentaje de forma descendente
porcentajes_actividad <- porcentajes_actividad %>%
  arrange(desc(percentage))

# Convertir caes_eph_label en factor para establecer el orden en el gráfico
porcentajes_actividad$caes_eph_label <- factor(porcentajes_actividad$caes_eph_label,
                                               levels = porcentajes_actividad$caes_eph_label)

  
  # Combinar las paletas para tener 14 colores en total
  colores <- c(brewer.pal(n = 12, name = "Set3"), 
               brewer.pal(n = 8, name = "Dark2"))
  
  # Gráfico de barras para Rama de Actividad
  ggplot(porcentajes_actividad, aes(x = percentage, y = caes_eph_label, fill = caes_eph_label)) +
    geom_col() +  # Geometría de barras
    labs(x = "Porcentaje", y = "Rama de Actividad", fill = "Rama de Actividad") +
    ggtitle("Gráfico 4. Porcentaje de Informalidad por Rama de Actividad. Año 2022") +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
    scale_fill_manual(values = colores)+  # Utilizar la paleta de colores personalizada
    theme(legend.position = "none")  # Eliminar la leyenda

  
  
###### Gráfico de barras para Nivel Educativo ######

# Calcular el porcentaje de informalidad por sexo, nivel educativo y año
porcentajes_nivel_edu <- H %>% na.omit() %>% # Filtrar las filas con valores NA en la variable "informalidad"
  filter(NIVEL_ED != 9) %>% # Filtrar nivel educativo diferente a categoría 9
  group_by(ANO4, NIVEL_ED, CH04) %>%
  summarise(percentage = mean(informalidad, na.rm = TRUE) * 100) %>% 
  filter(ANO4 == 2018 | ANO4 == 2022) %>% 
  organize_labels()

# Gráfico de barras para Nivel Educativo con barras agrupadas por sexo y espacio entre categorías, facet por ANO4
ggplot(porcentajes_nivel_edu, aes(x = factor(NIVEL_ED), y = percentage, fill = factor(CH04))) +
  geom_col(position = position_dodge(width = 0.7)) +  # Ajuste del ancho y espacio entre barras
  labs(x = "Nivel Educativo", y = "Porcentaje de Informalidad", fill = "Sexo",
       caption = "Fuente: Elaboración propia en base a datos de la EPH") +
  ggtitle("Gráfico 5. Porcentaje de Informalidad por Nivel Educativo y Sexo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ANO4, ncol = 1)  # Generar un gráfico por cada año, una columna por año




##### Panel #####

# Crea una lista vacía para almacenar los data frames
lista_eph <- list()


var_panel <- c('CODUSU','NRO_HOGAR', 'COMPONENTE' ,'ANO4','TRIMESTRE',
              'ESTADO','CAT_OCUP','PONDERA', 'CH04', 'CH06',
              'P21')

# Itera sobre los años y los trimestres para descargar los datos de la EPH
lapply(seq(2021, 2022, 1),  function(año) {
  lapply(1:4, function(trimestre) {
    nombre <- paste0(año, "_t", trimestre)
    datos_eph <- get_microdata(year = año, trimester = trimestre, vars = var_panel)
    lista_eph[[nombre]] <<- datos_eph
  })
})




panel <- organize_panels(lista_eph, 
                         variables = var_panel, 
                         window = "trimestral")

panel <- panel %>% filter(consistencia == TRUE)

##### Transiciones laborales

# Cargar las librerías necesarias
library(networkD3)

# Paso 1: Preparar los datos para el diagrama de Sankey
tabla_estados <- table(panel$ESTADO, panel$ESTADO_t1)
df_sankey <- as.data.frame(as.table(tabla_estados))
names(df_sankey) <- c("Estado", "Estado_t1", "Frecuencia")

# Paso 2: Crear un data frame de nodos que incluya todos los estados presentes en el diagrama
nodes <- data.frame(name = unique(c(df_sankey$Estado, df_sankey$Estado_t1)))

# Paso 3: Asignar identificadores numéricos a los estados para que networkD3 funcione correctamente
nodes$ID <- seq_len(nrow(nodes))
df_sankey$ID_Estado <- match(df_sankey$Estado, nodes$name) - 1
df_sankey$ID_Estado_t1 <- match(df_sankey$Estado_t1, nodes$name) - 1

# Paso 4: Crear el diagrama de Sankey
sankey <- sankeyNetwork(
  Links = df_sankey,
  Nodes = nodes,
  Source = "ID_Estado",
  Target = "ID_Estado_t1",
  Value = "Frecuencia",
  NodeID = "name",
  units = "Frecuencia",
  linkColour = "steelblue",  # Cambiar el color de los enlaces
  nodeColour = "lightblue"
)

# Paso 5: Mostrar el gráfico
sankey


#####################################################################################

# Crea una lista vacía para almacenar los data frames
lista_eph <- list()


# Itera sobre los años y los trimestres para descargar los datos de la EPH
lapply(seq(2017, 2022, 1),  function(año) {
  lapply(1:4, function(trimestre) {
    nombre <- paste0(año, "_t", trimestre)
    datos_eph <- get_microdata(year = año, trimester = trimestre, vars = variables)
    lista_eph[[nombre]] <<- datos_eph
  })
})

## no funciona bien si no están todos los trimestres disponibles
