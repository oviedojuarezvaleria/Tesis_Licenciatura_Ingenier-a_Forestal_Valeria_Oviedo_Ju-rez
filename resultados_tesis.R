# ============================================================
# PROCESAMIENTO DE DATOS - TRABAJO FINAL DE GRADUACIÓN
# Valeria Oviedo Juárez
# 18 de setiembre de 2026
# Versión 1

# ============================================================
# 1. Cargar paquetes
# ============================================================

library(readxl)
library(tidyverse)
library(janitor)
library(writexl)
library(ggplot2)
library(scales)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

# ============================================================
# 2. Importar la base de datos de empresas tratantes
# ============================================================

datos <- read_excel(
  "C:/Users/valeo/Downloads/Resultados/BASE_DATOS_TARIMERAS.xlsx",
  sheet = 3
)


# ============================================================
# 3. Exploración inicial de la base de datos
# ============================================================

glimpse(datos)

dim(datos)

names(datos)

head(datos)


# ============================================================
# 4. Revisión de datos faltantes
# ============================================================

colSums(is.na(datos))


# ============================================================
# 5. Exploración de variables categóricas
# ============================================================

count(datos, Provincia)

count(datos, Estado)

count(datos, Tipo)

count(datos, `Tipo de horno`) %>%
  print(n = Inf)

count(datos, `Tipo de calor`) %>%
  print(n = Inf)

count(datos, `Fuente de calor`) %>%
  print(n = Inf)


# ============================================================
# 6. Exploración de variables operativas
# ============================================================

count(datos, `Cantidad de abanicos`) %>%
  print(n = Inf)

datos %>%
  count(`Tipo de horno`, `Cantidad de abanicos`) %>%
  arrange(`Tipo de horno`, `Cantidad de abanicos`) %>%
  print(n = Inf)


# ============================================================
# 7. Exploración del año y referencia de registros
# ============================================================

count(datos, Año) %>%
  arrange(Año)

count(datos, Tarimera) %>%
  print(n = Inf)


# ============================================================
# 8. Exploración de variables de capacidad
# ============================================================

print(count(datos, Europeas), n = Inf)

print(count(datos, Americanas), n = Inf)

print(count(datos, m3), n = Inf)

datos %>%
  select(Empresa, Europeas, Americanas, m3) %>%
  print(n = Inf)


# ============================================================
# 9. Revisión de información geográfica y localización
# ============================================================

datos %>%
  select(Empresa, Provincia, `Cantón`, Localización) %>%
  print(n = Inf)

summary(datos$X)

summary(datos$Y)

sum(is.na(datos$X))

sum(is.na(datos$Y))


# ============================================================
# 10. Exploración de combinaciones de información de capacidad
# ============================================================

datos %>%
  mutate(
    capacidad_registrada = case_when(
      !is.na(Europeas) & is.na(Americanas) & is.na(m3) ~ "Europeas",
      is.na(Europeas) & !is.na(Americanas) & is.na(m3) ~ "Americanas",
      is.na(Europeas) & is.na(Americanas) & !is.na(m3) ~ "m3",
      !is.na(Europeas) & !is.na(Americanas) & is.na(m3) ~ "Europeas + Americanas",
      !is.na(Europeas) & is.na(Americanas) & !is.na(m3) ~ "Europeas + m3",
      is.na(Europeas) & !is.na(Americanas) & !is.na(m3) ~ "Americanas + m3",
      !is.na(Europeas) & !is.na(Americanas) & !is.na(m3) ~ "Las tres",
      TRUE ~ "Sin información"
    )
  ) %>%
  count(capacidad_registrada)


# ============================================================
# 11. Importar resultados de contacto con las empresas
# ============================================================

resultados <- read_excel(
  "C:/Users/valeo/Downloads/Resultados/Resultados.xlsx"
)

glimpse(resultados)


# ============================================================
# 12. Exploración de los resultados de contacto
# ============================================================

count(resultados, `¿Contestó?`) %>%
  print(n = Inf)

count(resultados, `¿Problemas con hongos?`) %>%
  print(n = Inf)

count(resultados, `¿Aceptó visita?`) %>%
  print(n = Inf)


# ============================================================
# 13. Relación entre respuesta y problemas con hongos reportados
# ============================================================

resultados %>%
  count(`¿Contestó?`, `¿Problemas con hongos?`) %>%
  print(n = Inf)


# ============================================================
# 14. Revisión de la variable de certificación
# ============================================================

sum(is.na(resultados$Certificación))

sum(resultados$Certificación == "-", na.rm = TRUE)

count(resultados, Certificación) %>%
  print(n = Inf)


# ============================================================
# 15. Verificación de correspondencia entre ambas bases
# ============================================================

resultados %>%
  anti_join(datos, by = c("Certificación" = "Código"))

datos %>%
  anti_join(resultados, by = c("Código" = "Certificación"))

# ============================================================
# 16. Revisión y estandarización de nombres de variables
# ============================================================

datos <- datos %>%
  rename(
    ano = Año,
    tarimera = Tarimera,
    n = N,
    tipo_certificacion = Tipo,
    empresa = Empresa,
    codigo = Código,
    cedula_juridica = `Cédula Jurídica`,
    provincia = Provincia,
    canton = Cantón,
    estado = Estado,
    tipo_horno = `Tipo de horno`,
    tipo_calor = `Tipo de calor`,
    cantidad_abanicos = `Cantidad de abanicos`,
    europeas = Europeas,
    americanas = Americanas,
    capacidad_m3 = m3,
    fuente_calor = `Fuente de calor`,
    coordenada_y = Y,
    coordenada_x = X,
    localizacion = Localización
  )

resultados <- resultados %>%
  rename(
    registro = `NA`,
    empresa = Empresa,
    certificacion = Certificación,
    contesto = `¿Contestó?`,
    problemas_hongos = `¿Problemas con hongos?`,
    acepto_visita = `¿Aceptó visita?`
  )

names(datos)
names(resultados)

# ============================================================
# 17. Depuración de valores
# ============================================================

# Estandarizar escritura de la fuente de calor

datos <- datos %>%
  mutate(
    fuente_calor = case_when(
      fuente_calor %in% c(
        "Caldera/leña",
        "Caldera/biomasa",
        "Caldera/Bunker",
        "Caldera",
        "Horno/Leña",
        "Madera"
      ) ~ "Caldera/Leña",
      
      fuente_calor %in% c(
        "Caldera/Agua",
        "Caldera/vapor"
      ) ~ "Caldera/Vapor",
      
      fuente_calor %in% c(
        "Caldera/ aserrin",
        "Aserrin"
      ) ~ "Caldera/Aserrin",
      
      fuente_calor %in% c(
        "Gas LP",
        "Calentador a gas"
      ) ~ "Gas LPG",
      TRUE ~ fuente_calor
    )
  )

datos <- datos %>%
  mutate(
    fuente_calor = na_if(fuente_calor, "-"))

# Unificar los tipos de horno extranjeros

datos <- datos %>%
  mutate(
    tipo_horno = case_when(
      str_detect(tipo_horno, regex("^Extrangera|^Extranjero", ignore_case = TRUE)) ~ "Extranjero",
      TRUE ~ tipo_horno
    )
  )

datos <- datos %>%
  mutate(
    tipo_horno = na_if(tipo_horno, "-"))

# Estandarizar tipo de calor

datos <- datos %>%
  mutate(
    tipo_calor = case_when(
      tipo_calor == "Agua caliente" ~ "Vapor",
      TRUE ~ tipo_calor
    )
  )

datos <- datos %>%
  mutate(
    tipo_calor = na_if(tipo_calor, "-"))

# Estandarizar provincia y cantón

datos <- datos %>%
  mutate(
    provincia = str_squish(provincia),
    canton = str_squish(canton)
  )
sort(unique(datos$provincia))
sort(unique(datos$canton))

datos <- datos %>%
  mutate(
    provincia = na_if(provincia, "-"),
    canton = na_if(canton, "-"))

# Depuración de la variable año

datos <- datos %>%
  mutate(
    ano = na_if(ano, "-")
  )%>%
  rename(
    ano_certificacion = ano
  )

# Depuración de cantidad de abanicos

datos <- datos %>%
  mutate(
    cantidad_abanicos = na_if(cantidad_abanicos, "-"),
    cantidad_abanicos = as.numeric(cantidad_abanicos)
  )

# Depuración de varibales de capacidad

datos <- datos %>%
  mutate(
    # Convertir "-" en valores faltantes
    europeas = na_if(europeas, "-"),
    americanas = na_if(americanas, "-"),
    capacidad_m3 = na_if(capacidad_m3, "-"),
    
    # Corregir errores de formato en capacidad_m3
    capacidad_m3 = str_remove_all(capacidad_m3, '"'),
    
    # Convertir las variables a formato numérico
    europeas = as.numeric(europeas),
    americanas = as.numeric(americanas),
    capacidad_m3 = as.numeric(capacidad_m3),
    
    # Normalizar los valores decimales de capacidad
    capacidad_m3 = round(capacidad_m3, 2)
  )

# Revisión de coordenadas

datos <- datos %>%
  mutate(
    coordenada_x = as.numeric(coordenada_x),
    coordenada_y = as.numeric(coordenada_y)
  )

# Revisión de variables de identificación

# Cédula jurídica
sum(is.na(datos$cedula_juridica))
sum(datos$cedula_juridica == "-", na.rm = TRUE)

datos <- datos %>%
  mutate(
    cedula_juridica = na_if(cedula_juridica, "-")
  )

# Certificación
datos <- datos %>%
  mutate(
    cedula_juridica = na_if(cedula_juridica, "-"),
    n = na_if(n, "-"),
    tipo_certificacion = na_if(tipo_certificacion, "-")
  )

# Variable N
sum(is.na(datos$n))
sum(datos$n == "-", na.rm = TRUE)

datos <- datos %>%
  mutate(
    cedula_juridica = na_if(cedula_juridica, "-"),
    n = na_if(n, "-")
  )

# Depuración de la base resultados
# Quitar valores faltantes

resultados <- resultados %>%
  mutate(
    problemas_hongos = na_if(problemas_hongos, "-"),
    acepto_visita = na_if(acepto_visita, "-")
  )

# ============================================================
# 18. Unir las bases de datos
# ============================================================

datos_final <- datos %>%
  left_join(
    resultados %>%
      select(
        certificacion,
        contesto,
        problemas_hongos,
        acepto_visita
      ),
    by = c("codigo" = "certificacion")
  )


# Comprobar la unión

dim(datos_final)

sum(is.na(datos_final$contesto))
sum(is.na(datos_final$problemas_hongos))
sum(is.na(datos_final$acepto_visita))

# Comprobar que cada código continúe siendo único

nrow(datos_final)
n_distinct(datos_final$codigo)

# ============================================================
# 19. Guardar la base de datos depurada e integrada
# ============================================================

write_xlsx(
  datos_final,
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tabla unida generada/datos_unidos.xlsx"
)

# ============================================================
# 20. Generación de variables derivadas-Conversión de capacidad de pallets a metros cúbicos
# ============================================================

# Volumen de referencia de un pallet europeo
volumen_pallet_europeo <- 1.200 * 0.800 * 0.144

# Volumen de referencia de un pallet americano
volumen_pallet_americano <- 1.219 * 1.016 * 0.141

# Cálculo de capacidad según tipo de pallet

datos_final <- datos_final %>%
  mutate(
    capacidad_europeas_m3 = europeas * volumen_pallet_europeo,
    capacidad_americanas_m3 = americanas * volumen_pallet_americano
  )

# Capacidad total del horno en metros cúbicos

datos_final <- datos_final %>%
  mutate(
    capacidad_total_m3 = case_when(
      
      !is.na(capacidad_m3) ~ capacidad_m3,
      
      !is.na(capacidad_europeas_m3) &
        !is.na(capacidad_americanas_m3) ~
        capacidad_europeas_m3 + capacidad_americanas_m3,
      
      !is.na(capacidad_europeas_m3) &
        is.na(capacidad_americanas_m3) ~
        capacidad_europeas_m3,
      
      is.na(capacidad_europeas_m3) &
        !is.na(capacidad_americanas_m3) ~
        capacidad_americanas_m3,
      
      TRUE ~ NA_real_
    )
  )

datos_final <- datos_final %>%
  mutate(
    capacidad_total_m3 = round(capacidad_total_m3, 2)
  )


# Revisión de variables de capacidad

datos_final %>%
  select(
    empresa,
    europeas,
    capacidad_europeas_m3,
    americanas,
    capacidad_americanas_m3,
    capacidad_m3,
    capacidad_total_m3
  ) %>%
  print(n = Inf)

datos_final %>%
  filter(
    !is.na(europeas) &
      !is.na(americanas)
  ) %>%
  select(
    empresa,
    capacidad_europeas_m3,
    capacidad_americanas_m3,
    capacidad_total_m3
  )

# ============================================================
# 21. Preparación de variables para el análisis
# ============================================================

datos_final <- datos_final %>%
  mutate(
    provincia = as.factor(provincia),
    canton = as.factor(canton),
    tipo_certificacion = as.factor(tipo_certificacion),
    estado = as.factor(estado),
    tipo_horno = as.factor(tipo_horno),
    tipo_calor = as.factor(tipo_calor),
    fuente_calor = as.factor(fuente_calor),
    contesto = as.factor(contesto),
    problemas_hongos = as.factor(problemas_hongos)
  )

# ============================================================
# 22. Guardar la base de datos para análisis
# ============================================================

write_xlsx(
  datos_final,
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tabla unida generada/datos_usar.xlsx"
)

# ============================================================
# 22. Análisis descriptivo

# ============================================================
# 23. Distribución geográfica por provincia y cantón
# ============================================================

tabla_provincia <- datos_final %>%
  count(provincia, name = "n_tratantes") %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  arrange(desc(n_tratantes))

tabla_provincia


# Cantón predominante dentro de cada provincia

tabla_canton <- datos_final %>%
  count(provincia, canton, name = "n_tratantes") %>%
  group_by(provincia) %>%
  mutate(
    porcentaje_provincia = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  ungroup() %>%
  arrange(provincia, desc(n_tratantes))

tabla_canton

# ============================================================
# 24. Guardar tablas de distribución geográfica
# ============================================================

write_xlsx(
  list(
    distribucion_provincia = tabla_provincia,
    distribucion_canton = tabla_canton
  ),
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/tablas_distribucion_geografica.xlsx"
)

# ============================================================
# 25. Características de certificación
# ============================================================

tabla_tipo_certificacion <- datos_final %>%
  filter(!is.na(tipo_certificacion)) %>%
  count(tipo_certificacion, name = "n_tratantes") %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  arrange(desc(n_tratantes))

tabla_tipo_certificacion

# ============================================================
# 26. Estado de las empresas tratantes
# ============================================================

tabla_estado <- datos_final %>%
  count(estado, name = "n_tratantes") %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  arrange(desc(n_tratantes))

tabla_estado

# ============================================================
# 27. Guardar tablas de características de certificación
# ============================================================

write_xlsx(
  list(
    tipo_certificacion = tabla_tipo_certificacion,
    estado = tabla_estado
  ),
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/tablas_caracteristicas_certificacion.xlsx"
)

# ============================================================
# 28. Características de las instalaciones
# ============================================================

tabla_tipo_horno <- datos_final %>%
  count(tipo_horno, name = "n_tratantes") %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  arrange(desc(n_tratantes))

tabla_tipo_horno

# ============================================================
# 29. Tipo de calor
# ============================================================

tabla_tipo_calor <- datos_final %>%
  count(tipo_calor, name = "n_tratantes") %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  arrange(desc(n_tratantes))

tabla_tipo_calor

# ============================================================
# 30. Fuente de calor
# ============================================================

datos_final <- datos_final %>%
  mutate(
    fuente_calor = case_when(
      fuente_calor %in% c(
        "Caldera/Biomasa",
        "Caldera/biomasa"
      ) ~ "Caldera/Leña",
      
      TRUE ~ as.character(fuente_calor)
    ),
    fuente_calor = as.factor(fuente_calor)
  )

tabla_fuente_calor <- datos_final %>%
  count(fuente_calor, name = "n_tratantes") %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  arrange(desc(n_tratantes))

tabla_fuente_calor

# ============================================================
# 31. Cantidad de abanicos
# ============================================================

tabla_abanicos <- datos_final %>%
  count(cantidad_abanicos, name = "n_tratantes") %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  arrange(cantidad_abanicos)

tabla_abanicos

# ============================================================
# 32. Guardar tablas de características de las instalaciones
# ============================================================

write_xlsx(
  list(
    tipo_horno = tabla_tipo_horno,
    tipo_calor = tabla_tipo_calor,
    fuente_calor = tabla_fuente_calor,
    cantidad_abanicos = tabla_abanicos
  ),
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/tablas_caracteristicas_instalaciones.xlsx"
)

# ============================================================
# 33. Capacidad de los hornos
# ============================================================

datos_final <- datos_final %>%
  mutate(
    capacidad_m3 = if_else(
      capacidad_m3 == 1000,
      100,
      capacidad_m3
    )
  )

datos_final <- datos_final %>%
  mutate(
    capacidad_total_m3 = case_when(
      !is.na(capacidad_m3) ~ capacidad_m3,
      !is.na(capacidad_europeas_m3) & !is.na(capacidad_americanas_m3) ~
        capacidad_europeas_m3 + capacidad_americanas_m3,
      !is.na(capacidad_europeas_m3) & is.na(capacidad_americanas_m3) ~
        capacidad_europeas_m3,
      is.na(capacidad_europeas_m3) & !is.na(capacidad_americanas_m3) ~
        capacidad_americanas_m3,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    capacidad_total_m3 = round(capacidad_total_m3, 2)
  )

tabla_capacidad <- tibble(
  estadistico = c(
    "Tratantes con información",
    "Sin información",
    "Mínimo",
    "Primer cuartil (Q1)",
    "Mediana",
    "Media",
    "Tercer cuartil (Q3)",
    "Máximo"
  ),
  valor = c(
    sum(!is.na(datos_final$capacidad_total_m3)),
    sum(is.na(datos_final$capacidad_total_m3)),
    min(datos_final$capacidad_total_m3, na.rm = TRUE),
    quantile(datos_final$capacidad_total_m3, 0.25, na.rm = TRUE),
    median(datos_final$capacidad_total_m3, na.rm = TRUE),
    mean(datos_final$capacidad_total_m3, na.rm = TRUE),
    quantile(datos_final$capacidad_total_m3, 0.75, na.rm = TRUE),
    max(datos_final$capacidad_total_m3, na.rm = TRUE)
  ),
  unidad = c(
    "tratantes",
    "tratantes",
    "m³",
    "m³",
    "m³",
    "m³",
    "m³",
    "m³"
  )
) %>%
  mutate(
    valor = round(as.numeric(valor), 2)
  )

tabla_capacidad

# ============================================================
# 34. Guardar tabla de capacidad de los hornos
# ============================================================

write_xlsx(
  tabla_capacidad,
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/tabla_capacidad_hornos.xlsx"
)

# ============================================================
# 35. Información reportada sobre problemas con hongos
# ============================================================

tabla_respuesta_hongos <- datos_final %>%
  count(contesto, name = "n_tratantes") %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  arrange(desc(n_tratantes))

tabla_respuesta_hongos

# ============================================================
# 36. Problemas con hongos reportados por las empresas
# ============================================================

tabla_problemas_hongos <- datos_final %>%
  filter(!is.na(problemas_hongos)) %>%
  count(problemas_hongos, name = "n_tratantes") %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  arrange(desc(n_tratantes))

tabla_problemas_hongos

# ============================================================
# 37. Guardar tablas de información sobre problemas con hongos
# ============================================================

write_xlsx(
  list(
    respuesta_hongos = tabla_respuesta_hongos,
    problemas_hongos = tabla_problemas_hongos
  ),
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/tablas_problemas_hongos.xlsx"
)

# ============================================================
# 38. Problemas con hongos según provincia
# ============================================================

tabla_hongos_provincia <- datos_final %>%
  filter(!is.na(problemas_hongos)) %>%
  count(provincia, problemas_hongos, name = "n_tratantes") %>%
  group_by(provincia) %>%
  mutate(
    porcentaje = round(
      n_tratantes / sum(n_tratantes) * 100,
      2
    )
  ) %>%
  ungroup() %>%
  arrange(provincia, desc(n_tratantes))

tabla_hongos_provincia

# ============================================================
# 39. Revisión de frecuencias esperadas
# ============================================================

tabla_contingencia <- table(
  datos_final$provincia,
  datos_final$problemas_hongos
)

tabla_contingencia

chisq.test(tabla_contingencia)$expected

# ============================================================
# 40. Exploración de variables numéricas
# ============================================================

# Resumen de variables numéricas

resumen_numericas <- tibble(
  variable = c(
    "Cantidad de abanicos",
    "Capacidad total del horno (m³)",
    "Año de certificación"
  ),
  
  n = c(
    sum(!is.na(datos_final$cantidad_abanicos)),
    sum(!is.na(datos_final$capacidad_total_m3)),
    sum(!is.na(datos_final$ano_certificacion))
  ),
  
  minimo = c(
    min(datos_final$cantidad_abanicos, na.rm = TRUE),
    min(datos_final$capacidad_total_m3, na.rm = TRUE),
    min(datos_final$ano_certificacion, na.rm = TRUE)
  ),
  
  maximo = c(
    max(datos_final$cantidad_abanicos, na.rm = TRUE),
    max(datos_final$capacidad_total_m3, na.rm = TRUE),
    max(datos_final$ano_certificacion, na.rm = TRUE)
  ),
  
  media = c(
    mean(datos_final$cantidad_abanicos, na.rm = TRUE),
    mean(datos_final$capacidad_total_m3, na.rm = TRUE),
    mean(datos_final$ano_certificacion, na.rm = TRUE)
  ),
  
  mediana = c(
    median(datos_final$cantidad_abanicos, na.rm = TRUE),
    median(datos_final$capacidad_total_m3, na.rm = TRUE),
    median(datos_final$ano_certificacion, na.rm = TRUE)
  ),
  
  desviacion_estandar = c(
    sd(datos_final$cantidad_abanicos, na.rm = TRUE),
    sd(datos_final$capacidad_total_m3, na.rm = TRUE),
    sd(datos_final$ano_certificacion, na.rm = TRUE)
  )
)

resumen_numericas

# ============================================================
# 41. Distribución de variables numéricas
# ============================================================
# Revisión de variables numéricas de año de certificación

str(
  datos_final %>%
    select(
      cantidad_abanicos,
      capacidad_total_m3,
      ano_certificacion
    )
)

summary(
  datos_final %>%
    select(
      cantidad_abanicos,
      capacidad_total_m3,
      ano_certificacion
    )
)


# Conversión del año de certificación

datos_final <- datos_final %>%
  mutate(
    ano_certificacion = as.numeric(ano_certificacion)
  )

# Verificar la conversión

str(datos_final$ano_certificacion)

summary(datos_final$ano_certificacion)

par(mfrow = c(1, 3))

hist(
  datos_final$cantidad_abanicos,
  main = "Cantidad de abanicos",
  xlab = "Número de abanicos",
  ylab = "Frecuencia"
)

hist(
  datos_final$capacidad_total_m3,
  main = "Capacidad total del horno",
  xlab = "Capacidad (m³)",
  ylab = "Frecuencia"
)

hist(
  datos_final$ano_certificacion,
  main = "Año de certificación",
  xlab = "Año",
  ylab = "Frecuencia"
)

par(mfrow = c(1, 1))




# ============================================================
# 42. Gráficos Q-Q de variables numéricas
# ============================================================

par(mfrow = c(1, 3))

qqnorm(
  datos_final$cantidad_abanicos,
  main = "Q-Q: cantidad de abanicos"
)
qqline(datos_final$cantidad_abanicos)

qqnorm(
  datos_final$capacidad_total_m3,
  main = "Q-Q: capacidad total"
)
qqline(datos_final$capacidad_total_m3)

qqnorm(
  datos_final$ano_certificacion,
  main = "Q-Q: año de certificación"
)
qqline(datos_final$ano_certificacion)

par(mfrow = c(1, 1))

# ============================================================
# 43. Prueba de normalidad de Shapiro-Wilk
# ============================================================

# Aplicar prueba de Shapiro-Wilk

shapiro_abanicos <- shapiro.test(
  datos_final$cantidad_abanicos
)

shapiro_capacidad <- shapiro.test(
  datos_final$capacidad_total_m3
)

shapiro_ano <- shapiro.test(
  datos_final$ano_certificacion
)


# Crear tabla de resultados

tabla_normalidad <- tibble(
  variable = c(
    "Cantidad de abanicos",
    "Capacidad total del horno (m³)",
    "Año de certificación"
  ),
  
  n = c(
    sum(!is.na(datos_final$cantidad_abanicos)),
    sum(!is.na(datos_final$capacidad_total_m3)),
    sum(!is.na(datos_final$ano_certificacion))
  ),
  
  estadistico_W = c(
    unname(shapiro_abanicos$statistic),
    unname(shapiro_capacidad$statistic),
    unname(shapiro_ano$statistic)
  ),
  
  p_valor = c(
    shapiro_abanicos$p.value,
    shapiro_capacidad$p.value,
    shapiro_ano$p.value
  ),
  
  decision = c(
    ifelse(
      shapiro_abanicos$p.value < 0.05,
      "No normal",
      "Compatible con normalidad"
    ),
    
    ifelse(
      shapiro_capacidad$p.value < 0.05,
      "No normal",
      "Compatible con normalidad"
    ),
    
    ifelse(
      shapiro_ano$p.value < 0.05,
      "No normal",
      "Compatible con normalidad"
    )
  )
)

# Redondear resultados

tabla_normalidad <- tabla_normalidad %>%
  mutate(
    estadistico_W = round(estadistico_W, 4),
    p_valor = round(p_valor, 4)
  )

# Mostrar tabla

tabla_normalidad


# Guardar tabla de normalidad

write_xlsx(
  tabla_normalidad,
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/tabla_prueba_normalidad.xlsx"
)

# ============================================================
# 44. Relación entre capacidad y cantidad de abanicos
# ============================================================

plot(
  datos_final$capacidad_total_m3,
  datos_final$cantidad_abanicos,
  main = "Capacidad del horno y cantidad de abanicos",
  xlab = "Capacidad total del horno (m³)",
  ylab = "Cantidad de abanicos",
  pch = 19
)

# ============================================================
# 45. Relación entre capacidad y año de certificación
# ============================================================

plot(
  datos_final$ano_certificacion,
  datos_final$capacidad_total_m3,
  main = "Año de certificación y capacidad del horno",
  xlab = "Año de certificación",
  ylab = "Capacidad total del horno (m³)",
  pch = 19
)

# ============================================================
# 46. Relación entre cantidad de abanicos y año de certificación
# ============================================================

plot(
  datos_final$ano_certificacion,
  datos_final$cantidad_abanicos,
  main = "Año de certificación y cantidad de abanicos",
  xlab = "Año de certificación",
  ylab = "Cantidad de abanicos",
  pch = 19
)

# ============================================================
# 47. Correlaciones de Spearman
# ============================================================

# Capacidad del horno y cantidad de abanicos


cor_capacidad_abanicos <- cor.test(
  datos_final$capacidad_total_m3,
  datos_final$cantidad_abanicos,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_capacidad_abanicos


# Capacidad del horno y año de certificación


cor_capacidad_ano <- cor.test(
  datos_final$capacidad_total_m3,
  datos_final$ano_certificacion,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_capacidad_ano


# Cantidad de abanicos y año de certificación

cor_abanicos_ano <- cor.test(
  datos_final$cantidad_abanicos,
  datos_final$ano_certificacion,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_abanicos_ano

# ============================================================
# 48. Tabla de correlaciones de Spearman
# ============================================================

tabla_correlaciones <- tibble(
  variable_1 = c(
    "Capacidad total del horno (m³)",
    "Capacidad total del horno (m³)",
    "Cantidad de abanicos"
  ),
  variable_2 = c(
    "Cantidad de abanicos",
    "Año de certificación",
    "Año de certificación"
  ),
  n = c(
    sum(complete.cases(
      datos_final$capacidad_total_m3,
      datos_final$cantidad_abanicos
    )),
    sum(complete.cases(
      datos_final$capacidad_total_m3,
      datos_final$ano_certificacion
    )),
    sum(complete.cases(
      datos_final$cantidad_abanicos,
      datos_final$ano_certificacion
    ))
  ),
  rho_spearman = c(
    unname(cor_capacidad_abanicos$estimate),
    unname(cor_capacidad_ano$estimate),
    unname(cor_abanicos_ano$estimate)
  ),
  p_valor = c(
    cor_capacidad_abanicos$p.value,
    cor_capacidad_ano$p.value,
    cor_abanicos_ano$p.value
  )
)

# Redondear resultados para presentación
tabla_correlaciones <- tabla_correlaciones %>%
  mutate(
    rho_spearman = round(rho_spearman, 3),
    p_valor = round(p_valor, 4)
  )

# Revisar tabla
tabla_correlaciones



# Guardar tabla

write_xlsx(
  tabla_correlaciones,
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/tabla_correlaciones_spearman.xlsx"
)

# ============================================================
# 49. Capacidad del horno según problemas con hongos
# ============================================================

tabla_capacidad_hongos <- datos_final %>%
  filter(
    !is.na(problemas_hongos),
    !is.na(capacidad_total_m3)
  ) %>%
  group_by(problemas_hongos) %>%
  summarise(
    n = n(),
    minimo = min(capacidad_total_m3),
    Q1 = quantile(capacidad_total_m3, 0.25),
    mediana = median(capacidad_total_m3),
    media = mean(capacidad_total_m3),
    Q3 = quantile(capacidad_total_m3, 0.75),
    maximo = max(capacidad_total_m3),
    desviacion_estandar = sd(capacidad_total_m3),
    .groups = "drop"
  )

tabla_capacidad_hongos

# ============================================================
# 50. Valores de capacidad según problemas con hongos
# ============================================================

datos_final %>%
  filter(
    !is.na(problemas_hongos),
    !is.na(capacidad_total_m3)
  ) %>%
  select(
    problemas_hongos,
    capacidad_total_m3
  ) %>%
  arrange(problemas_hongos, capacidad_total_m3)

# ============================================================
# 51. Comparación de capacidad del horno según problemas
#     con hongos
# ============================================================

prueba_capacidad_hongos <- wilcox.test(
  capacidad_total_m3 ~ problemas_hongos,
  data = datos_final %>%
    filter(
      !is.na(problemas_hongos),
      !is.na(capacidad_total_m3)
    ),
  exact = FALSE
)

prueba_capacidad_hongos

# ============================================================
# 52. Guardar comparación de capacidad según problemas
#     con hongos
# ============================================================

tabla_capacidad_hongos_resultado <- datos_final %>%
  filter(
    !is.na(problemas_hongos),
    !is.na(capacidad_total_m3)
  ) %>%
  group_by(problemas_hongos) %>%
  summarise(
    n = n(),
    minimo = min(capacidad_total_m3),
    Q1 = quantile(capacidad_total_m3, 0.25),
    mediana = median(capacidad_total_m3),
    media = mean(capacidad_total_m3),
    Q3 = quantile(capacidad_total_m3, 0.75),
    maximo = max(capacidad_total_m3),
    desviacion_estandar = sd(capacidad_total_m3),
    .groups = "drop"
  )

# Resultado de la prueba de Wilcoxon
resultado_wilcoxon <- data.frame(
  variable = "Capacidad total del horno (m³)",
  comparacion = "Problemas con hongos (Sí vs No)",
  prueba = "Wilcoxon-Mann-Whitney",
  W = unname(prueba_capacidad_hongos$statistic),
  p_valor = prueba_capacidad_hongos$p.value
)

# Guardar ambas tablas en un mismo archivo Excel
write_xlsx(
  list(
    "Comparacion_descriptiva" = tabla_capacidad_hongos_resultado,
    "Prueba_Wilcoxon" = resultado_wilcoxon
  ),
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/tabla_capacidad_hongos.xlsx"
)

# ============================================================
# 53. Cantidad de abanicos según problemas con hongos
# ============================================================

tabla_abanicos_hongos <- datos_final %>%
  filter(
    !is.na(problemas_hongos),
    !is.na(cantidad_abanicos)
  ) %>%
  group_by(problemas_hongos) %>%
  summarise(
    n = n(),
    minimo = min(cantidad_abanicos),
    Q1 = quantile(cantidad_abanicos, 0.25),
    mediana = median(cantidad_abanicos),
    media = mean(cantidad_abanicos),
    Q3 = quantile(cantidad_abanicos, 0.75),
    maximo = max(cantidad_abanicos),
    desviacion_estandar = sd(cantidad_abanicos),
    .groups = "drop"
  )

tabla_abanicos_hongos

# ============================================================
# 54. Distribución de cantidad de abanicos según problemas con hongos
# ============================================================

tabla_frecuencia_abanicos_hongos <- datos_final %>%
  filter(
    !is.na(problemas_hongos),
    !is.na(cantidad_abanicos)
  ) %>%
  count(
    problemas_hongos,
    cantidad_abanicos
  ) %>%
  group_by(problemas_hongos) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  ) %>%
  ungroup()

tabla_frecuencia_abanicos_hongos

# ============================================================
# 55. Comparación de cantidad de abanicos según problemas con hongos
# ============================================================

prueba_abanicos_hongos <- wilcox.test(
  cantidad_abanicos ~ problemas_hongos,
  data = datos_final %>%
    filter(
      !is.na(problemas_hongos),
      !is.na(cantidad_abanicos)
    ),
  exact = FALSE
)

prueba_abanicos_hongos

# ============================================================
# 56. Guardar comparación de cantidad de abanicos según problemas con hongos
# ============================================================

tabla_abanicos_hongos_resultado <- datos_final %>%
  filter(
    !is.na(problemas_hongos),
    !is.na(cantidad_abanicos)
  ) %>%
  group_by(problemas_hongos) %>%
  summarise(
    n = n(),
    minimo = min(cantidad_abanicos),
    Q1 = quantile(cantidad_abanicos, 0.25),
    mediana = median(cantidad_abanicos),
    media = mean(cantidad_abanicos),
    Q3 = quantile(cantidad_abanicos, 0.75),
    maximo = max(cantidad_abanicos),
    desviacion_estandar = sd(cantidad_abanicos),
    .groups = "drop"
  )

# Resultado de la prueba de Wilcoxon
resultado_wilcoxon_abanicos <- data.frame(
  variable = "Cantidad de abanicos",
  comparacion = "Problemas con hongos (Sí vs No)",
  prueba = "Wilcoxon-Mann-Whitney",
  W = unname(prueba_abanicos_hongos$statistic),
  p_valor = prueba_abanicos_hongos$p.value
)

# Guardar ambas tablas
write_xlsx(
  list(
    "Comparacion_descriptiva" = tabla_abanicos_hongos_resultado,
    "Prueba_Wilcoxon" = resultado_wilcoxon_abanicos
  ),
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/tabla_abanicos_hongos.xlsx"
)

# ============================================================
# 57. Figura 1. Distribución geográfica por provincia y cantón
# ============================================================
# Preparar datos

tabla_provincia_canton <- tabla_canton %>%
  rename(
    numero_empresas = n_tratantes
  ) %>%
  mutate(
    canton = as.character(canton)
  )

orden_provincias <- tabla_provincia_canton %>%
  group_by(provincia) %>%
  summarise(
    total = sum(numero_empresas),
    .groups = "drop"
  ) %>%
  arrange(desc(total)) %>%
  pull(provincia)

tabla_provincia_canton <- tabla_provincia_canton %>%
  mutate(
    provincia = factor(
      provincia,
      levels = orden_provincias
    )
  ) %>%
  arrange(
    match(provincia, orden_provincias),
    desc(numero_empresas)
  )


# Orden de los cantones
# Mayor a menor para que el mayor quede arriba

orden_cantones <- tabla_provincia_canton %>%
  arrange(
    desc(numero_empresas),
    canton
  ) %>%
  pull(canton) %>%
  unique()

tabla_provincia_canton <- tabla_provincia_canton %>%
  mutate(
    canton = factor(
      canton,
      levels = orden_cantones
    )
  )


totales_provincia <- tabla_provincia_canton %>%
  group_by(provincia) %>%
  summarise(
    total = sum(numero_empresas),
    .groups = "drop"
  )


# Colores base de las provincias

colores_provincias <- c(
  "Alajuela" = "#00913f",
  "Limón" = "#0833A2",
  "Heredia" = "#E8031B",
  "Cartago" = "#5A1877",
  "Puntarenas" = "#EC7C26",
  "San José" = "#55686F",
  "Guanacaste" = "#ffeb3b"
)


# Crear tonos para cada cantón

colores_cantones <- c()

for (prov in levels(tabla_provincia_canton$provincia)) {
  
  cantones_prov <- tabla_provincia_canton %>%
    filter(provincia == prov) %>%
    arrange(
      desc(numero_empresas),
      canton
    ) %>%
    pull(canton)
  
  n_cantones <- length(cantones_prov)
  
  tonos <- colorRampPalette(
    c(
      colores_provincias[prov],
      "#E8E8E8"
    )
  )(n_cantones + 2)[1:n_cantones]
  
  colores_cantones <- c(
    colores_cantones,
    setNames(
      tonos,
      cantones_prov
    )
  )
}

# Gráfico principal

figura_barras <- ggplot(
  tabla_provincia_canton,
  aes(
    x = provincia,
    y = numero_empresas,
    fill = canton
  )
) +
  geom_col(
    width = 0.72,
    color = "white",
    linewidth = 0.4
  ) +
  geom_text(
    data = totales_provincia,
    aes(
      x = provincia,
      y = total,
      label = total
    ),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 5,
    fontface = "bold",
    family = "Arial"
  ) +
  scale_fill_manual(
    values = colores_cantones
  ) +
  scale_y_continuous(
    breaks = seq(0, 40, 5),
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  labs(
    title = "Distribución de las empresas tratantes por provincia y cantón",
    x = "Provincia",
    y = "Número de empresas"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      family = "Arial"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 14,
      family = "Arial"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 14,
      family = "Arial"
    ),
    axis.text.x = element_text(
      face = "bold",
      size = 14,
      family = "Arial"
    ),
    axis.text.y = element_text(
      size = 13,
      family = "Arial"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.3,
      color = "#000"
    ),
    panel.border = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "none"
  )


# Preparar datos para la leyenda

datos_leyenda <- tabla_provincia_canton %>%
  group_by(provincia) %>%
  arrange(
    desc(numero_empresas),
    .by_group = TRUE
  ) %>%
  mutate(
    fila = row_number(),
    etiqueta = paste0(
      canton,
      " (",
      numero_empresas,
      ")"
    ),
    color = colores_cantones[
      as.character(canton)
    ]
  ) %>%
  ungroup() %>%
  mutate(
    posicion_x = match(
      provincia,
      levels(tabla_provincia_canton$provincia)
    )
  )


# Nombres de las provincias

encabezados_provincia <- tibble(
  posicion_x = 1:7,
  posicion_y = 7,
  provincia = levels(
    tabla_provincia_canton$provincia
  )
)


# Construir leyenda

figura_leyenda <- ggplot() +
  
  # Nombres de las provincias
  geom_text(
    data = encabezados_provincia,
    aes(
      x = posicion_x,
      y = posicion_y,
      label = provincia
    ),
    fontface = "bold",
    size = 4.2,
    family = "Arial"
  ) +
  
  # Cuadros de color
  geom_point(
    data = datos_leyenda,
    aes(
      x = posicion_x - 0.22,
      y = 5.9 - (fila - 1) * 0.72,
      color = color
    ),
    shape = 15,
    size = 4.8,
    show.legend = FALSE
  ) +
  
  # Nombres de los cantones
  geom_text(
    data = datos_leyenda,
    aes(
      x = posicion_x - 0.10,
      y = 5.9 - (fila - 1) * 0.72,
      label = etiqueta
    ),
    hjust = 0,
    size = 4.2,
    family = "Arial"
  ) +
  
  scale_color_identity() +
  
  scale_x_continuous(
    limits = c(0.5, 7.5),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(0.2, 7.5),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_void(
    base_family = "Arial"
  ) +
  
  theme(
    plot.margin = margin(
      0,
      0,
      0,
      0
    )
  )


# Figura completa

figura_1 <- figura_barras /
  figura_leyenda +
  plot_layout(
    heights = c(4, 1.9)
  )

figura_1


# Guardar Figura 1

carpeta_figuras <- "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Figuras_generadas"

if (!dir.exists(carpeta_figuras)) {
  dir.create(
    carpeta_figuras,
    recursive = TRUE
  )
}

ggsave(
  filename = file.path(
    carpeta_figuras,
    "figura_1_distribucion_geografica.png"
  ),
  plot = figura_1,
  width = 16,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ============================================================
# 58. Figura 2. Distribución geográfica de las empresas tratantes
# ============================================================
# Preparar coordenadas de las empresas

datos_mapa <- datos_final %>%
  filter(
    !is.na(coordenada_x),
    !is.na(coordenada_y)
  ) %>%
  st_as_sf(
    coords = c(
      "coordenada_x",
      "coordenada_y"
    ),
    crs = 4326,
    remove = FALSE
  )


# Preparar mapa base de Costa Rica

mapa_costa_rica <- map_data(
  "world",
  region = "Costa Rica"
)


# Colores de las provincias

colores_provincias <- c(
  "Alajuela" = "#00913f",
  "Limón" = "#0833A2",
  "Heredia" = "#E8031B",
  "Cartago" = "#5A1877",
  "Puntarenas" = "#EC7C26",
  "San José" = "#55686F",
  "Guanacaste" = "#ffeb3b"
)


# Crear mapa

figura_2 <- ggplot() +
  
  # Límite de Costa Rica
  geom_polygon(
    data = mapa_costa_rica,
    aes(
      x = long,
      y = lat
    ),
    fill = "grey95",
    color = "grey45",
    linewidth = 0.5
  ) +
  
  # Empresas tratantes
  geom_sf(
    data = datos_mapa,
    aes(
      color = provincia
    ),
    size = 3,
    alpha = 0.9
  ) +
  
  scale_color_manual(
    values = colores_provincias,
    name = "Provincia"
  ) +
  
  coord_sf(
    xlim = c(-86, -82.5),
    ylim = c(8.0, 11.3),
    expand = FALSE
  ) +
  
  labs(
    title = "Distribución geográfica de las empresas tratantes",
    x = "Longitud",
    y = "Latitud"
  ) +
  
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      family = "Arial"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 13,
      family = "Arial"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 13,
      family = "Arial"
    ),
    axis.text = element_text(
      size = 11,
      family = "Arial"
    ),
    legend.title = element_text(
      face = "bold",
      size = 12,
      family = "Arial"
    ),
    legend.text = element_text(
      size = 11,
      family = "Arial"
    ),
    panel.grid.major = element_line(
      color = "grey85",
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      color = "grey60",
      fill = NA,
      linewidth = 0.4
    )
  )


# Mostrar Figura 2

figura_2

# Guardar Figura 2

ggsave(
  filename = file.path(
    carpeta_figuras,
    "figura_2_distribucion_geografica_mapa.png"
  ),
  plot = figura_2,
  width = 10,
  height = 11,
  units = "in",
  dpi = 300,
  bg = "white"
)
# ============================================================
# 59. Figura 3. Características de certificación
# ============================================================
# Preparar datos para tipo de certificación

datos_donut_certificacion <- datos_final %>%
  mutate(
    tipo_certificacion_grafico = case_when(
      is.na(tipo_certificacion) ~ "Sin información",
      TRUE ~ as.character(tipo_certificacion)
    )
  ) %>%
  count(
    tipo_certificacion_grafico,
    name = "n_tratantes"
  ) %>%
  mutate(
    porcentaje = n_tratantes / sum(n_tratantes) * 100,
    etiqueta_porcentaje = if_else(
      porcentaje >= 5,
      paste0(
        round(porcentaje, 1),
        "%"
      ),
      ""
    )
  ) %>%
  mutate(
    tipo_certificacion_grafico = factor(
      tipo_certificacion_grafico,
      levels = c(
        "Certificación",
        "Re-certificación",
        "Sin información"
      )
    )
  )


# Preparar datos para estado

datos_donut_estado <- datos_final %>%
  mutate(
    estado_grafico = case_when(
      estado == "ACTIVO" ~ "Activo",
      estado == "SUSPENDIDO" ~ "Suspendido",
      is.na(estado) | estado == "SIN_INFORMACION" ~ "Sin información",
      TRUE ~ as.character(estado)
    )
  ) %>%
  count(
    estado_grafico,
    name = "n_tratantes"
  ) %>%
  mutate(
    porcentaje = n_tratantes / sum(n_tratantes) * 100,
    etiqueta_porcentaje = if_else(
      porcentaje >= 5,
      paste0(
        round(porcentaje, 1),
        "%"
      ),
      ""
    )
  ) %>%
  mutate(
    estado_grafico = factor(
      estado_grafico,
      levels = c(
        "Activo",
        "Suspendido",
        "Sin información"
      )
    )
  )


# Colores

colores_certificacion <- c(
  "Certificación" = "#00913f",
  "Re-certificación" = "#5CBF80",
  "Sin información" = "#BDBDBD"
)

colores_estado <- c(
  "Activo" = "#00913f",
  "Suspendido" = "#EC7C26",
  "Sin información" = "#BDBDBD"
)


# Donut de tipo de certificación

donut_certificacion <- ggplot(
  datos_donut_certificacion,
  aes(
    x = 2,
    y = n_tratantes,
    fill = tipo_certificacion_grafico
  )
) +
  geom_col(
    width = 1,
    color = "white",
    linewidth = 1
  ) +
  coord_polar(
    theta = "y"
  ) +
  geom_text(
    aes(
      label = etiqueta_porcentaje
    ),
    position = position_stack(
      vjust = 0.5
    ),
    size = 5,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0,
    y = 0,
    label = "78\nempresas",
    size = 5.5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = colores_certificacion
  ) +
  scale_x_continuous(
    limits = c(0, 2.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "A. Tipo de certificación"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5,
      margin = margin(
        t = 0,
        b = -18
      )
    ),
    legend.position = "none",
    plot.margin = margin(
      0,
      5,
      0,
      5
    )
  )


# Donut de estado

donut_estado <- ggplot(
  datos_donut_estado,
  aes(
    x = 2,
    y = n_tratantes,
    fill = estado_grafico
  )
) +
  geom_col(
    width = 1,
    color = "white",
    linewidth = 1
  ) +
  coord_polar(
    theta = "y"
  ) +
  geom_text(
    aes(
      label = etiqueta_porcentaje
    ),
    position = position_stack(
      vjust = 0.5
    ),
    size = 5,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0,
    y = 0,
    label = "78\nempresas",
    size = 5.5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = colores_estado
  ) +
  scale_x_continuous(
    limits = c(0, 2.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "B. Estado de las empresas"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0.5,
      margin = margin(
        t = 0,
        b = -18
      )
    ),
    legend.position = "none",
    plot.margin = margin(
      0,
      5,
      0,
      5
    )
  )


# Preparar simbología de tipo de certificación

simbolos_certificacion <- tibble(
  fila = c(
    1.90,
    1.45,
    1.00
  ),
  categoria = c(
    "Certificación (42; 53,8 %)",
    "Re-certificación (33; 42,3 %)",
    "Sin información (3; 3,8 %)"
  ),
  color = c(
    colores_certificacion["Certificación"],
    colores_certificacion["Re-certificación"],
    colores_certificacion["Sin información"]
  )
)


# Preparar simbología de estado

simbolos_estado <- tibble(
  fila = c(
    1.90,
    1.45,
    1.00
  ),
  categoria = c(
    "Activo (73; 93,6 %)",
    "Suspendido (2; 2,6 %)",
    "Sin información (3; 3,8 %)"
  ),
  color = c(
    colores_estado["Activo"],
    colores_estado["Suspendido"],
    colores_estado["Sin información"]
  )
)


# Simbología de tipo de certificación

leyenda_certificacion <- ggplot(
  simbolos_certificacion
) +
  geom_point(
    aes(
      x = 0.25,
      y = fila,
      color = color
    ),
    shape = 15,
    size = 6,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      x = 0.33,
      y = fila,
      label = categoria
    ),
    hjust = 0,
    size = 4.5
  ) +
  scale_color_identity() +
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(
      0.75,
      2.15
    ),
    expand = c(0, 0)
  ) +
  theme_void() +
  theme(
    plot.margin = margin(
      0,
      10,
      0,
      10
    )
  )


# Simbología de estado

leyenda_estado <- ggplot(
  simbolos_estado
) +
  geom_point(
    aes(
      x = 0.25,
      y = fila,
      color = color
    ),
    shape = 15,
    size = 6,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      x = 0.33,
      y = fila,
      label = categoria
    ),
    hjust = 0,
    size = 4.5
  ) +
  scale_color_identity() +
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(
      0.75,
      2.15
    ),
    expand = c(0, 0)
  ) +
  theme_void() +
  theme(
    plot.margin = margin(
      0,
      10,
      0,
      10
    )
  )


# Figura completa

figura_3 <- wrap_plots(
  donut_certificacion,
  donut_estado,
  leyenda_certificacion,
  leyenda_estado,
  ncol = 2,
  byrow = TRUE,
  heights = c(
    4,
    0.85
  )
) +
  plot_annotation(
    title = "Características de certificación de las empresas tratantes",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        hjust = 0.5,
        margin = margin(
          t = 5,
          b = 20
        )
      )
    )
  )


# Mostrar Figura 3

figura_3


# Guardar Figura 3

ggsave(
  filename = file.path(
    carpeta_figuras,
    "figura_3_caracteristicas_certificacion.png"
  ),
  plot = figura_3,
  width = 14,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ============================================================
# 60. Figura 4. Características de las instalaciones
# ============================================================

# Preparar datos para la figura

tabla_figura4 <- bind_rows(
  tabla_tipo_horno %>%
    transmute(
      caracteristica = "Tipo de horno",
      categoria = as.character(tipo_horno),
      n_tratantes = n_tratantes
    ),
  
  tabla_tipo_calor %>%
    transmute(
      caracteristica = "Tipo de calor",
      categoria = as.character(tipo_calor),
      n_tratantes = n_tratantes
    ),
  
  tabla_fuente_calor %>%
    transmute(
      caracteristica = "Fuente de calor",
      categoria = as.character(fuente_calor),
      n_tratantes = n_tratantes
    ),
  
  tabla_abanicos %>%
    transmute(
      caracteristica = "Cantidad de abanicos",
      categoria = as.character(cantidad_abanicos),
      n_tratantes = n_tratantes
    )
) %>%
  mutate(
    categoria = if_else(
      is.na(categoria),
      "Sin información",
      categoria
    )
  )

# Ordenar categorías dentro de cada característica

tabla_figura4 <- tabla_figura4 %>%
  group_by(caracteristica) %>%
  arrange(desc(n_tratantes), categoria, .by_group = TRUE) %>%
  mutate(
    categoria = factor(
      categoria,
      levels = rev(categoria)
    )
  ) %>%
  ungroup()

# Figura 4

figura4 <- ggplot(
  tabla_figura4,
  aes(
    x = n_tratantes,
    y = categoria
  )
) +
  geom_col(
    fill = "#6B8E23",
    width = 0.7
  ) +
  geom_text(
    aes(
      label = n_tratantes
    ),
    hjust = -0.15,
    size = 3.8
  ) +
  facet_wrap(
    ~ caracteristica,
    scales = "free_y",
    ncol = 2
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    x = "Número de empresas tratantes",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 12,
      color= "#000"
    ),
    axis.text.y = element_text(
      size = 10,
      color = "#000"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.3
    )
  )

figura4

# Guardar Figura 4

ggsave(
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Figuras_generadas/Figura_4_caracteristicas_instalaciones.png",
  figura4,
  width = 11,
  height = 8,
  dpi = 300
)

# ============================================================
# 61. Figura 5. Capacidad de los hornos
# ============================================================

figura5 <- ggplot(
  datos_final %>%
    filter(!is.na(capacidad_total_m3)),
  aes(
    x = "",
    y = capacidad_total_m3
  )
) +
  geom_boxplot(
    fill = "#6B8E23",
    color = "black",
    width = 0.35,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.08,
    height = 0,
    size = 2.2,
    alpha = 0.65
  ) +
  labs(
    title = "Capacidad de los hornos de las empresas tratantes",
    x = "Empresas tratantes",
    y = "Capacidad total de los hornos (m³)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      color = "black",
      face = "bold",
      hjust = 0.5
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.3
    )
  )

figura5

# Guardar Figura 5

ggsave(
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Figuras_generadas/Figura_5_capacidad_hornos2.png",
  figura5,
  width = 10,
  height = 7,
  dpi = 300
)

# ============================================================
# 62. Figura 6. Problemas con hongos reportados por las empresas tratantes
# ============================================================
datos_hongos_general <- tabla_problemas_hongos %>%
  mutate(
    etiqueta = paste0(
      porcentaje,
      " % (n = ",
      n_tratantes,
      ")"
    )
  )

datos_hongos_provincia <- tabla_hongos_provincia %>%
  filter(
    problemas_hongos == "Sí"
  ) %>%
  arrange(
    desc(n_tratantes),
    provincia
  ) %>%
  mutate(
    provincia = factor(
      provincia,
      levels = rev(provincia)
    ),
    etiqueta = paste0(
      n_tratantes,
      " (",
      porcentaje,
      " %)"
    )
  )

# Colores de las provincias utilizados en la Figura 1

colores_provincias <- c(
  "Alajuela" = "#00913f",
  "Limón" = "#0833A2",
  "Heredia" = "#E8031B",
  "Cartago" = "#5A1877",
  "Puntarenas" = "#EC7C26",
  "San José" = "#55686F",
  "Guanacaste" = "#ffeb3b"
)

# Gráfico general

grafico_general_hongos <- ggplot(
  datos_hongos_general,
  aes(
    x = "",
    y = n_tratantes,
    fill = problemas_hongos
  )
) +
  geom_col(
    width = 1,
    color = "white"
  ) +
  coord_polar(
    theta = "y"
  ) +
  geom_text(
    data = datos_hongos_general %>%
      filter(problemas_hongos == "Sí"),
    aes(
      label = etiqueta
    ),
    position = position_stack(
      vjust = 0.5
    ),
    color = "black",
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 1,
    y = 0,
    label = "n = 56\nempresas",
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Sí" = "#00913f",
      "No" = "#B8D9C5"
    ),
    labels = c(
      "No" = "No: 10,7 % (n = 6)",
      "Sí" = "Sí: 89,3 % (n = 50)"
    )
  ) +
  labs(
    fill = "Problemas con hongos"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(
      color = "black",
      face = "bold"
    ),
    legend.text = element_text(
      color = "black"
    )
  )

# Gráfico tipo lollipop por provincia

grafico_provincia_hongos <- ggplot(
  datos_hongos_provincia,
  aes(
    x = n_tratantes,
    y = provincia
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = n_tratantes,
      y = provincia,
      yend = provincia
    ),
    color = "grey75",
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      color = provincia
    ),
    size = 4
  ) +
  geom_text(
    aes(
      x = n_tratantes + 0.5,
      label = etiqueta
    ),
    color = "black",
    size = 3.6,
    fontface = "bold",
    hjust = 0
  ) +
  scale_color_manual(
    values = colores_provincias
  ) +
  scale_x_continuous(
    breaks = seq(0, 25, 5),
    expand = expansion(
      mult = c(0, 0.25)
    )
  ) +
  labs(
    x = "Número de empresas",
    y = "Provincia"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    axis.text.x = element_text(
      color = "black"
    ),
    axis.text.y = element_text(
      color = "black",
      face = "bold"
    ),
    axis.title.x = element_text(
      color = "black"
    ),
    axis.title.y = element_text(
      color = "black"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.3
    ),
    legend.position = "none"
  )

# Figura completa

figura6 <- (
  grafico_general_hongos +
    grafico_provincia_hongos
) +
  plot_annotation(
    title = "Problemas con hongos reportados por las empresas tratantes",
    theme = theme(
      plot.title = element_text(
        color = "black",
        face = "bold",
        hjust = 0.5,
        size = 16
      )
    )
  ) +
  plot_layout(
    widths = c(1, 1.35)
  )

figura6

# Guardar Figura 6

ggsave(
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Figuras_generadas/Figura_6_problemas_hongos.png",
  figura6,
  width = 12,
  height = 7,
  dpi = 300
)

# ============================================================
# 63. 
# ============================================================
