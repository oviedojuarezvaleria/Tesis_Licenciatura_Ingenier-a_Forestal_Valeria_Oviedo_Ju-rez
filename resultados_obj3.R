
# ============================================================
# PROCESAMIENTO DE DATOS - ENSAYO EXPERIMENTAL DE HONGOS
# Trabajo Final de Graduación
# Versión 1
# ============================================================


# ============================================================
# 1. Cargar paquetes
# ============================================================

library(readxl)
library(tidyverse)
library(janitor)
library(writexl)
library(ggplot2)
library(scales)
library(ordinal)
library(lme4)
library(dplyr)
library(purrr)
library(broom.mixed)

# ============================================================
# 2. Importar la base de datos del ensayo experimental
# ============================================================

datos <- read_excel(
  "C:/Users/valeo/Downloads/Resultados/Ensayo/Grados_ensayos.xlsx"
)


# ============================================================
# 3. Exploración inicial de la base de datos
# ============================================================

glimpse(datos)
dim(datos)
names(datos)
head(datos)
tail(datos)
summary(datos)

# ============================================================
# 4. Revisión de datos faltantes
# ============================================================

# Cantidad de datos faltantes por variable
colSums(is.na(datos))

# Porcentaje de datos faltantes por variable
datos %>%
  summarise(across(
    everything(),
    ~ round(sum(is.na(.x)) / n() * 100, 2)
  ))

# Identificar registros que contienen al menos un dato faltante
datos %>%
  filter(if_any(everything(), is.na))

# ============================================================
# 5. Exploración de variables categóricas
# ============================================================

# Frecuencia de registros por hongo
count(datos, Hongo)

# Frecuencia de registros por tratamiento
count(datos, Tratamiento) %>%
  print(n = Inf)

# Frecuencia de registros por combinación de hongo y tratamiento
datos %>%
  count(Hongo, Tratamiento) %>%
  arrange(Hongo, Tratamiento) %>%
  print(n = Inf)

# ============================================================
# 6. Revisión de identificadores y estructura experimental
# ============================================================

# Cantidad de placas por hongo y tratamiento
datos %>%
  distinct(Hongo, Tratamiento, Placa) %>%
  count(Hongo, Tratamiento) %>%
  arrange(Hongo, Tratamiento) %>%
  print(n = Inf)

# Cantidad de probetas por hongo y tratamiento
datos %>%
  distinct(Hongo, Tratamiento, Probeta) %>%
  count(Hongo, Tratamiento) %>%
  arrange(Hongo, Tratamiento) %>%
  print(n = Inf)

# Cantidad de evaluaciones por probeta
datos %>%
  count(Hongo, Tratamiento, Placa, Probeta) %>%
  count(n, name = "Cantidad de probetas") %>%
  arrange(n)

# Revisar registros duplicados
datos %>%
  count(Hongo, Tratamiento, Placa, Probeta, Fecha) %>%
  filter(n > 1)

# ============================================================
# 7. Revisión de fechas y días de evaluación
# ============================================================

# Fechas registradas
datos %>%
  count(Fecha, `Día de evaluación`) %>%
  arrange(`Día de evaluación`)

# Días de evaluación registrados
datos %>%
  count(`Día de evaluación`) %>%
  arrange(`Día de evaluación`)

# Verificar si existe alguna combinación de fecha y día
# de evaluación inconsistente
datos %>%
  distinct(Fecha, `Día de evaluación`) %>%
  arrange(`Día de evaluación`)

# ============================================================
# 8. Revisión de los grados de colonización
# ============================================================

# Valores únicos registrados
sort(unique(datos$`Grado de colonización`))

# Frecuencia de cada grado de colonización
datos %>%
  count(`Grado de colonización`) %>%
  arrange(`Grado de colonización`)

# Verificar valores fuera del rango establecido (0 a 4)
datos %>%
  filter(
    `Grado de colonización` < 0 |
      `Grado de colonización` > 4
  )

# Verificar si existen valores decimales
datos %>%
  filter(
    `Grado de colonización` !=
      floor(`Grado de colonización`)
  )

# ============================================================
# 9. Preparación y transformación de la base de datos
# ============================================================

# Estandarización de nombres de variables
datos <- datos %>%
  clean_names()

# Transformación de variables
datos <- datos %>%
  mutate(
    hongo = as.factor(hongo),
    tratamiento = as.factor(tratamiento),
    placa = as.integer(placa),
    probeta = as.integer(probeta),
    fecha = as.Date(fecha, format = "%d/%m/%Y"),
    dia_de_evaluacion = as.integer(dia_de_evaluacion),
    grado_de_colonizacion = ordered(
      grado_de_colonizacion,
      levels = 0:4
    )
  )

# Revisar estructura de la base transformada
glimpse(datos)

# Revisar resumen de las variables
summary(datos)

# Verificar niveles de la variable ordinal
levels(datos$grado_de_colonizacion)

# ============================================================
# 10. Distribución de los grados de colonización por hongo
# ============================================================

# Frecuencia de cada grado según el hongo
tabla_grados_hongo <- datos %>%
  count(hongo, grado_de_colonizacion) %>%
  group_by(hongo) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  ungroup()

# Visualizar resultados
tabla_grados_hongo %>%
  arrange(hongo, grado_de_colonizacion) %>%
  print(n = Inf)

# ============================================================
# 11. Distribución de los grados de colonización por tratamiento
# ============================================================

# Frecuencia de cada grado según el tratamiento
tabla_grados_tratamiento <- datos %>%
  count(tratamiento, grado_de_colonizacion) %>%
  group_by(tratamiento) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  ungroup()

# Visualizar resultados
tabla_grados_tratamiento %>%
  arrange(tratamiento, grado_de_colonizacion) %>%
  print(n = Inf)

# ============================================================
# 12. Distribución de los grados por tratamiento y hongo
# ============================================================

# Frecuencia de cada grado por tratamiento y hongo
tabla_grados_tratamiento_hongo <- datos %>%
  count(hongo, tratamiento, grado_de_colonizacion) %>%
  group_by(hongo, tratamiento) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  ungroup()

# Visualizar resultados
tabla_grados_tratamiento_hongo %>%
  arrange(hongo, tratamiento, grado_de_colonizacion) %>%
  print(n = Inf)

# ============================================================
# 13. Evolución de los grados de colonización según el día
# ============================================================

# Frecuencia de cada grado por hongo y día de evaluación
tabla_evolucion_hongo <- datos %>%
  count(hongo, dia_de_evaluacion, grado_de_colonizacion) %>%
  group_by(hongo, dia_de_evaluacion) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  ungroup()

# Visualizar resultados
tabla_evolucion_hongo %>%
  arrange(hongo, dia_de_evaluacion, grado_de_colonizacion) %>%
  print(n = Inf)

# ============================================================
# 14. Evolución de los grados por tratamiento, hongo y fecha
# ============================================================

# Frecuencia de cada grado por hongo, tratamiento y día
tabla_evolucion_tratamiento <- datos %>%
  count(
    hongo,
    tratamiento,
    dia_de_evaluacion,
    grado_de_colonizacion
  ) %>%
  group_by(hongo, tratamiento, dia_de_evaluacion) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  ungroup()

# Visualizar resultados
tabla_evolucion_tratamiento %>%
  arrange(
    hongo,
    tratamiento,
    dia_de_evaluacion,
    grado_de_colonizacion
  ) %>%
  print(n = Inf)

# ============================================================
# 15. Distribución de los grados por placa y fecha
# ============================================================

# Frecuencia de los grados de colonización por placa
tabla_grados_placa <- datos %>%
  count(
    hongo,
    tratamiento,
    placa,
    dia_de_evaluacion,
    grado_de_colonizacion
  ) %>%
  arrange(
    hongo,
    tratamiento,
    placa,
    dia_de_evaluacion,
    grado_de_colonizacion
  )

# Visualizar resultados
tabla_grados_placa %>%
  print(n = Inf)

# ============================================================
# 16. Variabilidad entre probetas de una misma placa
# ============================================================

# Identificar cuántos grados diferentes presenta cada placa
variabilidad_placa <- datos %>%
  group_by(
    hongo,
    tratamiento,
    placa,
    dia_de_evaluacion
  ) %>%
  summarise(
    grados_diferentes = n_distinct(grado_de_colonizacion),
    .groups = "drop"
  )

# Frecuencia de placas según cantidad de grados diferentes
variabilidad_placa %>%
  count(grados_diferentes) %>%
  arrange(grados_diferentes)

# Revisar distribución por hongo y tratamiento
variabilidad_placa %>%
  count(hongo, tratamiento, grados_diferentes) %>%
  arrange(hongo, tratamiento, grados_diferentes) %>%
  print(n = Inf)

# ============================================================
# 17. Preparación de una base resumida por placa y fecha
# ============================================================

# Resumen de la distribución de grados por placa
resumen_placa <- datos %>%
  group_by(
    hongo,
    tratamiento,
    placa,
    dia_de_evaluacion
  ) %>%
  summarise(
    total_probetas = n(),
    
    grado_0 = sum(grado_de_colonizacion == 0),
    grado_1 = sum(grado_de_colonizacion == 1),
    grado_2 = sum(grado_de_colonizacion == 2),
    grado_3 = sum(grado_de_colonizacion == 3),
    grado_4 = sum(grado_de_colonizacion == 4),
    
    sin_crecimiento = sum(grado_de_colonizacion == 0),
    con_crecimiento = sum(grado_de_colonizacion > 0),
    
    .groups = "drop"
  )

# Revisar estructura de la nueva base
glimpse(resumen_placa)

# Revisar los primeros registros
head(resumen_placa)

# Verificar cantidad de registros
dim(resumen_placa)

# Verificar que cada placa y fecha tenga dos probetas
count(resumen_placa, total_probetas)

# ============================================================
# 18. Proporción de probetas con crecimiento visible
# ============================================================

resumen_placa <- resumen_placa %>%
  mutate(
    proporcion_crecimiento = con_crecimiento / total_probetas,
    porcentaje_crecimiento = round(
      proporcion_crecimiento * 100, 2
    )
  )

# Revisar los resultados
resumen_placa %>%
  select(
    hongo,
    tratamiento,
    placa,
    dia_de_evaluacion,
    con_crecimiento,
    sin_crecimiento,
    porcentaje_crecimiento
  ) %>%
  print(n = Inf)

# Resumen general por hongo, tratamiento y fecha
resumen_crecimiento <- resumen_placa %>%
  group_by(hongo, tratamiento, dia_de_evaluacion) %>%
  summarise(
    placas = n(),
    porcentaje_promedio = round(
      mean(porcentaje_crecimiento), 2
    ),
    .groups = "drop"
  )

# Visualizar resultados
resumen_crecimiento %>%
  arrange(hongo, tratamiento, dia_de_evaluacion) %>%
  print(n = Inf)

# ============================================================
# 19. Resumen de la proporción de crecimiento por tratamiento
# ============================================================

# Calcular promedio y desviación estándar entre placas
resumen_crecimiento <- resumen_placa %>%
  group_by(hongo, tratamiento, dia_de_evaluacion) %>%
  summarise(
    numero_placas = n(),
    promedio_crecimiento = mean(porcentaje_crecimiento),
    desviacion_estandar = sd(porcentaje_crecimiento),
    .groups = "drop"
  )

# Redondear resultados
resumen_crecimiento <- resumen_crecimiento %>%
  mutate(
    promedio_crecimiento = round(promedio_crecimiento, 2),
    desviacion_estandar = round(desviacion_estandar, 2)
  )

# Visualizar tabla
resumen_crecimiento %>%
  arrange(hongo, tratamiento, dia_de_evaluacion) %>%
  print(n = Inf)

# ============================================================
# 19.1. Visualización de la desviación estándar
# ============================================================

resumen_crecimiento %>%
  select(
    hongo,
    tratamiento,
    dia_de_evaluacion,
    promedio_crecimiento,
    desviacion_estandar
  ) %>%
  arrange(hongo, tratamiento, dia_de_evaluacion) %>%
  print(n = Inf)

# ============================================================
# 19.2. Mostrar todos los resultados de la tabla
# ============================================================

print(
  resumen_crecimiento %>%
    select(
      hongo,
      tratamiento,
      dia_de_evaluacion,
      promedio_crecimiento,
      desviacion_estandar
    ) %>%
    arrange(hongo, tratamiento, dia_de_evaluacion),
  n = Inf,
  width = Inf
)

# ============================================================
# 20. Figura. Evolución del crecimiento visible
# ============================================================

# Colores acordados para los tratamientos

colores_tratamientos <- c(
  "Carbonato de sodio" = "#EC7C26",
  "Ceratol" = "#5A1877",
  "Control" = "#55686F",
  "Fytosan" = "#0833A2",
  "KN-8010" = "#00913f"
)

# Símbolos acordados para los tratamientos

simbolos_tratamientos <- c(
  "Carbonato de sodio" = 16,
  "Ceratol" = 17,
  "Control" = 15,
  "Fytosan" = 18,
  "KN-8010" = 20
)

# Construcción del gráfico

grafico_crecimiento <- ggplot(
  resumen_crecimiento,
  aes(
    x = dia_de_evaluacion,
    y = promedio_crecimiento,
    color = tratamiento,
    shape = tratamiento,
    group = tratamiento
  )
) +
  
  # Líneas de evolución
  geom_line(linewidth = 0.9) +
  
  # Puntos de cada evaluación
  geom_point(size = 3) +
  
  # Separación por hongo y nombres en la parte inferior
  facet_wrap(
    ~ hongo,
    nrow = 1,
    strip.position = "bottom"
  ) +
  
  # Colores y símbolos
  scale_color_manual(
    values = colores_tratamientos
  ) +
  
  scale_shape_manual(
    values = simbolos_tratamientos
  ) +
  
  # Ejes
  scale_x_continuous(
    breaks = c(13, 16, 20)
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  
  # Títulos y etiquetas
  labs(
    title = "Evolución del crecimiento visible en las probetas",
    x = "Día de evaluación",
    y = "Probetas con crecimiento visible",
    color = "Tratamiento",
    shape = "Tratamiento"
  ) +
  
  # Formato general
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    ),
    
    axis.title.x = element_text(
      face = "bold",
      size = 14
    ),
    
    axis.title.y = element_text(
      face = "bold",
      size = 14
    ),
    
    axis.text = element_text(
      color = "black"
    ),
    
    # Nombres de hongos debajo de cada panel
    strip.placement = "outside",
    
    strip.text = element_text(
      face = "italic",
      size = 12
    ),
    
    strip.background = element_blank(),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.3
    )
  )

# Mostrar gráfico
grafico_crecimiento

# Guardar figura
ggsave(
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Figuras_generadas/Figura_evolucion_crecimiento.png",
  grafico_crecimiento,
  width = 12,
  height = 7,
  dpi = 300
)

# ============================================================
# 21. CUADRO 1. Crecimiento visible por hongos y tratamiento
# ============================================================

cuadro_1 <- resumen_crecimiento %>%
  mutate(
    promedio_de = paste0(
      round(promedio_crecimiento, 1),
      " ± ",
      round(desviacion_estandar, 1)
    )
  ) %>%
  select(
    hongo,
    tratamiento,
    dia_de_evaluacion,
    promedio_de
  ) %>%
  pivot_wider(
    names_from = dia_de_evaluacion,
    values_from = promedio_de,
    names_prefix = "Día "
  ) %>%
  arrange(
    hongo,
    tratamiento
  ) %>%
  rename(
    Hongo = hongo,
    Tratamiento = tratamiento
  )

# visualizar cuadro
print(cuadro_1)

# guardar cuadro
write_xlsx(
  cuadro_1,
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/Cuadro_1_crecimiento_visible.xlsx"
)

# ============================================================
# 22. cuadro 2. Frecuencia y porcentaje de observaciones según el grado de colonización por hongo y tratamiento.
# ============================================================

cuadro_2 <- datos %>%
  mutate(
    grado_de_colonizacion = as.character(grado_de_colonizacion)
  ) %>%
  count(
    hongo,
    tratamiento,
    grado_de_colonizacion,
    name = "frecuencia"
  ) %>%
  complete(
    hongo,
    tratamiento,
    grado_de_colonizacion = as.character(0:4),
    fill = list(frecuencia = 0)
  ) %>%
  group_by(hongo, tratamiento) %>%
  mutate(
    porcentaje = frecuencia / sum(frecuencia) * 100,
    frecuencia_porcentaje = paste0(
      frecuencia,
      " (",
      sub(",", ".", format(round(porcentaje, 1), nsmall = 1)),
      " %)"
    )
  ) %>%
  ungroup() %>%
  select(
    hongo,
    tratamiento,
    grado_de_colonizacion,
    frecuencia_porcentaje
  ) %>%
  pivot_wider(
    names_from = grado_de_colonizacion,
    values_from = frecuencia_porcentaje,
    names_prefix = "Grado "
  ) %>%
  arrange(
    hongo,
    tratamiento
  ) %>%
  rename(
    Hongo = hongo,
    Tratamiento = tratamiento
  )

# visualizar cuadro
print(cuadro_2)

# guardar cuadro
write_xlsx(
  cuadro_2,
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/Cuadro_2_distribucion_grados_colonizacion.xlsx"
)

# ============================================================
# 23. figura 2. distribución de los grados de colonización
# ============================================================

datos_figura_2 <- datos %>%
  count(
    hongo,
    tratamiento,
    grado_de_colonizacion,
    name = "frecuencia"
  ) %>%
  group_by(hongo, tratamiento) %>%
  mutate(
    porcentaje = frecuencia / sum(frecuencia) * 100
  ) %>%
  ungroup()

figura_2 <- ggplot(
  datos_figura_2,
  aes(
    x = tratamiento,
    y = porcentaje,
    fill = factor(grado_de_colonizacion)
  )
) +
  geom_col(
    width = 0.7,
    color = "white",
    linewidth = 0.2
  ) +
  facet_wrap(
    ~ hongo,
    nrow = 1,
    strip.position = "bottom"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c(
      "0" = "#238443",
      "1" = "#A1D99B",
      "2" = "#FEE08B",
      "3" = "#F46D43",
      "4" = "#B2182B"
    ),
    breaks = as.character(0:4),
    drop = FALSE,
    name = "Grado de colonización"
  ) +
  labs(
    x = "Tratamiento",
    y = "Observaciones (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(
      color = "grey80",
      linewidth = 0.4
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      size = 9
    ),
    axis.text.y = element_text(size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(
      face = "italic",
      size = 11
    ),
    legend.position = "bottom",
    panel.spacing = unit(1.5, "lines")
  )

# visualizar figura
print(figura_2)

# guardar figura
ggsave(
  filename = "Figura_2_distribucion_grados_colonizacion.png",
  plot = figura_2,
  path = "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Figuras_generadas",
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# ============================================================
# 24. preparacion de datos para el analisis inferencial
# ============================================================

datos_modelo <- datos %>%
  mutate(
    placa_id = interaction(
      hongo,
      tratamiento,
      placa,
      drop = TRUE
    ),
    probeta_id = interaction(
      hongo,
      tratamiento,
      placa,
      probeta,
      drop = TRUE
    ),
    dia_factor = factor(
      dia_de_evaluacion,
      levels = c(13, 16, 20)
    ),
    grado_de_colonizacion = ordered(
      grado_de_colonizacion,
      levels = 0:4
    )
  )

# ============================================================
# 25. ajuste del modelo ordinal mixto
# ============================================================

modelo_ordinal <- clmm(
  grado_de_colonizacion ~ tratamiento * hongo + dia_factor +
    (1 | placa_id) + (1 | probeta_id),
  data = datos_modelo,
  link = "logit",
  Hess = TRUE
)

summary(modelo_ordinal)

# ============================================================
# 26. modelo ordinal mixto con efecto aleatorio de placa
# ============================================================

modelo_ordinal_placa <- clmm(
  grado_de_colonizacion ~ tratamiento * hongo + dia_factor +
    (1 | placa_id),
  data = datos_modelo,
  link = "logit",
  Hess = TRUE
)

summary(modelo_ordinal_placa)

# ============================================================
# 27. prueba global de la interaccion tratamiento y hongo
# ============================================================

modelo_sin_interaccion <- clmm(
  grado_de_colonizacion ~ tratamiento + hongo + dia_factor +
    (1 | placa_id),
  data = datos_modelo,
  link = "logit",
  Hess = TRUE
)

# ============================================================
# 28. preparacion de comparaciones multiples
# ============================================================

if (!requireNamespace("emmeans", quietly = TRUE)) {
  install.packages("emmeans")
}

library(emmeans)

# ============================================================
# 29. comparaciones multiples entre tratamientos por hongo
# ============================================================

comparaciones_tratamientos <- emmeans(
  modelo_ordinal_placa,
  pairwise ~ tratamiento | hongo,
  adjust = "tukey"
)

resultados_comparaciones <- as.data.frame(
  comparaciones_tratamientos$contrasts
)

print(resultados_comparaciones)
anova(
  modelo_sin_interaccion,
  modelo_ordinal_placa
)

# ============================================================
# 30. intervalos de confianza de las comparaciones
# ============================================================

intervalos_comparaciones <- as.data.frame(
  confint(
    comparaciones_tratamientos$contrasts,
    level = 0.95,
    adjust = "tukey"
  )
)

print(intervalos_comparaciones)

# ============================================================
# 31. evaluacion de la estabilidad del modelo ordinal
# ============================================================

# informacion del proceso de estimacion
modelo_ordinal_placa$optRes

# numero de condicion de la matriz hessiana
modelo_ordinal_placa$cond.H

# varianza de los efectos aleatorios
VarCorr(modelo_ordinal_placa)

# efectos aleatorios estimados para las placas
efectos_placa <- ranef(modelo_ordinal_placa)

print(efectos_placa)

resumen_modelo <- summary(modelo_ordinal_placa)

resumen_modelo$cond.H

resumen_modelo$optRes$max.grad

resumen_modelo$optRes$convergence

# ============================================================
# 33. intervalos de confianza de los coeficientes del modelo
# ============================================================

intervalos_modelo <- as.data.frame(
  confint(modelo_ordinal_placa)
)

print(intervalos_modelo)

# ============================================================
# 34. evaluacion del supuesto de odds proporcionales
# ============================================================

modelo_diagnostico <- clm(
  grado_de_colonizacion ~ tratamiento * hongo + dia_factor,
  data = datos_modelo,
  link = "logit"
)

prueba_odds <- nominal_test(modelo_diagnostico)

print(prueba_odds)

# ============================================================
# 35. revision de la prueba de odds proporcionales
# ============================================================

str(prueba_odds)

print(prueba_odds, digits = 6)

# ============================================================
# 36. comparacion de coeficientes entre modelos
# ============================================================

coef_mixto <- as.data.frame(
  coef(summary(modelo_ordinal_placa))
) %>%
  tibble::rownames_to_column("parametro") %>%
  filter(!grepl("\\|", parametro)) %>%
  select(
    parametro,
    estimacion_mixta = Estimate,
    error_mixto = `Std. Error`,
    p_mixto = `Pr(>|z|)`
  )

coef_simple <- as.data.frame(
  coef(summary(modelo_diagnostico))
) %>%
  tibble::rownames_to_column("parametro") %>%
  filter(!grepl("\\|", parametro)) %>%
  select(
    parametro,
    estimacion_simple = Estimate,
    error_simple = `Std. Error`,
    p_simple = `Pr(>|z|)`
  )

comparacion_modelos <- full_join(
  coef_mixto,
  coef_simple,
  by = "parametro"
)

print(comparacion_modelos)

# ============================================================
# 37. identificacion de placas con mayor efecto aleatorio
# ============================================================

efectos_placa_resumen <- as.data.frame(
  ranef(modelo_ordinal_placa)$placa_id
) %>%
  tibble::rownames_to_column("placa_id") %>%
  mutate(
    magnitud = abs(`(Intercept)`)
  ) %>%
  arrange(desc(magnitud))

print(efectos_placa_resumen)

# ============================================================
# 38. analisis de sensibilidad al excluir placas extremas
# ============================================================

placas_revision <- efectos_placa_resumen %>%
  slice_max(magnitud, n = 3) %>%
  pull(placa_id)

datos_sensibilidad <- datos_modelo %>%
  filter(!placa_id %in% placas_revision)

modelo_sensibilidad <- clmm(
  grado_de_colonizacion ~ tratamiento * hongo + dia_factor +
    (1 | placa_id),
  data = datos_sensibilidad,
  link = "logit",
  Hess = TRUE
)

summary(modelo_sensibilidad)

# ============================================================
# 39. probabilidades estimadas de los grados de colonizacion
# ============================================================

probabilidades_grados <- emmeans(
  modelo_ordinal_placa,
  ~ grado_de_colonizacion | tratamiento * hongo,
  mode = "prob"
)

cuadro_probabilidades <- as.data.frame(
  probabilidades_grados
)

print(cuadro_probabilidades)

# ============================================================
# 40. verificacion de probabilidades estimadas
# ============================================================

# niveles originales de la variable
levels(datos_modelo$grado_de_colonizacion)

# suma de probabilidades por tratamiento y hongo
verificacion_probabilidades <- cuadro_probabilidades %>%
  group_by(hongo, tratamiento) %>%
  summarise(
    suma_probabilidades = sum(prob),
    .groups = "drop"
  )

print(verificacion_probabilidades)

# ============================================================
# 41. cuadro 3. probabilidades estimadas de colonizacion
# ============================================================

cuadro_3 <- cuadro_probabilidades %>%
  mutate(
    grado = as.character(
      as.integer(grado_de_colonizacion) - 1
    ),
    porcentaje = round(prob * 100, 1)
  ) %>%
  select(
    hongo,
    tratamiento,
    grado,
    porcentaje
  ) %>%
  pivot_wider(
    names_from = grado,
    values_from = porcentaje,
    names_prefix = "Grado "
  ) %>%
  arrange(
    hongo,
    tratamiento
  ) %>%
  rename(
    Hongo = hongo,
    Tratamiento = tratamiento
  )

print(cuadro_3)

write_xlsx(
  cuadro_3,
  "C:/Users/valeo/Downloads/Resultados/Analisis_de_datos/Tablas_generadas/Cuadro_3_probabilidades_estimadas_colonizacion.xlsx"
)

# ============================================================
# 43. diagnostico del supuesto de odds proporcionales
# ============================================================

# ajustar un modelo logistico mixto para cada punto de corte

modelos_corte <- lapply(0:3, function(corte) {
  
  datos_corte <- datos_modelo %>%
    mutate(
      respuesta = as.integer(
        as.character(grado_de_colonizacion)
      ) > corte
    )
  
  glmer(
    respuesta ~ tratamiento * hongo + dia_factor +
      (1 | placa_id),
    data = datos_corte,
    family = binomial(link = "logit"),
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 200000)
    )
  )
})

# extraer los coeficientes de cada modelo

resultados_cortes <- bind_rows(
  lapply(0:3, function(corte) {
    
    coeficientes <- as.data.frame(
      coef(summary(modelos_corte[[corte + 1]]))
    )
    
    coeficientes$termino <- rownames(coeficientes)
    coeficientes$punto_corte <- corte
    
    coeficientes
  })
) %>%
  select(
    punto_corte,
    termino,
    Estimate,
    `Std. Error`,
    `z value`,
    `Pr(>|z|)`
  )

print(resultados_cortes, n = Inf)

# verificar que se crearon los cuatro modelos
length(modelos_corte)

# revisar las advertencias y la convergencia
lapply(modelos_corte, function(modelo) {
  list(
    convergencia = modelo@optinfo$conv,
    singular = isSingular(modelo)
  )
})


distribucion_cortes <- datos_modelo %>%
  mutate(
    grado = as.integer(
      as.character(grado_de_colonizacion)
    )
  ) %>%
  group_by(tratamiento, hongo) %>%
  summarise(
    total = n(),
    superior_0 = sum(grado > 0),
    superior_1 = sum(grado > 1),
    superior_2 = sum(grado > 2),
    superior_3 = sum(grado > 3),
    .groups = "drop"
  )

print(distribucion_cortes, n = Inf)

# ============================================================
# 44. revision de la estructura del modelo ordinal principal
# ============================================================

summary(modelo_ordinal_placa)

VarCorr(modelo_ordinal_placa)

nobs(modelo_ordinal_placa)

# ============================================================
# 45. revision de magnitud de los coeficientes del modelo ordinal
# ============================================================

coeficientes_modelo <- coef(summary(modelo_ordinal_placa))

tabla_coeficientes <- data.frame(
  Parametro = rownames(coeficientes_modelo),
  Estimacion = coeficientes_modelo[, "Estimate"],
  Error_estandar = coeficientes_modelo[, "Std. Error"],
  Estadistico_z = coeficientes_modelo[, "z value"],
  Valor_p = coeficientes_modelo[, "Pr(>|z|)"]
)

print(tabla_coeficientes, row.names = FALSE)

# ============================================================
# 46. intervalos de confianza de los coeficientes del modelo ordinal
# ============================================================

intervalos_modelo <- as.data.frame(
  confint(modelo_ordinal_placa)
)

intervalos_modelo$Parametro <- rownames(intervalos_modelo)

tabla_intervalos <- tabla_coeficientes %>%
  filter(!Parametro %in% c("0|1", "1|2", "2|3", "3|4")) %>%
  left_join(
    intervalos_modelo,
    by = "Parametro"
  ) %>%
  select(
    Parametro,
    Estimacion,
    Error_estandar,
    Valor_p,
    `2.5 %`,
    `97.5 %`
  )

print(tabla_intervalos, row.names = FALSE)

# ============================================================
# 47. verificacion de la estructura de efectos aleatorios
# ============================================================

# comparar el modelo con efecto aleatorio de placa y probeta
# frente al modelo con efecto aleatorio de placa solamente

AIC(modelo_ordinal, modelo_ordinal_placa)

logLik(modelo_ordinal)
logLik(modelo_ordinal_placa)

# revisar las varianzas de los efectos aleatorios
VarCorr(modelo_ordinal)
VarCorr(modelo_ordinal_placa)

# confirmar el numero de observaciones
nobs(modelo_ordinal)
nobs(modelo_ordinal_placa)

# ============================================================
# 48. comparacion de modelos ordinales proporcionales y no proporcionales
# ============================================================

# modelo con odds proporcionales
modelo_clmm2_po <- clmm2(
  location = grado_de_colonizacion ~
    tratamiento * hongo + dia_factor,
  random = placa_id,
  data = datos_modelo,
  link = "logistic",
  Hess = TRUE
)

# modelo con efectos nominales para tratamiento y hongo
modelo_clmm2_npo <- clmm2(
  location = grado_de_colonizacion ~
    tratamiento * hongo + dia_factor,
  nominal = ~ tratamiento * hongo,
  random = placa_id,
  data = datos_modelo,
  link = "logistic",
  Hess = TRUE
)

# comparar ajuste y numero de parametros
logLik(modelo_clmm2_po)
logLik(modelo_clmm2_npo)

AIC(modelo_clmm2_po)
AIC(modelo_clmm2_npo)

length(coef(modelo_clmm2_po))
length(coef(modelo_clmm2_npo))

# revisar convergencia
summary(modelo_clmm2_po)
summary(modelo_clmm2_npo)

# ============================================================
# 48.1. revision de los modelos ordinales ajustados
# ============================================================

# revisar el modelo con odds proporcionales
summary(modelo_clmm2_po)

# revisar el modelo con efectos nominales
summary(modelo_clmm2_npo)

# comprobar el numero de observaciones
nobs(modelo_clmm2_po)
nobs(modelo_clmm2_npo)

# revisar las desviaciones estandar de los efectos aleatorios
modelo_clmm2_po$stDev
modelo_clmm2_npo$stDev

# revisar las log-verosimilitudes
logLik(modelo_clmm2_po)
logLik(modelo_clmm2_npo)

# ============================================================
# 49. comparacion de frecuencias observadas y probabilidades estimadas por dia
# ============================================================

# obtener probabilidades estimadas por el modelo para cada dia

probabilidades_por_dia <- emmeans(
  modelo_ordinal_placa,
  ~ grado_de_colonizacion | tratamiento * hongo * dia_factor,
  mode = "prob"
) %>%
  as.data.frame() %>%
  mutate(
    grado = as.integer(
      as.character(grado_de_colonizacion)
    ) - 1,
    porcentaje_estimado = prob * 100
  ) %>%
  select(
    hongo,
    tratamiento,
    dia_factor,
    grado,
    porcentaje_estimado
  )

# calcular frecuencias y porcentajes observados

frecuencias_observadas <- datos_modelo %>%
  mutate(
    grado = as.integer(
      as.character(grado_de_colonizacion)
    )
  ) %>%
  group_by(
    hongo,
    tratamiento,
    dia_factor,
    grado
  ) %>%
  summarise(
    frecuencia_observada = n(),
    porcentaje_observado = n() / 10 * 100,
    .groups = "drop"
  )

# comparar resultados observados y estimados

comparacion_por_dia <- frecuencias_observadas %>%
  full_join(
    probabilidades_por_dia,
    by = c("hongo", "tratamiento", "dia_factor", "grado")
  ) %>%
  mutate(
    frecuencia_observada = coalesce(frecuencia_observada, 0L),
    porcentaje_observado = coalesce(porcentaje_observado, 0),
    porcentaje_estimado = coalesce(porcentaje_estimado, 0),
    diferencia_pp = porcentaje_observado - porcentaje_estimado,
    diferencia_absoluta_pp = abs(diferencia_pp)
  ) %>%
  arrange(
    hongo,
    tratamiento,
    dia_factor,
    grado
  )

print(comparacion_por_dia, n = Inf)

# ============================================================
# 49.1. verificacion de las probabilidades estimadas por dia
# ============================================================

verificacion_probabilidades <- probabilidades_por_dia %>%
  group_by(
    hongo,
    tratamiento,
    dia_factor
  ) %>%
  summarise(
    suma_probabilidades = sum(porcentaje_estimado),
    cantidad_grados = n_distinct(grado),
    .groups = "drop"
  )

# mostrar resultados sin utilizar print()
cat(
  "Combinaciones evaluadas:",
  nrow(verificacion_probabilidades),
  "\n"
)

cat(
  "Suma minima:",
  min(verificacion_probabilidades$suma_probabilidades),
  "\n"
)

cat(
  "Suma maxima:",
  max(verificacion_probabilidades$suma_probabilidades),
  "\n"
)

cat(
  "Cantidad minima de grados:",
  min(verificacion_probabilidades$cantidad_grados),
  "\n"
)

cat(
  "Cantidad maxima de grados:",
  max(verificacion_probabilidades$cantidad_grados),
  "\n"
)

# ============================================================
# 49.2. resumen de diferencias entre porcentajes observados y estimados
# ============================================================

resumen_ajuste <- comparacion_por_dia %>%
  group_by(
    hongo,
    tratamiento,
    dia_factor
  ) %>%
  summarise(
    error_absoluto_medio = mean(diferencia_absoluta_pp),
    error_maximo = max(diferencia_absoluta_pp),
    .groups = "drop"
  )

# resumen general
cat(
  "Error absoluto medio general:",
  mean(comparacion_por_dia$diferencia_absoluta_pp),
  "puntos porcentuales\n"
)

cat(
  "Error absoluto maximo:",
  max(comparacion_por_dia$diferencia_absoluta_pp),
  "puntos porcentuales\n"
)

# ============================================================
# 49.3. identificacion de las mayores discrepancias
# ============================================================

mayores_discrepancias <- comparacion_por_dia %>%
  arrange(desc(diferencia_absoluta_pp)) %>%
  select(
    hongo,
    tratamiento,
    dia_factor,
    grado,
    porcentaje_observado,
    porcentaje_estimado,
    diferencia_pp,
    diferencia_absoluta_pp
  )

# mostrar las 15 discrepancias mas grandes

print(head(mayores_discrepancias, 15), row.names = FALSE)

# ============================================================
# 49.4. revision detallada de las mayores discrepancias
# ============================================================

options(width = 250)

tabla_discrepancias <- mayores_discrepancias %>%
  slice_head(n = 15) %>%
  mutate(
    porcentaje_observado = round(porcentaje_observado, 2),
    porcentaje_estimado = round(porcentaje_estimado, 2),
    diferencia_pp = round(diferencia_pp, 2),
    diferencia_absoluta_pp = round(diferencia_absoluta_pp, 2)
  )

print(as.data.frame(tabla_discrepancias), row.names = FALSE)

# ============================================================
# 49.5. revision de los valores ajustados del modelo ordinal
# ============================================================

# revisar la estructura de los valores ajustados
str(modelo_ordinal_placa$fitted.values)

# comprobar su longitud
length(modelo_ordinal_placa$fitted.values)

# comprobar si contienen una matriz de probabilidades
dim(modelo_ordinal_placa$fitted.values)

# revisar los primeros valores
head(modelo_ordinal_placa$fitted.values)

# ============================================================
# 49.6. evaluacion de las probabilidades asignadas a las observaciones
# ============================================================

# incorporar las probabilidades ajustadas a cada observacion

datos_ajuste <- datos_modelo %>%
  mutate(
    probabilidad_observada = modelo_ordinal_placa$fitted.values
  )

# resumen general

cat(
  "Probabilidad media asignada a la categoria observada:",
  mean(datos_ajuste$probabilidad_observada),
  "\n"
)

cat(
  "Probabilidad minima:",
  min(datos_ajuste$probabilidad_observada),
  "\n"
)

cat(
  "Probabilidad maxima:",
  max(datos_ajuste$probabilidad_observada),
  "\n"
)

# resumen por hongo y tratamiento

resumen_probabilidades_ajustadas <- datos_ajuste %>%
  group_by(hongo, tratamiento) %>%
  summarise(
    probabilidad_media = mean(probabilidad_observada),
    probabilidad_minima = min(probabilidad_observada),
    probabilidad_maxima = max(probabilidad_observada),
    .groups = "drop"
  )

print(as.data.frame(resumen_probabilidades_ajustadas), row.names = FALSE)

