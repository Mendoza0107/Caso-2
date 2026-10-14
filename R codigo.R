# Instalar librerías (solo la primera vez)
install.packages(c("readxl", "dplyr", "tidyr", "stringr"))

# Cargar librerías
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Importar base de datos
icfes <- read_excel(file.choose())

# Visualizar los primeros registros
head(icfes)

# Conocer las dimensiones de la base
dim(icfes)

# Revisar la estructura
str(icfes)


# Seleccionar variables de interés
icfes <- icfes %>%
  select(
    ESTU_GENERO,
    ESTU_MCPIO_RESIDE,
    FAMI_ESTRATOVIVIENDA,
    FAMI_TIENEINTERNET,
    FAMI_TIENECOMPUTADOR,
    COLE_AREA_UBICACION,
    PERCENTIL_GLOBAL
  )

# Visualizar la base
head(icfes)

# Cantidad de valores faltantes por variable
colSums(is.na(icfes))

# Porcentaje de valores faltantes
round(colMeans(is.na(icfes)) * 100, 2)


# Depuración de la base de datos
icfes_limpia <- icfes %>%
  drop_na() %>%
  filter(
    ESTU_GENERO %in% c("F", "M"),
    FAMI_TIENEINTERNET %in% c("Si", "No"),
    FAMI_TIENECOMPUTADOR %in% c("Si", "No"),
    COLE_AREA_UBICACION %in% c("URBANO", "RURAL"),
    PERCENTIL_GLOBAL >= 1,
    PERCENTIL_GLOBAL <= 100
  )

# Comparar dimensiones
dim(icfes)
dim(icfes_limpia)


# Crear variable dependiente binaria
icfes_limpia <- icfes_limpia %>%
  mutate(
    SUPERA_P25 = ifelse(PERCENTIL_GLOBAL > 25, 1, 0)
  )

# Revisar distribución
table(icfes_limpia$SUPERA_P25)

# Calcular porcentajes
prop.table(table(icfes_limpia$SUPERA_P25)) * 100


# Eliminar variable original
icfes_limpia <- icfes_limpia %>%
  select(-PERCENTIL_GLOBAL)

# Revisar variables
names(icfes_limpia)


# Convertir variables categóricas
icfes_limpia <- icfes_limpia %>%
  mutate(
    ESTU_GENERO = factor(ESTU_GENERO),
    
    ESTU_MCPIO_RESIDE = factor(ESTU_MCPIO_RESIDE),
    
    FAMI_ESTRATOVIVIENDA = factor(
      FAMI_ESTRATOVIVIENDA,
      levels = c(
        "Sin Estrato",
        "Estrato 1",
        "Estrato 2",
        "Estrato 3",
        "Estrato 4",
        "Estrato 5",
        "Estrato 6"
      )
    ),
    
    FAMI_TIENEINTERNET = factor(
      FAMI_TIENEINTERNET,
      levels = c("No", "Si")
    ),
    
    FAMI_TIENECOMPUTADOR = factor(
      FAMI_TIENECOMPUTADOR,
      levels = c("No", "Si")
    ),
    
    COLE_AREA_UBICACION = factor(
      COLE_AREA_UBICACION,
      levels = c("RURAL", "URBANO")
    ),
    
    SUPERA_P25 = factor(
      SUPERA_P25,
      levels = c(0, 1)
    )
  )

# Revisar estructura final
str(icfes_limpia)



# Verificar valores faltantes
colSums(is.na(icfes_limpia))

# Verificar dimensiones
dim(icfes_limpia)

# Verificar registros duplicados
sum(duplicated(icfes_limpia))

# Verificar distribución de la variable dependiente
table(icfes_limpia$SUPERA_P25)

# Revisar estructura
str(icfes_limpia)

# Visualizar primeros registros
head(icfes_limpia)


# Guardar base depurada en formato CSV
write.csv(
  icfes_limpia,
  "ICFES_DEPURADA.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Guardar base en formato RDS
saveRDS(
  icfes_limpia,
  "ICFES_DEPURADA.rds"
)


icfes_limpia <- readRDS("ICFES_DEPURADA.rds")


# Número total de estudiantes
nrow(icfes_limpia)


library(dplyr)
library(tidyr)


# Variables que vamos a analizar
variables <- c(
  "SUPERA_P25",
  "ESTU_GENERO",
  "ESTU_MCPIO_RESIDE",
  "FAMI_ESTRATOVIVIENDA",
  "FAMI_TIENEINTERNET",
  "FAMI_TIENECOMPUTADOR",
  "COLE_AREA_UBICACION"
)

# Crear tabla de frecuencias y porcentajes
tabla_descriptiva <- icfes_limpia %>%
  select(all_of(variables)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Categoria"
  ) %>%
  group_by(Variable, Categoria) %>%
  summarise(
    Frecuencia = n(),
    .groups = "drop"
  ) %>%
  group_by(Variable) %>%
  mutate(
    Porcentaje = round(
      Frecuencia / sum(Frecuencia) * 100, 2
    )
  ) %>%
  ungroup()

# Visualizar tabla
View(tabla_descriptiva)




# Mejorar nombres de variables
tabla_descriptiva <- tabla_descriptiva %>%
  mutate(
    Variable = recode(
      Variable,
      "SUPERA_P25" = "Supera percentil 25",
      "ESTU_GENERO" = "Género",
      "ESTU_MCPIO_RESIDE" = "Municipio de residencia",
      "FAMI_ESTRATOVIVIENDA" = "Estrato socioeconómico",
      "FAMI_TIENEINTERNET" = "Acceso a internet",
      "FAMI_TIENECOMPUTADOR" = "Disponibilidad de computador",
      "COLE_AREA_UBICACION" = "Ubicación del colegio"
    )
  )

# Visualizar resultados
View(tabla_descriptiva)

saveRDS(tabla_descriptiva, "tabla_descriptiva.rds")



Sys.setlocale("LC_CTYPE", "Spanish_Colombia.utf8")


library(dplyr)
library(plotly)

# Preparar datos
datos_percentil <- icfes_limpia %>%
  count(SUPERA_P25) %>%
  mutate(
    Categoria = ifelse(
      as.character(SUPERA_P25) == "1",
      "Supera P25",
      "No supera P25"
    ),
    Porcentaje = round(n / sum(n) * 100, 1)
  )

# Crear gráfico interactivo
figura_1 <- plot_ly(
  data = datos_percentil,
  x = ~Categoria,
  y = ~Porcentaje,
  type = "bar",
  marker = list(color = "gray"),
  text = ~paste0(Porcentaje, "%"),
  textposition = "outside",
  customdata = ~n,
  hovertemplate = paste0(
    "<b>%{x}</b>",
    "<br>Porcentaje: %{y}%",
    "<br>Estudiantes: %{customdata}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "Figura 1. Distribucion de estudiantes segun el percentil 25",
      x = 0.5
    ),
    xaxis = list(title = "Clasificacion del estudiante"),
    yaxis = list(
      title = "Porcentaje de estudiantes (%)",
      range = c(0, 100)
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    showlegend = FALSE
  )

figura_1


saveRDS(figura_1, "figura_1.rds")






library(dplyr)
library(plotly)

# Preparar datos
datos_genero <- icfes_limpia %>%
  count(ESTU_GENERO) %>%
  mutate(
    Categoria = case_when(
      ESTU_GENERO == "F" ~ "Femenino",
      ESTU_GENERO == "M" ~ "Masculino",
      TRUE ~ as.character(ESTU_GENERO)
    ),
    Porcentaje = round(n / sum(n) * 100, 1)
  )

# Crear gráfico de dona interactivo
figura_2 <- plot_ly(
  data = datos_genero,
  labels = ~Categoria,
  values = ~n,
  type = "pie",
  hole = 0.55,
  sort = FALSE,
  marker = list(
    colors = c("gray65", "gray35"),
    line = list(color = "white", width = 2)
  ),
  textinfo = "label+percent",
  textposition = "outside",
  hovertemplate = paste0(
    "<b>%{label}</b>",
    "<br>Estudiantes: %{value:,}",
    "<br>Porcentaje: %{percent}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "Figura 2. Distribucion de estudiantes segun genero",
      x = 0.5
    ),
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1
    ),
    margin = list(
      l = 80,
      r = 80,
      b = 80,
      t = 90
    ),
    paper_bgcolor = "white"
  )

# Visualizar gráfico
figura_2


saveRDS(figura_2, "figura_2.rds")



names(icfes_limpia)


library(dplyr)
library(plotly)

# Preparar datos
datos_estrato <- icfes_limpia %>%
  count(FAMI_ESTRATOVIVIENDA) %>%
  mutate(
    Categoria = as.character(FAMI_ESTRATOVIVIENDA),
    Porcentaje = round(n / sum(n) * 100, 1)
  )

# Ordenar estratos
orden_estratos <- c(
  "Estrato 1",
  "Estrato 2",
  "Estrato 3",
  "Estrato 4",
  "Estrato 5",
  "Estrato 6",
  "Sin Estrato"
)

datos_estrato <- datos_estrato %>%
  mutate(
    Categoria = factor(
      Categoria,
      levels = orden_estratos
    )
  ) %>%
  arrange(Categoria)

# Crear gráfico interactivo
figura_3 <- plot_ly(
  data = datos_estrato,
  x = ~Porcentaje,
  y = ~Categoria,
  type = "bar",
  orientation = "h",
  marker = list(color = "gray55"),
  text = ~paste0(Porcentaje, "%"),
  textposition = "outside",
  customdata = ~n,
  hovertemplate = paste0(
    "<b>%{y}</b>",
    "<br>Porcentaje: %{x}%",
    "<br>Estudiantes: %{customdata}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "Figura 3. Distribucion de estudiantes segun estrato socioeconomico",
      x = 0.5
    ),
    xaxis = list(
      title = "Porcentaje de estudiantes (%)",
      rangemode = "tozero"
    ),
    yaxis = list(
      title = "Estrato socioeconomico",
      autorange = "reversed"
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    showlegend = FALSE,
    margin = list(
      l = 110,
      r = 80,
      b = 70,
      t = 90
    )
  )

# Visualizar
figura_3


saveRDS(figura_3, "figura_3.rds")


icfes_limpia <- readRDS("ICFES_DEPURADA.rds")

names(icfes_limpia)






# FIGURA 5. DISPONIBILIDAD DE COMPUTADOR

library(dplyr)
library(plotly)

# Cargar base de datos
icfes_limpia <- readRDS("ICFES_DEPURADA.rds")

# Preparar datos
datos_computador <- icfes_limpia %>%
  mutate(
    Computador = case_when(
      toupper(trimws(as.character(FAMI_TIENECOMPUTADOR))) %in%
        c("SI", "SÍ") ~ "Sí tiene computador",
      toupper(trimws(as.character(FAMI_TIENECOMPUTADOR))) == "NO" ~
        "No tiene computador",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Computador)) %>%
  count(Computador, name = "Frecuencia") %>%
  mutate(
    Porcentaje = round(Frecuencia / sum(Frecuencia) * 100, 1)
  )

# Crear gráfico interactivo
figura_5 <- plot_ly(
  data = datos_computador,
  x = ~Porcentaje,
  y = ~"Estudiantes",
  color = ~Computador,
  colors = c("gray35", "gray70"),
  type = "bar",
  orientation = "h",
  text = ~paste0(Porcentaje, "%"),
  textposition = "inside",
  insidetextanchor = "middle",
  customdata = ~Frecuencia,
  hovertemplate = paste0(
    "<b>%{fullData.name}</b>",
    "<br>Porcentaje: %{x:.1f}%",
    "<br>Estudiantes: %{customdata}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "Figura 5. Distribución de estudiantes según disponibilidad de computador",
      x = 0.5
    ),
    barmode = "stack",
    xaxis = list(
      title = "Porcentaje de estudiantes (%)",
      range = c(0, 100)
    ),
    yaxis = list(title = ""),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.3
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(
      l = 80,
      r = 60,
      b = 110,
      t = 100
    )
  )

# Visualizar
figura_5
