################################################################################
#                         TRABAJO PRÁCTICO FINAL

# Estadística Bayesiana 
# Junio 2026

# Docentes: Evangelista Ignacio, Capretto Tomás, Ovando Francisco
# Integrantes: Abatedaga Lucía, Denipotti Pía, Mogetta María Eugenia


################################################################################

# Librerías
library(ggplot2)
library(corrplot)
library(tidyverse)

#### Carga de datos ####

datos <- read.csv("../datos/notas_estadistica.csv")
#View(datos)
notas <- datos

# data set solo con las ausencias igual cero y nota final igual cero
library(tidyverse)
datos_ausencias_cero <- datos %>% filter(datos$nota3 == 0)
#View(datos_ausencias_cero)

summary(datos_ausencias_cero)

# 38 alumnos son eliminados del conjunto de datos ya que representan inconsistencias (aquellos con nota final = 0 y 0 ausencias )

datos <- datos %>% 
  filter(!(ausencias == 0 & nota3 == 0))


# Variables categóricas:

# escuela, sexo, domicilio, tamano_familia, convivencia_padres, educacion_madre, educacion_padre, 
# trabajo_madre, trabajo_padre, motivo_escuela, tutor y las siguientes:

levels(datos$domicilio) <- c("Rural", "Urbano")
datos$apoyo_escolar <- as.factor(datos$apoyo_escolar)
datos$apoyo_familiar <- as.factor(datos$apoyo_familiar)
datos$clases_pagadas <- as.factor(datos$clases_pagadas)
datos$actividades <- as.factor(datos$actividades)
datos$jardin_infantes <- as.factor(datos$jardin_infantes)
datos$quiere_superior <- as.factor(datos$quiere_superior)
datos$internet <- as.factor(datos$internet)
datos$pareja <- as.factor(datos$pareja)
datos$relacion_familiar <- as.factor(datos$relacion_familiar)
datos$tiempo_libre <- as.factor(datos$tiempo_libre)
datos$salidas_amigos <- as.factor(datos$salidas_amigos)
datos$alcohol_semana <- as.factor(datos$alcohol_semana)
datos$alcohol_finde <- as.factor(datos$alcohol_finde)
datos$salud <- as.factor(datos$salud)

#### Análisis exploratorio ####

# Análisis univariado

summary(datos)
str(datos)

# Hay 357 alumnos bajo análisis, de los cuales 315 pertenecen a la escuela GP (88,2 %) y solo 42 a la escuela MS (11,7 %).

nrow(datos) 
summary(datos$escuela) 
prop.table(table(datos$escuela))

# Nota final:

summary(datos$nota3)
sd(datos$nota3, na.rm = TRUE)

hist(datos$nota3, 
     xlab = "Nota Final", 
     ylab = "Frecuencia", 
     main = " ", 
     xaxt = "n",
     breaks = seq(0, 10, by = 1) 
)
axis(side = 1, at = 0:10)

ggplot(datos, aes(x = nota3)) +
  geom_histogram(binwidth = 1, fill = "#847949", color = "white") +
  theme_minimal() +
  # Línea divisoria entre aprobados y no aprobados
  geom_vline(xintercept = 5, color = "black", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Nota Final",
       y = "Frecuencia")

ggplot(datos, aes(x = nota3)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "cadetblue", color = "black", alpha = 0.7) +
  # La curva de densidad suaviza la distribución para ver la forma general
  geom_density(color = "darkred", size = 1.2) +
  # Línea vertical marcando la media matemática
  geom_vline(aes(xintercept = mean(nota3, na.rm = TRUE)), color = "black", linetype = "dashed", size = 1) +
  labs(
    x = "Nota Final",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))



ggplot(datos, aes(x = nota3)) +
  # binwidth = 0.5 define una barra por cada salto de nota. 
  # boundary = 0.25 centra la barra exactamente en el valor de la nota.
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, boundary = 0.25, fill = "#E62222", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(nota3, na.rm = TRUE)), color = "black", linetype = "dashed", linewidth = 1) +
  # Define las etiquetas del 1 al 10 y expande ligeramente los límites para que las barras de los extremos no se corten
  scale_x_continuous(breaks = 1:10, limits = c(0.5, 10.5)) +
  labs(
    x = "Nota Final",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Figura 1. Distribución de la nota final de los alumnos de estadística. 
summary(as.factor(datos$nota3))

# Análisis bivariado

#datos_base <- datos %>%
#  select(-nota1, -nota2)

# Relación entre las variables cuantitativas y la nota final:
# 1) Matriz de correlaciones 

datos_numericos <- datos[, sapply(datos, is.numeric)]
datos_numericos <- subset(datos_numericos, select = -c(nota1, nota2))
cor_con_nota3 <- cor(datos_numericos, datos_numericos$nota3, use = "pairwise.complete.obs")
print(round(cor_con_nota3, 3))

# Seleccionamos las variables numéricas para una matriz de correlación
vars_numericas <- datos %>%
  select(edad, tiempo_viaje, tiempo_estudio, reprobaciones_previas, ausencias, nota3)

# Matriz de correlación
matriz_corr <- cor(vars_numericas, use = "complete.obs")
matriz_corr

# 2) Gráficos de dispersión

ggplot(notas, aes(x = tiempo_viaje, y = nota3)) +
  geom_point(alpha = 0.6, size = 3) + 
  labs(caption = "Relación entre Nota 3 y Tiempo de viaje",
       x = "Tiempo de viaje",
       y = "Nota 3") +
  theme_minimal()

ggplot(datos, aes(x = edad, y = nota3)) +
  geom_point(alpha = 0.6, size = 3) +
  labs(caption = "Relación entre Nota 3 y Edad",
       x = "Edad",
       y = "Nota 3") +
  theme_minimal()


ggplot(datos, aes(x = tiempo_estudio, y = nota3)) +
  geom_point(alpha = 0.6, size = 3) +
  labs(caption = "Relación entre Nota 3 y Tiempo de estudio",
       x = "Tiempo de estudio",
       y = "Nota 3") +
  theme_minimal()

ggplot(datos, aes(x = reprobaciones_previas, y = nota3)) +
  geom_point(alpha = 0.6, size = 3) +
  labs(caption = "Relación entre Nota 3 y Reprobaciones previas",
       x = "Reprobaciones previas",
       y = "Nota 3") +
  theme_minimal()

ggplot(datos, aes(x = ausencias, y = nota3)) +
  geom_point(alpha = 0.6, size = 3) +
  labs(caption = "Relación entre Nota 3 y Ausencias",
       x = "Ausencias",
       y = "Nota 3") +
  theme_minimal()

# Relación entre las variables cualitativas y la nota final: Boxplots

summary(datos$sexo)
datos <- datos %>% mutate(educacion_madre = factor(educacion_madre, levels = c("sin educación", "educación primaria", "educación secundaria incompleta", "educación secundaria", "educación superior")))
datos <- datos %>% mutate(educacion_padre = factor(educacion_padre, levels = c("sin educación", "educación primaria", "educación secundaria incompleta", "educación secundaria", "educación superior")))

ggplot(datos, aes(x = as.factor(educacion_madre), y = nota3, fill = as.factor(educacion_madre))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Educación de la madre",
       y = "Nota 3") +
  # Especificamos el orden del eje X y del color de relleno
  scale_x_discrete(limits = c("sin educación", "educación primaria", "educación secundaria incompleta", "educación secundaria", "educación superior")) +
  scale_fill_discrete(limits = c("sin educación", "educación primaria", "educación secundaria incompleta", "educación secundaria", "educación superior")) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(educacion_padre), y = nota3, fill = as.factor(educacion_padre))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Educación del padre",
       y = "Nota 3") +
  # Especificamos el orden del eje X y del color de relleno
  scale_x_discrete(limits = c("sin educación", "educación primaria", "educación secundaria incompleta", "educación secundaria", "educación superior")) +
  scale_fill_discrete(limits = c("sin educación", "educación primaria", "educación secundaria incompleta", "educación secundaria", "educación superior")) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(escuela), y = nota3, fill = as.factor(escuela))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Escuela a la que asiste",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(sexo), y = nota3, fill = as.factor(sexo))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#847949", "#491211")) + 
  labs(x = "Sexo",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(domicilio), y = nota3, fill = as.factor(domicilio))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Domicilio",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(tamano_familia), y = nota3, fill = as.factor(tamano_familia))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Tamaño de la familia",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(convivencia_padres), y = nota3, fill = as.factor(convivencia_padres))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Situación de convivencia de los padres",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(trabajo_madre), y = nota3, fill = as.factor(trabajo_madre))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Trabajo de la madre",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(trabajo_padre), y = nota3, fill = as.factor(trabajo_padre))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Trabajo del padre",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(apoyo_escolar), y = nota3, fill = as.factor(apoyo_escolar))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#847949", "#491211")) + 
  scale_x_discrete(labels = c("0" = "No", "1" = "Sí")) +
  labs(
    x = "El alumno recibe apoyo educativo extra de la escuela",
    y = "Nota Final"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

datos_logistica <- datos %>% 
  mutate(aprobado = ifelse(nota3 >= 5, 1, 0))

prop.table(table(datos_logistica$apoyo_escolar, datos_logistica$aprobado), margin = 1)

# 1. Creas la tabla de frecuencias absolutas base
tabla_frecuencias <- table(datos_logistica$apoyo_escolar, datos_logistica$aprobado)

# 2. Asignas los nombres a las dimensiones (filas y columnas)
dimnames(tabla_frecuencias) <- list(
  "Apoyo Escolar" = c("No recibe", "Sí recibe"),
  "Condición" = c("No aprobado", "Aprobado")
)

# 3. Calculas la proporción marginal por fila
prop.table(tabla_frecuencias, margin = 1)

ggplot(datos, aes(x = as.factor(apoyo_familiar), y = nota3, fill = as.factor(apoyo_familiar))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Recibe apoyo familiar por parte de la familia",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(clases_pagadas), y = nota3, fill = as.factor(clases_pagadas))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Educaciones pagadas extra",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(actividades), y = nota3, fill = as.factor(actividades))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Participación en actividades extracurriculares",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(jardin_infantes), y = nota3, fill = as.factor(jardin_infantes))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Asistió al jardin de infantes",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(quiere_superior), y = nota3, fill = as.factor(quiere_superior))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Quiere continuar estudios superiores",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(internet), y = nota3, fill = as.factor(internet))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Acceso a internet",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(pareja), y = nota3, fill = as.factor(pareja))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Está en pareja",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(relacion_familiar), y = nota3, fill = as.factor(relacion_familiar))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Calidad de la relación familiar",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(tiempo_libre), y = nota3, fill = as.factor(tiempo_libre))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Tiempo libre después de la escuela",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(salidas_amigos), y = nota3, fill = as.factor(salidas_amigos))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Frecuencia con la que sale con amigos",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(alcohol_semana), y = nota3, fill = as.factor(alcohol_semana))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Consumo de alcohol en días de semana",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(alcohol_finde), y = nota3, fill = as.factor(alcohol_finde))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Consumo de alcohol durante el fin de semana",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = as.factor(salud), y = nota3, fill = as.factor(salud))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Estado de salud actual",
       y = "Nota 3") +
  theme_minimal() +
  theme(legend.position = "none")

# Estandarización de variables numéricas:

datos_std <- datos %>%
  mutate(across(c(edad, tiempo_viaje, tiempo_estudio, reprobaciones_previas, ausencias), 
                ~ scale(.) %>% as.vector()))

datos=datos_std

# De acuerdo a los gráficos vemos cuales variables sacamos y con cuales nos quedamos

#NOS QUEDAMOS CON : reprobaciones previas, edad y tiempo de estudio, tiempo de viaje, ausencias (cuant)
#educación de la madre y del padre, trabajo de la madre y del padre, apoyo escolar, 
#quiere superior, internet, relación familiar, tiempo libre, salidas con amigos, alcohol semana y finde y salud (cuali)

#### Modelos de regresión lineal normal bayesiana ####

# Propuesta y ajuste de modelos: ¿Qué subconjunto de variables resulta razonable para construir un modelo interpretable?

# 1) Modelo completo (todas las variables seleccionadas mirando los gráfico)

# Yi ~ N(μ_i, σ^2)
# μ_i = β0 +
#         + β1*(reprobaciones_previas) + β2*(edad) + β3*(tiempo_estudio) + β4*(tiempo_viaje)
#         + β5*(educacion_madre_primaria) + β6*(educacion_madre_sec_incompleta) + β7*(educacion_madre_secundaria) + β8*(educacion_madre_superior)
#         + β9*(educacion_padre_primaria) + β10*(educacion_padre_sec_incompleta) + β11*(educacion_padre_secundaria) + β12*(educacion_padre_superior)
#         + β13*(trabajo_madre_health) + β14*(trabajo_madre_other) + β15*(trabajo_madre_services) + β16*(trabajo_madre_teacher)
#         + β17*(trabajo_padre_health) + β18*(trabajo_padre_other) + β19*(trabajo_padre_services) + β20*(trabajo_padre_teacher)
#         + β21*(apoyo_escolar_yes)
#         + β22*(quiere_superior_yes)
#         + β23*(internet_yes)
#         + β24*(relacion_familiar_2) + β25*(relacion_familiar_3) + β26*(relacion_familiar_4) + β27*(relacion_familiar_5)
#         + β28*(tiempo_libre_2) + β29*(tiempo_libre_3) + β30*(tiempo_libre_4) + β31*(tiempo_libre_5)
#         + β32*(salidas_con_amigos_2) + β33*(salidas_con_amigos_3) + β34*(salidas_con_amigos_4) + β35*(salidas_con_amigos_5)
#         + β36*(alcohol_semana_2) + β37*(alcohol_semana_3) + β38*(alcohol_semana_4) + β39*(alcohol_semana_5)
#         + β40*(alcohol_finde_2) + β41*(alcohol_finde_3) + β42*(alcohol_finde_4) + β43*(alcohol_finde_5)
#         + β44*(salud_2) + β45*(salud_3) + β46*(salud_4) + β47*(salud_5) + β48*(ausencias)
#         + ε

# donde Yi: Nota final del i-ésimo alumno en la asigntaura Estadística

#### Ajuste del Modelo Final en Stan ####

library(rstan)
library(bayesplot)
library(loo)

# Opciones de optimización para RStan
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# 1. Crear la matriz de diseño 'X' con todas las variables requeridas.
# La función 'model.matrix' toma tus factores en R y expande automáticamente las variables de nivel 2, 3, 4, 5
# y las dicotómicas (yes/no) omitiendo la categoría de referencia.
# Asegúrate de usar el dataframe con las cuantitativas previamente estandarizadas (ej. datos_std).

X_completo <- model.matrix(
  nota3 ~ reprobaciones_previas + edad + tiempo_estudio + tiempo_viaje +
    educacion_madre + educacion_padre + trabajo_madre + trabajo_padre +
    apoyo_escolar + quiere_superior + internet + relacion_familiar +
    tiempo_libre + salidas_amigos + alcohol_semana + alcohol_finde + salud + ausencias,
  data = datos_std
)

# Eliminamos la primera columna de la matriz generada porque corresponde al Intercepto automático de R, 
# y en nuestro código de Stan ya declaramos 'beta0' de forma independiente.
X_completo <- X_completo[, -1]

# Verificamos que contenga exactamente los 47 predictores requeridos
print(ncol(X_completo)) 
nombres_betas <- colnames(X_completo) # Guardamos los nombres para los gráficos (beta_1 a beta_47)

# 2. Armar la lista de datos estructurada para Stan
stan_data_completo <- list(
  N     = nrow(datos_std),
  K     = ncol(X_completo),
  X     = X_completo,
  nota3 = datos_std$nota3
)

# 3. Ajustar el modelo usando MCMC con 4 cadenas
modelo_completo_fit <- sampling(
  object  = stan_model("modelo_notas.stan"), 
  data    = stan_data_completo,
  chains  = 4,
  iter    = 2000,                            # 1000 iteraciones de Warmup y 1000 de muestreo por cadena
  seed    = 1211,                            # Semilla consistente con tu archivo qmd de Australia
  refresh = 200
)

modelo_completo_fit

#### Filtrado de Efectos a Posteriori y Selección de Variables ####

# 1. Extraer el resumen de los parámetros (medias de los betas)
summary_completo <- summary(modelo_completo_fit, pars = "beta")$summary

# Asignamos los nombres de las variables expandidas (las 47 dummies/cuantitativas)
rownames(summary_completo) <- nombres_betas

# Convertir a dataframe para manipularlo fácilmente con el tidyverse
df_efectos <- as.data.frame(summary_completo) %>% 
  select(mean, `2.5%`, `97.5%`) %>% 
  rownames_to_column(var = "variable")

# 2. Identificar las variables que queremos EXCLUIR 
# (aquellas cuya media posterior está entre -0.7 y 0.7)
variables_a_excluir <- df_efectos %>% 
  filter(mean >= -0.7 & mean <= 0.7) %>% 
  pull(variable)

cat("Variables que se eliminarán por tener efectos insignificantes (media entre -0.2 y 0.2):\n")
print(variables_a_excluir)

# 3. Identificar las variables que SÍ se quedan en el modelo
variables_a_mantener <- df_efectos %>% 
  filter(mean < -0.7 | mean > 0.7) %>% 
  pull(variable)

# 4. Mapear los nombres de las columnas 'dummy' a los nombres de los factores originales
# Esto es necesario porque 'model.matrix' crea nombres como 'salud2', 'salud3', 
# pero en el dataframe original la variable se llama simplemente 'salud'.
factores_originales <- c("reprobaciones_previas", "edad", "tiempo_estudio", "tiempo_viaje",
                         "educacion_madre", "educacion_padre", "trabajo_madre", "trabajo_padre",
                         "apoyo_escolar", "quiere_superior", "internet", "relacion_familiar",
                         "tiempo_libre", "salidas_con_amigos", "alcohol_semana", "alcohol_finde", "salud")

# Filtramos qué factores originales tienen al menos una categoría seleccionada para quedarse
variables_finales_r <- factores_originales[sapply(factores_originales, function(f) {
  any(grepl(f, variables_a_mantener))
})]

# 5. Construir dinámicamente la nueva matriz de diseño reducida
formula_reducida_txt <- paste("nota3 ~", paste(variables_finales_r, collapse = " + "))
cat("\nNueva especificación del modelo:\n", formula_reducida_txt, "\n")

formula_reducida <- as.formula(formula_reducida_txt)
X_reducido <- model.matrix(formula_reducida, data = datos_std)[, -1]

# 6. Preparar datos y ajustar el nuevo modelo en Stan
stan_data_reducido <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido),
  X     = X_reducido,
  nota3 = datos_std$nota3
)

# Volvemos a usar tu modelo_completo.stan ya que su estructura matricial 
# se adapta automáticamente a cualquier cantidad de columnas (K)
modelo_reducido_fit <- sampling(
  object  = stan_model("modelo_notas.stan"), 
  data    = stan_data_reducido,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211,                            
  refresh = 500
)

modelo_reducido_fit

# Guardar nombres para los gráficos del nuevo modelo
nombres_betas_reducido <- colnames(X_reducido)

# 2) Modelos univariados 

#### Ajuste de Modelos Univariados en Bloque ####

#### Filtrado de Efectos Preservando Factores Juntos ####

# 1. Extraer las medias del posterior del modelo completo
summary_completo <- summary(modelo_completo_fit, pars = "beta")$summary
rownames(summary_completo) <- nombres_betas
df_efectos <- as.data.frame(summary_completo) %>% 
  select(mean) %>% 
  rownames_to_column(var = "variable_dummy")

# 2. Definir el listado de factores originales tal como están en tu dataset
factores_originales <- c(
  "reprobaciones_previas", "edad", "tiempo_estudio", "tiempo_viaje",
  "educacion_madre", "educacion_padre", "trabajo_madre", "trabajo_padre",
  "apoyo_escolar", "quiere_superior", "internet", "relacion_familiar",
  "tiempo_libre", "salidas_con_amigos", "alcohol_semana", "alcohol_finde", "salud"
)

# 3. Evaluar qué factores tienen AL MENOS UNA dummy con un efecto relevante
factores_a_mantener <- c()

for (factor in factores_originales) {
  # Filtrar todas las dummies que pertenecen a este factor específico
  dummies_del_factor <- df_efectos %>% 
    filter(grepl(factor, variable_dummy))
  
  # Criterio: ¿Alguna de las dummies de este factor tiene una media fuera de [-0.7, 0.7]?
  tiene_efecto_relevante <- any(dummies_del_factor$mean < -0.7 | dummies_del_factor$mean > 0.7)
  
  if (tiene_efecto_relevante) {
    factores_a_mantener <- c(factores_a_mantener, factor)
  }
}

cat("Factores originales seleccionados que se mantienen completos:\n")
print(factores_a_mantener)

# 4. Construir la fórmula dinámica para la matriz de diseño reducida
formula_reducida_txt <- paste("nota3 ~", paste(factores_a_mantener, collapse = " + "))
cat("\nNueva especificación estructural del modelo:\n", formula_reducida_txt, "\n")

formula_reducida <- as.formula(formula_reducida_txt)

# 5. Generar la nueva matriz de diseño X manteniendo las dummies unidas
X_reducido <- model.matrix(formula_reducida, data = datos_std)[, -1]

#### Ajuste de Modelos Univariados por Bloque/Factor ####
#### Ajuste de Modelos Univariados por Bloque (Corregido sin Cruces) ####

lista_modelos_univariados <- list()
lista_loo_univariados <- list()
lista_betas_univariados <- list() 

# Compilar el modelo Stan una única vez
modelo_stan_compilado <- stan_model("modelo_notas.stan")

for (factor in factores_a_mantener) {
  cat("\n----------------------------------------------------------------\n")
  cat("Ajustando modelo univariado para el bloque:", factor, "\n")
  
  # CORRECCIÓN CLAVE: Usamos ^ para buscar columnas que EMPIECEN exactamente 
  # con el nombre del factor. Así "salud" no arrastra a "trabajo_madre_health".
  columnas_factor <- colnames(X_reducido)[grepl(paste0("^", factor), colnames(X_reducido))]
  
  # Si por la estructura de nombres no encuentra nada, probamos coincidencia exacta de palabra
  if(length(columnas_factor) == 0) {
    columnas_factor <- colnames(X_reducido)[colnames(X_reducido) == factor]
  }
  
  # Mensaje de control para que veas qué columnas entran en este modelo
  cat("Columnas incluidas en este modelo:", paste(columnas_factor, collapse = ", "), "\n")
  
  # Extraer submatriz limpia
  X_uni <- X_reducido[, columnas_factor, drop = FALSE]
  
  # Datos para Stan
  stan_data_uni <- list(
    N     = nrow(datos_std),
    K     = ncol(X_uni),
    X     = X_uni,
    nota3 = datos_std$nota3
  )
  
  # Muestreo MCMC
  fit_uni <- sampling(
    object  = modelo_stan_compilado,
    data    = stan_data_uni,
    chains  = 4,
    iter    = 2000,
    seed    = 1211,
    refresh = 0
  )
  
  # Guardar objetos
  lista_modelos_univariados[[factor]] <- fit_uni
  lista_loo_univariados[[factor]] <- loo(fit_uni, pars = "log_likelihood")
  
  # Extraer summary de los Betas asignando sus nombres reales limpios
  summary_betas <- summary(fit_uni, pars = "beta")$summary
  rownames(summary_betas) <- columnas_factor
  
  lista_betas_univariados[[factor]] <- summary_betas
  
  # Imprimir los betas en la consola
  cat("Betas estimados a posteriori para", factor, ":\n")
  print(round(summary_betas[, c("mean", "sd", "2.5%", "97.5%", "Rhat"), drop = FALSE], 4))
}

#### Ranking Estructural de Factores Univariados ####

tabla_comparativa_uni <- data.frame(
  Factor   = names(lista_loo_univariados),
  elpd_loo = sapply(lista_loo_univariados, function(l) l$estimates["elpd_loo", "Estimate"]),
  se_elpd  = sapply(lista_loo_univariados, function(l) l$estimates["elpd_loo", "SE"])
)

# Ordenar el ranking de mejor a peor según su capacidad explicativa aislada
tabla_comparativa_uni <- tabla_comparativa_uni[order(-tabla_comparativa_uni$elpd_loo), ]
cat("\nRanking de factores univariados completos:\n")
print(tabla_comparativa_uni)


# nos quedamos con : 
#reprobaciones previas
#educacion madre
#trabajo madre
#apoyo escolar
#quiere superior
#internet
#salud

#3) Modelo 3 (mirando los betas a posteriori univariados): 

#μ_i = β0 
#          + β1  * (reprobaciones_previas_i)
#          + β2  * (educacion_madre_primaria_i)
#          + β3  * (educacion_madre_sec_incompleta_i)
#          + β4  * (educacion_madre_secundaria_i)
#          + β5  * (educacion_madre_superior_i)
#          + β6 * (trabajo_madre_health_i)
#          + β7 * (trabajo_madre_other_i)
#          + β12 * (trabajo_madre_services_i)
#          + β13 * (trabajo_madre_teacher_i)
#          + β14 * (apoyo_escolar_yes_i)
#          + β15 * (quiere_superior_yes_i)
#          + β16 * (internet_yes_i)
#          + β17 * (salud_2_i)
#          + β18 * (salud_3_i)
#          + β19 * (salud_4_i)
#          + β20 * (salud_5_i)

#### Ajuste del Modelo Reducido Seleccionado (20 Betas) ####


# 1. Armar la matriz de diseño 'X' especificando únicamente las 8 variables finales
# model.matrix se encarga de crear las 20 columnas numéricas resultantes
X_reducido_3 <- model.matrix(
  nota3 ~ reprobaciones_previas + educacion_madre + 
    trabajo_madre + apoyo_escolar + quiere_superior + internet + salud,
  data = datos_std
)

# Eliminamos la primera columna (el Intercepto automático de R) 
# porque en Stan ya mapeamos de forma independiente a 'beta0'
X_reducido_3 <- X_reducido_3[, -1]

# Guardamos los nombres exactos de las 20 dummies/cuantitativas para las tablas y gráficos
nombres_betas_3 <- colnames(X_reducido_3)

# Verificación de control para el equipo en la consola:
cat("Número de predictores generados para Stan:", ncol(X_reducido_3), "\n")
print(nombres_betas_3)

# 2. Armar la lista de datos estructurada para Stan
stan_data_reducido_3 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido_3), # K va a ser exactamente 20
  X     = X_reducido_3,
  nota3 = datos_std$nota3
)

# 3. Ajustar el modelo usando MCMC con el archivo .stan matricial
# Reutilizamos 'modelo_completo_notas.stan' porque su álgebra matricial (X * beta)
# se adapta de forma perfecta a cualquier cantidad de columnas K.
modelo_reducido_3_fit <- sampling(
  object  = stan_model("modelo_notas3.stan"), 
  data    = stan_data_reducido_3,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211,                            
  refresh = 500
)

modelo_reducido_3_fit

# 4. Extraer y renombrar los coeficientes para el informe
summary_modelo3 <- summary(modelo_reducido_3_fit, pars = "beta")$summary
rownames(summary_modelo3) <- nombres_betas_3

cat("\n--- RESULTADOS DEL MODELO REDUCIDO BAYESIANO ---\n")
print(round(summary_modelo3[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))

#4) Modelo 4 (mirando los betas a posteriori del modelo 3):

#    μ_i = β_0 
#          + β_1 * (reprobaciones_previas_i)
#          + β_6 * (quiere_superior_yes_i)
#          + β_7 * (salud_2_i)
#          + β_8 * (salud_3_i)
#          + β_9 * (salud_4_i)
#          + β_10 * (salud_5_i)

################################################################################
# 3) MODELO REDUCIDO SELECCIONADO - ESPECIFICACIÓN SIMBÓLICA (4 VARIABLES)
#
# ESTRUCTURA FORMAL DEL MODELO REGRESIVO LINEAL NORMAL BAYESIANO:
#
# A) VEROSIMILITUD (LIKELIHOOD):
#    Y_i | μ_i, σ² ~ i.i.d. Normal(μ_i, σ²)    ∀ i ∈ {1, ..., 395}
#
#    Donde:
#    - Y_i: Escalar ∈ [0, 10] que representa la nota final (nota3) del i-ésimo alumno.
#    - μ_i: Esperanza condicional de la variable respuesta E(Y_i | X_i).
#    - σ²: Varianza residual del término de error estocástico ε_i ~ N(0, σ²).
#
# B) COMPONENTE SISTEMÁTICO (ECUACIÓN DE LA MEDIA μ_i):
#    μ_i = β_0 + ∑_{j=1}^{10} β_j * X_{i,j}
#
#    Expansión simbólica de los términos indicadores (Dummies):
#    μ_i = β_0 
#          + β_1 * (reprobaciones_previas_i)
#          + β_6 * (quiere_superior_yes_i)
#          + β_7 * (salud_2_i)
#          + β_8 * (salud_3_i)
#          + β_9 * (salud_4_i)
#          + β_10 * (salud_5_i)
#
#    Categorías Basales de Referencia (absorbidas en el intercepto β_0):
#    X_0 = {educación_madre: sin educación, quiere_superior: no, salud: 1}
#
# C) DISTRIBUCIONES A PRIORI (PRIORS):
#    β_0 ~ Normal(μ = 5.0,  σ² = 2.5²)
#    β_j ~ Normal(μ = 0.0,  σ² = 2.0²)    ∀ j ∈ {1, ..., 10}
#    σ   ~ Normal⁺(μ = 0.0, σ² = 3.0²)   Soporte: σ ∈ (0, +∞)
#
################################################################################

#### Ajuste del Modelo Reducido Selección Especifica (11 Coeficientes Totales) ####

# 1. Crear la matriz de diseño 'X' con el set específico de 4 variables
X_reducido_4 <- model.matrix(
  nota3 ~ reprobaciones_previas + quiere_superior + salud,
  data = datos_std
)

# Eliminamos la columna del intercepto automático de R (la primera)
X_reducido_4 <- X_reducido_4[, -1]

# Guardamos los nombres de las columnas resultantes para las tablas y gráficos
nombres_betas_4 <- colnames(X_reducido_4)

# Control pedagógico en consola (debe dar exactamente 10 columnas)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_reducido_4), "\n")

# 2. Armar la lista de datos estructurada para el sampler de Stan
stan_data_reducido_4 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido_4), # K = 10
  X     = X_reducido_4,
  nota3 = datos_std$nota3
)

# 3. Ejecutar el sampler MCMC de Stan
modelo_reducido_4_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_reducido_4,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

# 4. Extraer el resumen posterior de los parámetros
summary_4 <- summary(modelo_reducido_4_fit, pars = "beta")$summary
rownames(summary_4) <- nombres_betas_4

# Imprimir los resultados ordenados en la consola de R
cat("\n--- ESTIMACIONES POSTERIORI DEL MODELO SELECCIONADO ---\n")
print(round(summary_4[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))

# Modelo 5 (a criterio propio):

#μ_i = β_0 
#          + β_1 * (sexo_M_i)
#          + β_2 * (tiempo_estudio_i)
#          + β_3 * (quiere_superior_yes_i)
#          + β_4 * (ausencias_i)


#### Ajuste del Modelo Reducido (5 Coeficientes Totales) ####

# 1. Crear la matriz de diseño 'X' con el set específico de las 4 variables solicitadas
X_reducido_5 <- model.matrix(
  nota3 ~ sexo + tiempo_estudio + quiere_superior + ausencias,
  data = datos_std
)

# Eliminamos la columna del intercepto automático de R (la primera)
X_reducido_5 <- X_reducido_5[, -1]

# Guardamos los nombres de las columnas resultantes para las tablas y gráficos
nombres_betas_5 <- colnames(X_reducido_5)

# Control pedagógico en consola (debe dar exactamente 4 columnas explicativas)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_reducido_5), "\n")
print(nombres_betas_5)

# 2. Armar la lista de datos estructurada para el sampler de Stan
stan_data_reducido_5 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido_5), # K = 4
  X     = X_reducido_5,
  nota3 = datos_std$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (reutilizando tu estructura matricial genérica)
modelo_reducido_5_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_reducido_5,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

# 4. Extraer el resumen posterior de los parámetros
summary_5 <- summary(modelo_reducido_5_fit, pars = "beta")$summary
rownames(summary_5) = nombres_betas_5

# Imprimir los resultados ordenados en la consola de R
cat("\n--- ESTIMACIONES POSTERIORI DEL MODELO SELECCIONADO ---\n")
print(round(summary_5[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))

# Modelo 6 (a criterio propio):

#μ_i = β_0 
#          + β_1 * (sexo_M_i)
#          + β_2 * (tiempo_estudio_i)
#          + β_3 * (ausencias_i)
#          + β_4 * (reprobaciones_previas_i)


#### Ajuste del Modelo Reducido (5 Coeficientes Totales) ####

# 1. Crear la matriz de diseño 'X' con el set específico de las 4 variables solicitadas
X_reducido_6 <- model.matrix(
  nota3 ~ sexo + tiempo_estudio + ausencias + reprobaciones_previas,
  data = datos_std
)

# Eliminamos la columna del intercepto automático de R (la primera)
X_reducido_6 <- X_reducido_6[, -1]

# Guardamos los nombres de las columnas resultantes para las tablas y gráficos
nombres_betas_6 <- colnames(X_reducido_6)

# Control pedagógico en consola (debe dar exactamente 4 columnas explicativas)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_reducido_6), "\n")
print(nombres_betas_6)

# 2. Armar la lista de datos estructurada para el sampler de Stan
stan_data_reducido_6 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido_6), # K = 4
  X     = X_reducido_6,
  nota3 = datos_std$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (reutilizando tu estructura matricial genérica)
modelo_reducido_6_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_reducido_6,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

# 4. Extraer el resumen posterior de los parámetros
summary_6 <- summary(modelo_reducido_6_fit, pars = "beta")$summary
rownames(summary_6) = nombres_betas_6

# Imprimir los resultados ordenados en la consola de R
cat("\n--- ESTIMACIONES POSTERIORI DEL MODELO SELECCIONADO ---\n")
print(round(summary_6[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))

# Modelo 7 (a criterio propio):

#μ_i = β_0 
#          + β_1 * (sexo_M_i)
#          + β_2 * (domicilio_urbano_i)
#          + β_3 * (ausencias_i)
#          + β_4 * (reprobaciones_previas_i)


#### Ajuste del Modelo Reducido (5 Coeficientes Totales) ####

# 1. Crear la matriz de diseño 'X' con el set específico de las 4 variables solicitadas
X_reducido_7 <- model.matrix(
  nota3 ~ sexo + domicilio + ausencias + reprobaciones_previas,
  data = datos_std
)

# Eliminamos la columna del intercepto automático de R (la primera)
X_reducido_7 <- X_reducido_7[, -1]

# Guardamos los nombres de las columnas resultantes para las tablas y gráficos
nombres_betas_7 <- colnames(X_reducido_7)

# Control pedagógico en consola (debe dar exactamente 4 columnas explicativas)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_reducido_7), "\n")
print(nombres_betas_7)

# 2. Armar la lista de datos estructurada para el sampler de Stan
stan_data_reducido_7 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido_7), # K = 4
  X     = X_reducido_7,
  nota3 = datos_std$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (reutilizando tu estructura matricial genérica)
modelo_reducido_7_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_reducido_7,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

# 4. Extraer el resumen posterior de los parámetros
summary_7 <- summary(modelo_reducido_7_fit, pars = "beta")$summary
rownames(summary_7) = nombres_betas_7

# Imprimir los resultados ordenados en la consola de R
cat("\n--- ESTIMACIONES POSTERIORI DEL MODELO SELECCIONADO ---\n")
print(round(summary_7[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))

# Modelo 8 (sacando domicilio):

#μ_i = β_0 
#          + β_1 * (sexo_M_i)
#          + β_2 * (ausencias_i)
#          + β_3 * (reprobaciones_previas_i)


#### Ajuste del Modelo Reducido (4 Coeficientes Totales) ####

# 1. Crear la matriz de diseño 'X' con el set específico de las 4 variables solicitadas
X_reducido_8 <- model.matrix(
  nota3 ~ sexo + ausencias + reprobaciones_previas,
  data = datos_std
)

# Eliminamos la columna del intercepto automático de R (la primera)
X_reducido_8 <- X_reducido_8[, -1]

# Guardamos los nombres de las columnas resultantes para las tablas y gráficos
nombres_betas_8 <- colnames(X_reducido_8)

# Control pedagógico en consola (debe dar exactamente 4 columnas explicativas)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_reducido_8), "\n")
print(nombres_betas_8)

# 2. Armar la lista de datos estructurada para el sampler de Stan
stan_data_reducido_8 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido_8), # K = 4
  X     = X_reducido_8,
  nota3 = datos_std$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (reutilizando tu estructura matricial genérica)
modelo_reducido_8_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_reducido_8,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

# 4. Extraer el resumen posterior de los parámetros
summary_8 <- summary(modelo_reducido_8_fit, pars = "beta")$summary
rownames(summary_8) = nombres_betas_8

# Imprimir los resultados ordenados en la consola de R
cat("\n--- ESTIMACIONES POSTERIORI DEL MODELO SELECCIONADO ---\n")
print(round(summary_8[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))


# Modelos propuestos para el análisis:      (3 o 4 modelos aprox)

# PRUEBAS PREDICTIVAS A PRIORI

################################################################################
#     PRUEBAS PREDICTIVAS A PRIORI (MODELOS 4, 5, 6, 7 Y 8)
#             Priors: β0 ~ N(0, 100), βj ~ N(0, 10), σ ~ N+(0, 1)
################################################################################

library(ggplot2)
library(tidyverse)

# 1. Configurar reproducibilidad y número de simulaciones a priori
set.seed(1211)
n_sim <- 1000 # Cantidad de réplicas de bases de datos a priori
N_obs <- nrow(datos_std) # 395 estudiantes

# --- Preparación de las Matrices de Diseño Reales ---
X4 <- model.matrix(nota3 ~ reprobaciones_previas + educacion_madre + quiere_superior + salud, data = datos_std)[, -1]
X5 <- model.matrix(nota3 ~ sexo + tiempo_estudio + quiere_superior + ausencias, data = datos_std)[, -1]
X6 <- model.matrix(nota3 ~ sexo + tiempo_estudio + quiere_superior + reprobaciones_previas, data = datos_std)[, -1]
X7 <- model.matrix(nota3 ~ sexo + domicilio + quiere_superior + reprobaciones_previas, data = datos_std)[, -1]
X8 <- model.matrix(nota3 ~ sexo + tiempo_estudio + quiere_superior + ausencias, data = datos_std)[, -1] 

matrices_diseno <- list(
  "Modelo 4" = X4, 
  "Modelo 5" = X5, 
  "Modelo 6" = X6, 
  "Modelo 7" = X7,
  "Modelo 8" = X8
)

# --- Contenedor final para el gráfico ---
df_prior_pred_total <- data.frame()

# 2. Bucle de Simulación Genérico por Modelo
for (nom_mod in names(matrices_diseno)) {
  
  X_mat <- matrices_diseno[[nom_mod]]
  K_vars <- ncol(X_mat)
  
  # Matriz para almacenar las notas simuladas (n_sim filas x N_obs columnas)
  y_prior_mat <- matrix(nrow = n_sim, ncol = N_obs)
  
  # Simular los hiperparámetros directos según las especificaciones pedidas
  beta0_sim <- rnorm(n_sim, mean = 5, sd = 1.7)    # sd = 10 ya que la varianza es 100 (Normal(0,100))
  sigma_sim <- abs(rnorm(n_sim, mean = 0, sd = 1)) # Normal truncada positiva para el error N+(0,1)
  
  # Muestreo de cada pendiente βj ~ Normal(0, sqrt(10)) ya que la varianza pedida es 10
  beta_sim_matrix <- matrix(rnorm(n_sim * K_vars, mean = 0, sd = 2.5), 
                            nrow = n_sim, ncol = K_vars)
  
  # Generación de los datos sintéticos predictivos a priori
  for (s in 1:n_sim) {
    # μ_i = β0 + X * β
    mu_i <- beta0_sim[s] + as.vector(X_mat %*% beta_sim_matrix[s, ])
    
    # Y_i ~ Normal(μ_i, σ)
    y_prior_mat[s, ] <- rnorm(N_obs, mean = mu_i, sd = sigma_sim[s])
  }
  
  # Formatear una muestra representativa de las réplicas para el gráfico de densidad
  # Seleccionamos las primeras 50 réplicas completas para mantener el gráfico legible
  df_mod_long <- as.data.frame(t(y_prior_mat[1:50, ])) %>%
    mutate(obs_id = 1:n()) %>%
    pivot_longer(cols = starts_with("V"), names_to = "replica", values_to = "nota_sim") %>%
    mutate(Modelo = nom_mod)
  
  df_prior_pred_total <- rbind(df_prior_pred_total, df_mod_long)
}

# 3. Construcción del Gráfico de Pruebas Predictivas A Priori (Facetado de 5 modelos)
ggplot() +
  # Líneas de simulación a priori (gris para denotar la dispersión teórica vaga)
  geom_density(data = df_prior_pred_total, 
               aes(x = nota_sim, group = replica), 
               color = "gray80", alpha = 0.4, linewidth = 0.3) +
  facet_wrap(~ Modelo, scales = "free_y", nrow = 2) +
  labs(
    caption = "Pruebas Predictivas A Priori",
    x = "Nota final",
    y = "Densidad"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "#264653"),
    strip.text = element_text(color = "white", face = "bold")
  )

# Visualización de bandas de regresión a priori solo para el modelo 8

set.seed(1211)
n_sim <- 1000
N_obs <- nrow(datos_std)

# 1. Recuperar la matriz de diseño del Modelo 8 (sexo + tiempo_estudio + quiere_superior + ausencias)
K_vars <- ncol(X8)

# Matriz para almacenar las predicciones a priori (filas: simulaciones, columnas: estudiantes)
y_prior_m8 <- matrix(nrow = n_sim, ncol = N_obs)

# 2. Simular parámetros bajo la distribución a priori vágamente informativa definida
beta0_sim <- rnorm(n_sim, mean = 5, sd = 1.7)    
sigma_sim <- abs(rnorm(n_sim, mean = 0, sd = 1)) 
beta_sim_matrix <- matrix(rnorm(n_sim * K_vars, mean = 0, sd = 2.5), nrow = n_sim, ncol = K_vars)

# 3. Generar la distribución predictiva a priori conjunta
for (s in 1:n_sim) {
  mu_i <- beta0_sim[s] + as.vector(X8 %*% beta_sim_matrix[s, ])
  y_prior_m8[s, ] <- rnorm(N_obs, mean = mu_i, sd = sigma_sim[s])
}

# 4. Calcular los intervalos del 50% y 95% para cada estudiante a lo largo de las observaciones
# Usamos el índice de la observación (1:N_obs) como nuestro eje X estructural
df_y_prior_50_ci <- apply(y_prior_m8, 2, quantile, probs = c(0.25, 0.75)) |>
  t() |>
  as.data.frame() |>
  mutate(x = 1:N_obs) |>
  rename(li = `25%`, ls = `75%`)

df_y_prior_95_ci <- apply(y_prior_m8, 2, quantile, probs = c(0.025, 0.975)) |>
  t() |>
  as.data.frame() |>
  mutate(x = 1:N_obs) |>
  rename(li = `2.5%`, ls = `97.5%`)

# 5. Construcción del gráfico sin observaciones reales (Únicamente la dispersión teórica a priori)
ggplot() +
  # Banda exterior de mayor cobertura (95%)
  geom_ribbon(
    data = df_y_prior_95_ci,
    aes(x = x, ymin = li, ymax = ls, fill = "Intervalo 95%"),
    alpha = 0.25
  ) +
  # Banda interior (50%)
  geom_ribbon(
    data = df_y_prior_50_ci,
    aes(x = x, ymin = li, ymax = ls, fill = "Intervalo 50%"),
    alpha = 0.45
  ) +
  # Configuración estética consistente con el trabajo
  labs(
    x = "Alumnos seleccionados",
    y = "Calificación Final a Priori",
    fill = "Bandas A Priori"
  ) +
  scale_fill_manual(values = c("Intervalo 50%" = "#491211", "Intervalo 95%" = "#C6B39A")) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  ) + ylim(-20,30)

# PRIOR PREDICTIVE CHECK PARA EL MODELO 8
set.seed(1211)
s <- 100 # Cantidad de iteraciones/réplicas requeridas
N_obs <- nrow(datos_std)

# Extraer la matriz de diseño del Modelo 8 (4 predictores)
K_vars <- ncol(X8)

# Simular los vectores de hiperparámetros a priori globales (las muestras "_rvs")
beta0_rvs <- rnorm(s, mean = 5, sd = 1.7)
sigma_rvs <- abs(rnorm(s, mean = 0, sd = 1))
beta_rvs_matrix <- matrix(rnorm(s * K_vars, mean = 0, sd = 2.5), nrow = s, ncol = K_vars)


# --- 1. Simulación para una sola réplica (Tu primera estructura) ---
# Multiplicación matricial %*% para combinar las 4 pendientes con los datos de los alumnos
mu_unica <- beta0_rvs[1] + as.vector(X8 %*% beta_rvs_matrix[1, ])
y_pp <- rnorm(N_obs, mean = mu_unica, sd = sigma_rvs[1])

# Gráfico aislado de densidad para esa única réplica
ggplot(data.frame(x = y_pp)) + 
  geom_density(aes(x = x), color = "darkred", linewidth = 1) +
  labs(title = "Densidad A Priori - Una Sola Réplica (Modelo 8)", x = "Nota Simulada", y = "Densidad") +
  theme_bw()


# --- 2. Simulación matricial iterativa (Tu segunda estructura con el bucle for) ---
# Inicializamos la matriz vacía con s filas y N_obs columnas
y_pp_matrix <- matrix(0, nrow = s, ncol = N_obs)

# El for recorre las FILAS y devuelve la base de datos sintética completa en cada iteración
for(j in seq_len(s)){
  # Componente sistemático matricial para la iteración j
  mu_j <- beta0_rvs[j] + as.vector(X8 %*% beta_rvs_matrix[j, ])
  
  y_pp_matrix[j, ] <- rnorm(N_obs, 
                            mean = mu_j, 
                            sd = sigma_rvs[j])
}


# --- 3. Construcción del Gráfico Colectivo (Overlay puro sin datos reales) ---
# Transponemos la matriz para que cada columna sea una réplica y pivotamos a formato largo
df_overlay_m8 <- as.data.frame(t(y_pp_matrix)) %>%
  mutate(obs_id = 1:n()) %>%
  pivot_longer(
    cols = starts_with("V"), 
    names_to = "replica", 
    values_to = "nota_sim"
  )

# Renderizado final con las s curvas superpuestas
ggplot(df_overlay_m8, aes(x = nota_sim, group = replica)) +
  geom_density(color = "#491211", alpha = 0.25, linewidth = 0.4) + # Color celeste de tu script
  labs(
    x = "Nota Final a Priori",
    y = "Densidad"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12)
  )

# PRUEBAS PREDICTIVAS A POSTERIORI

################################################################################
#     PRUEBAS PREDICTIVAS A POSTERIORI (MODELOS 4, 5, 6, 7 Y 8)
################################################################################

library(ggplot2)
library(tidyverse)

set.seed(1211) # Mantener consistencia del equipo
n_replicas_grafico <- 50 # Número de líneas del posterior a dibujar por modelo
N_obs <- nrow(datos_std)

# 1. Definir la lista con los objetos de ajuste de Stan de cada modelo
modelos_ajustados <- list(
  "Modelo 4" = modelo_reducido_4_fit,
  "Modelo 5" = modelo_reducido_5_fit,
  "Modelo 6" = modelo_reducido_6_fit,
  "Modelo 7" = modelo_reducido_7_fit,
  "Modelo 8" = modelo_reducido_8_fit  
)

df_post_pred_total <- data.frame()

# 2. Extraer muestras 'y_rep' y formatear los datos para el facetado
for (nom_mod in names(modelos_ajustados)) {
  
  # Extraer la matriz completa de y_rep desde el objeto de Stan (iteraciones x observaciones)
  y_rep_completo <- rstan::extract(modelos_ajustados[[nom_mod]], "y_rep")$y_rep
  
  # Seleccionar una muestra aleatoria de filas para no sobrecargar el gráfico (ej. 50 réplicas)
  filas_muestreadas <- sample(nrow(y_rep_completo), n_replicas_grafico)
  y_rep_sub <- y_rep_completo[filas_muestreadas, ]
  
  # Transformar a formato largo compatible con ggplot
  df_mod_long <- as.data.frame(t(y_rep_sub)) %>%
    mutate(obs_id = 1:n()) %>%
    pivot_longer(cols = starts_with("V"), names_to = "replica", values_to = "nota_sim") %>%
    mutate(Modelo = nom_mod)
  
  df_post_pred_total <- rbind(df_post_pred_total, df_mod_long)
}

# 3. Estructurar el dataframe de la variable observada real para la superposición
df_real <- data.frame(
  nota_sim = datos_std$nota3,
  replica = "Datos Reales",
  Modelo = rep(names(modelos_ajustados), each = N_obs)
)

# 4. Construcción del Gráfico PPC Colectivo (Facetado)
ggplot() +
  # Líneas celestes/grises del posterior generado (y_rep)
  geom_density(data = df_post_pred_total, 
               aes(x = nota_sim, group = replica), 
               color = "#219ebc", alpha = 0.3, linewidth = 0.4) + # Color celeste temático del script
  # Curva negra/oscura gruesa para los datos observados reales (y)
  geom_density(data = df_real, 
               aes(x = nota_sim), 
               color = "#264653", linewidth = 1.3) + 
  facet_wrap(~ Modelo, scales = "free_y", nrow = 2) +
  labs(
    caption = "Chequeo Predictivo A Posteriori",
    x = "Calificación final",
    y = "Densidad"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "#e76f51"), # Naranja oscuro del encabezado del script
    strip.text = element_text(color = "white", face = "bold")
  )

# POSTERIOR PREDICTIVE CHECK SOLO DEL MODELO 8

ppc_dens_overlay(datos_std$nota3, y_rep_m8[1:50,])


# COMPARACIÓN DE MODELOS:
# con Pareto smoothed importance sampling cross validation

################################################################################
#   VALIDACIÓN CRUZADA PSIS-LOO-CV Y COMPARACIÓN FORMAL DE MODELOS
################################################################################

library(loo)

# 1. Calcular el objeto LOO para cada modelo utilizando el log-likelihood punto a punto
# (Asegúrate de que 'log_likelihood' esté definido en el bloque 'generated quantities' de tu archivo .stan)

cat("--- Calculando PSIS-LOO para cada modelo ---\n")

loo_m4 <- loo(modelo_reducido_4_fit, pars = "log_likelihood")
cat("\nResumen Modelo 4:\n"); print(loo_m4)

loo_m5 <- loo(modelo_reducido_5_fit, pars = "log_likelihood")
cat("\nResumen Modelo 5:\n"); print(loo_m5)

loo_m6 <- loo(modelo_reducido_6_fit, pars = "log_likelihood")
cat("\nResumen Modelo 6:\n"); print(loo_m6)

loo_m7 <- loo(modelo_reducido_7_fit, pars = "log_likelihood")
cat("\nResumen Modelo 7:\n"); print(loo_m7)

loo_m8 <- loo(modelo_reducido_8_fit, pars = "log_likelihood") 
cat("\nResumen Modelo 8:\n"); print(loo_m8)


# 2. Comparación formal y ordenamiento de modelos (De mejor a peor según ELPD)
# La función loo_compare los ordena automáticamente situando al mejor modelo al principio.

cat("\n----------------------------------------------------------------\n")
cat("   TABLA COMPARATIVA DE MODELOS (Muestreo de Importancia Pareto)\n")
cat("----------------------------------------------------------------\n")

tabla_comparativa_loo <- loo_compare(loo_m4, loo_m5, loo_m6, loo_m7, loo_m8)
print(tabla_comparativa_loo)

# LOS MODELOS CON MEJOR CAPACIDAD PREDICTIVA SON LOS MODELOS 6,7 Y 8 (no es significativa la diferencia)
#dado que el modelo 8 tiene 3 variables nos quedamos con ese ya que es más parsimonioso

################################################################################
#      DISTRIBUCIONES MARGINALES A POSTERIORI (MODELO 8) - SOLUCIÓN DEFINITIVA
################################################################################

library(ggplot2)
library(tidyverse)

# 1. Extraer las muestras del posterior de forma limpia
muestras_rstan <- rstan::extract(modelo_reducido_8_fit, c("beta0", "beta", "sigma"))

# 2. Reconstruir el dataframe indexando numéricamente las columnas de la matriz beta
df_draws_8 <- data.frame(
  `beta0 (Intercepto)`          = muestras_rstan$beta0,
  `beta1 (sexo_M)`              = muestras_rstan$beta[, 1],
  `beta2 (ausencias)`      = muestras_rstan$beta[, 2],
  `beta3 (reprobaciones_previas)`  = muestras_rstan$beta[, 3],
  `sigma (Error)`               = muestras_rstan$sigma,
  check.names = FALSE # Evita que R reemplace los espacios por puntos en los nombres
)

# Eliminar cualquier fila incompleta por seguridad
df_draws_8 <- na.omit(df_draws_8)

# 3. Pivotar a formato largo usando exactamente "value" (como el qmd original)
df_draws_long_8 <- df_draws_8 %>%
  tidyr::pivot_longer(
    cols = everything(), 
    names_to = "parametro", 
    values_to = "value" # <--- CLAVE: Genera la columna 'value' que busca ggplot
  )

# Color naranja claro e identidades estéticas del script guía
c_orange_light <- "#491211"

# 4. Construcción del Gráfico de Histogramas (Alineado con el qmd de Australia)
ggplot(df_draws_long_8) +
  geom_histogram(
    aes(x = value, y = after_stat(density)), # <--- Ahora 'value' sí existe
    bins = 40,
    fill = c_orange_light,
    color = "white",
    alpha = 0.8
  ) +
  facet_wrap(~ parametro, scales = "free", nrow = 2) + 
  labs(
    caption = "Distribuciones Marginales A Posteriori - Modelo 8",
    x = "Valor",
    y = "Densidad"
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "#C6B39A"), 
    strip.text = element_text(color = "white", face = "bold")
  )



# Modelo final:

#μ_i = β_0 
#          + β_1 * (sexo_M_i)
#          + β_2 * (ausencias_i)
#          + β_3 * (reprobaciones_previas_i)

#

#intervalos de credibilidad predictivos a posteriori para las diferentes observaciones.
# Forzamos a bayesplot a regresar a su esquema de colores por defecto
color_scheme_set("blue")

set.seed(1211) # Para que la selección de los 60 estudiantes sea reproducible

# 1. Extraer la matriz y_rep del modelo ajustado
y_rep_m8 <- rstan::extract(modelo_reducido_8_fit, "y_rep")$y_rep

# 2. Seleccionar exactamente 60 índices usando el dataframe que tenga los datos finales
n_mostrar <- 60
indices_60 <- sample(1:nrow(datos_std), size = n_mostrar)

# 3. Filtrar el vector real 'y' y las columnas de la matriz 'y_rep' para esos 60 casos
y_subgrupo_60 <- datos_std$nota3[indices_60]
y_rep_subgrupo_60 <- y_rep_m8[, indices_60]

# 4. Graficar pasando ÚNICAMENTE las variables filtradas del subgrupo
ppc_intervals(
  y = y_subgrupo_60,          
  yrep = y_rep_subgrupo_60,   
  prob = 0.5,       
  prob_outer = 0.95 
) +
  labs(
    caption = "Intervalos de Credibilidad Predictivos A Posteriori - Modelo 8",
    x = "Alumnos Seleccionados",
    y = "Calificación Final a Posteriori"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 8)
  )

# • ¿Cómo cambia el modelo cuando se incorpora nota1?

# modelo 8 + nota 1

#μ_i = β_0 
#          + β_1 * (sexo_M_i)
#          + β_2 * (ausencias_i)
#          + β_3 * (reprobaciones_previas_i)
#          + β_4 * (nota_1)


# 1. Crear la matriz de diseño 'X' con el set específico de las 4 variables solicitadas
X_reducido_notas1 <- model.matrix(
  nota3 ~ sexo + ausencias + reprobaciones_previas + nota1,
  data = datos
)

# Eliminamos la columna del intercepto automático de R (la primera)
X_reducido_notas1 <- X_reducido_notas1[, -1]

# Guardamos los nombres de las columnas resultantes para las tablas y gráficos
nombres_betas_notas1 <- colnames(X_reducido_notas1)

# Control pedagógico en consola (debe dar exactamente 4 columnas explicativas)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_reducido_notas1), "\n")
print(nombres_betas_notas1)

# 2. Armar la lista de datos estructurada para el sampler de Stan
stan_data_reducido_notas1 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido_notas1), 
  X     = X_reducido_notas1,
  nota3 = datos$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (reutilizando tu estructura matricial genérica)
modelo_reducido_notas1_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_reducido_notas1,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

# 4. Extraer el resumen posterior de los parámetros
summary_notas1 <- summary(modelo_reducido_notas1_fit, pars = "beta")$summary
rownames(summary_notas1) = nombres_betas_notas1

# Imprimir los resultados ordenados en la consola de R
cat("\n--- ESTIMACIONES POSTERIORI DEL MODELO SELECCIONADO ---\n")
print(round(summary_notas1[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))

loo_m_notas1 <- loo(modelo_reducido_notas1_fit, pars = "log_likelihood") 
cat("\nResumen Modelo con notas1:\n"); print(loo_m_notas1)

loo_m8

tabla_comparativa_loo <- loo_compare(loo_m8, loo_m_notas1)
print(tabla_comparativa_loo)

#modelo sólo con nota 1

# 1. Crear la matriz de diseño 'X' incorporando únicamente nota1
X_notas1 <- model.matrix(
  nota3 ~ nota1,
  data = datos # Usar el dataframe limpio 'datos_std' para evitar desajustes de filas
)

# CORRECCIÓN CLAVE: Agregamos drop = FALSE para que conserve la estructura de matriz
X_notas1 <- X_notas1[, -1, drop = FALSE]

# Ahora los nombres y columnas ya no van a dar NULL
nombres_betas_notas1 <- colnames(X_notas1)

# Control pedagógico en consola (ahora va a dar exactamente 1)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_notas1), "\n")
print(nombres_betas_notas1)

# 2. Armar la lista de datos estructurada de forma segura para Stan
stan_data_notas1 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_notas1), # K va a valer exactamente 1 de forma numérica
  X     = X_notas1,
  nota3 = datos_std$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (va a correr perfectamente de forma matricial)
modelo_notas1_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_notas1,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

modelo_notas1_fit

# • ¿Qué ocurre al incorporar también nota2?

#modelo 8 + nota 1 + nota 2

#μ_i = β_0 
#          + β_1 * (sexo_M_i)
#          + β_2 * (ausencias_i)
#          + β_3 * (reprobaciones_previas_i)
#          + β_4 * (nota_1)
#          + β_5 * (nota_2)


# 1. Crear la matriz de diseño 'X' con el set específico de las 4 variables solicitadas
X_reducido_notas12 <- model.matrix(
  nota3 ~ sexo + ausencias + reprobaciones_previas + nota1 + nota2,
  data = datos
)

# Eliminamos la columna del intercepto automático de R (la primera)
X_reducido_notas12 <- X_reducido_notas12[, -1]

# Guardamos los nombres de las columnas resultantes para las tablas y gráficos
nombres_betas_notas12 <- colnames(X_reducido_notas12)

# Control pedagógico en consola (debe dar exactamente 4 columnas explicativas)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_reducido_notas12), "\n")
print(nombres_betas_notas12)

# 2. Armar la lista de datos estructurada para el sampler de Stan
stan_data_reducido_notas12 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido_notas12), 
  X     = X_reducido_notas12,
  nota3 = datos$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (reutilizando tu estructura matricial genérica)
modelo_reducido_notas12_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_reducido_notas12,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

# 4. Extraer el resumen posterior de los parámetros
summary_notas12 <- summary(modelo_reducido_notas12_fit, pars = "beta")$summary
rownames(summary_notas12) = nombres_betas_notas12

# Imprimir los resultados ordenados en la consola de R
cat("\n--- ESTIMACIONES POSTERIORI DEL MODELO SELECCIONADO ---\n")
print(round(summary_notas12[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))

# modelo 8 + notas2

# 1. Crear la matriz de diseño 'X' con el set específico de las 4 variables solicitadas
X_reducido_notas2 <- model.matrix(
  nota3 ~ sexo + ausencias + reprobaciones_previas + nota2,
  data = datos
)


# Eliminamos la columna del intercepto automático de R (la primera)
X_reducido_notas2 <- X_reducido_notas2[, -1]

# Guardamos los nombres de las columnas resultantes para las tablas y gráficos
nombres_betas_notas2 <- colnames(X_reducido_notas2)

# Control pedagógico en consola (debe dar exactamente 4 columnas explicativas)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_reducido_notas2), "\n")
print(nombres_betas_notas2)

# 2. Armar la lista de datos estructurada para el sampler de Stan
stan_data_reducido_notas2 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_reducido_notas2), 
  X     = X_reducido_notas2,
  nota3 = datos$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (reutilizando tu estructura matricial genérica)
modelo_reducido_notas2_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_reducido_notas2,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

# 4. Extraer el resumen posterior de los parámetros
summary_notas2 <- summary(modelo_reducido_notas2_fit, pars = "beta")$summary
rownames(summary_notas2) = nombres_betas_notas2

# Imprimir los resultados ordenados en la consola de R
cat("\n--- ESTIMACIONES POSTERIORI DEL MODELO SELECCIONADO ---\n")
print(round(summary_notas2[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))


# modelo sólo con nota 2


# 1. Crear la matriz de diseño 'X' incorporando únicamente nota1
X_notas2 <- model.matrix(
  nota3 ~ nota2,
  data = datos # Usar el dataframe limpio 'datos_std' para evitar desajustes de filas
)

# CORRECCIÓN CLAVE: Agregamos drop = FALSE para que conserve la estructura de matriz
X_notas2 <- X_notas2[, -1, drop = FALSE]

# Ahora los nombres y columnas ya no van a dar NULL
nombres_betas_notas2 <- colnames(X_notas2)

# Control pedagógico en consola (ahora va a dar exactamente 1)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_notas2), "\n")
print(nombres_betas_notas2)

# 2. Armar la lista de datos estructurada de forma segura para Stan
stan_data_notas2 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_notas2), # K va a valer exactamente 1 de forma numérica
  X     = X_notas2,
  nota3 = datos_std$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (va a correr perfectamente de forma matricial)
modelo_notas2_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_notas2,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

modelo_notas2_fit

# modelo sólo con nota 1 y nota 2

# 1. Crear la matriz de diseño 'X' incorporando únicamente nota1
X_notas12 <- model.matrix(
  nota3 ~ nota1 + nota2,
  data = datos # Usar el dataframe limpio 'datos_std' para evitar desajustes de filas
)

# CORRECCIÓN CLAVE: Agregamos drop = FALSE para que conserve la estructura de matriz
X_notas12 <- X_notas12[, -1, drop = FALSE]

# Ahora los nombres y columnas ya no van a dar NULL
nombres_betas_notas12 <- colnames(X_notas12)

# Control pedagógico en consola (ahora va a dar exactamente 1)
cat("Cantidad de columnas explicativas generadas para Stan:", ncol(X_notas12), "\n")
print(nombres_betas_notas12)

# 2. Armar la lista de datos estructurada de forma segura para Stan
stan_data_notas12 <- list(
  N     = nrow(datos_std),
  K     = ncol(X_notas12), # K va a valer exactamente 1 de forma numérica
  X     = X_notas12,
  nota3 = datos_std$nota3
)

# 3. Ejecutar el sampler MCMC de Stan (va a correr perfectamente de forma matricial)
modelo_notas12_fit <- sampling(
  object  = stan_model("modelo_notas4.stan"), 
  data    = stan_data_notas12,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

modelo_notas12_fit

# COMPARACIÓN DE LOS MODELOS 8, MODELO 8 + NOTA 1, MODELO 8 + NOTA 2, MODELO 8 + NOTA 1 + NOTA 2
# MODELO NOTA 1, MODELO CON NOTA 2 , MODELO CON NOTA 1 + NOTA 2

loo_m8_notas1 <- loo(modelo_reducido_notas1_fit, pars = "log_likelihood") 
cat("\nResumen Modelo con notas1:\n"); print(loo_m8_notas1)

loo_m_notas1 <- loo(modelo_notas1_fit, pars = "log_likelihood") 
cat("\nResumen Modelo con notas1:\n"); print(loo_m_notas1)

loo_m8_notas2 <- loo(modelo_reducido_notas2_fit, pars = "log_likelihood") 
cat("\nResumen Modelo con notas1:\n"); print(loo_m8_notas2)

loo_m_notas2 <- loo(modelo_notas2_fit, pars = "log_likelihood") 
cat("\nResumen Modelo con notas1:\n"); print(loo_m_notas2)

loo_m8_notas12 <- loo(modelo_reducido_notas12_fit, pars = "log_likelihood") 
cat("\nResumen Modelo con notas1:\n"); print(loo_m8_notas12)

loo_m_notas12 <- loo(modelo_notas12_fit, pars = "log_likelihood") 
cat("\nResumen Modelo con notas1:\n"); print(loo_m_notas12)

tabla_comparativa_loo <- loo_compare(loo_m8, loo_m8_notas1, loo_m_notas1, loo_m8_notas2, 
                                     loo_m_notas2, loo_m8_notas12, loo_m_notas12 )
print(tabla_comparativa_loo)

# • ¿Es preferible un modelo con una o dos notas previas? ¿Por qué?
# Es preferible un modelo con dos notas previas ya que el modelo solo con notas 1 y 2 
# es el mejor respecto al elpd pero no es significativamente mejor que el modelo 8 
# con notas 1 y notas 2 por ( que tiene mayor elpd) por lo que retenemos el modelo
# solo con notas 1 y 2 dado que es el mas parcimonioso entre los dos mejores

################################################################################
#  ESTRATEGIA DE PARSIMONIA: EXPLORACIÓN FORWARD DESDE SOLO NOTAS 1 Y 2
################################################################################

# Recordamos la base ganadora por parsimonia (Solo notas 1 y 2):
# X_solo_notas12 <- model.matrix(nota3 ~ nota1 + nota2, data = datos_std)[, -1]

# --- 1. Construcción de Matrices de Diseño Añadiendo de a una Variable ---

# Alternativa A: Agregando 'sexo'
X_notas12_sexo <- model.matrix(nota3 ~ nota1 + nota2 + sexo, data = datos)[, -1]

# Alternativa B: Agregando 'tiempo_estudio'
X_notas12_testudio <- model.matrix(nota3 ~ nota1 + nota2 + tiempo_estudio, data = datos)[, -1]

# Alternativa C: Agregando 'quiere_superior'
X_notas12_qsuperior <- model.matrix(nota3 ~ nota1 + nota2 + quiere_superior, data = datos)[, -1]

# Alternativa D: Agregando 'ausencias'
X_notas12_ausencias <- model.matrix(nota3 ~ nota1 + nota2 + ausencias, data = datos)[, -1]


# --- 2. Ajuste de los Modelos Competidores en Stan ---

# Modelo + Sexo
fit_notas12_sexo <- sampling(
  object = stan_model("modelo_notas4.stan"), 
  data = list(N=nrow(datos), K=ncol(X_notas12_sexo), X=X_notas12_sexo, nota3=datos_std$nota3),
  chains = 4, iter = 2000, seed = 1211, refresh = 0 # refresh=0 para no saturar la consola
)

# Modelo + Tiempo de Estudio
fit_notas12_testudio <- sampling(
  object = stan_model("modelo_notas4.stan"), 
  data = list(N=nrow(datos), K=ncol(X_notas12_testudio), X=X_notas12_testudio, nota3=datos_std$nota3),
  chains = 4, iter = 2000, seed = 1211, refresh = 0
)

# Modelo + Quiere Superior
fit_notas12_qsuperior <- sampling(
  object = stan_model("modelo_notas4.stan"), 
  data = list(N=nrow(datos), K=ncol(X_notas12_qsuperior), X=X_notas12_qsuperior, nota3=datos_std$nota3),
  chains = 4, iter = 2000, seed = 1211, refresh = 0
)

# Modelo + Ausencias
fit_notas12_ausencias <- sampling(
  object = stan_model("modelo_notas4.stan"), 
  data = list(N=nrow(datos), K=ncol(X_notas12_ausencias), X=X_notas12_ausencias, nota3=datos_std$nota3),
  chains = 4, iter = 2000, seed = 1211, refresh = 0
)


# --- 3. Cálculo de Criterios LOO para Evaluar los Agregados ---

loo_notas12_sexo      <- loo(fit_notas12_sexo, pars = "log_likelihood")
loo_notas12_testudio  <- loo(fit_notas12_testudio, pars = "log_likelihood")
loo_notas12_qsuperior <- loo(fit_notas12_qsuperior, pars = "log_likelihood")
loo_notas12_ausencias <- loo(fit_notas12_ausencias, pars = "log_likelihood")


# --- 4. Comparación Formal Completa ---
# Incluimos loo_m_notas12 (el que ya tenías de solo notas 1 y 2) para ver si alguno le gana

cat("\n----------------------------------------------------------------\n")
cat("   COMPARACIÓN FORWARD: ¿ALBERGA VALOR AGREGAR ALGUNA VARIABLE?\n")
cat("----------------------------------------------------------------\n")

comparacion_forward <- loo_compare(
  loo_m_notas12,          # El modelo parsimonioso base
  loo_notas12_sexo, 
  loo_notas12_testudio, 
  loo_notas12_qsuperior, 
  loo_notas12_ausencias
)
print(comparacion_forward)

# NOS QUEDAMOS CON EL MODELO CON NOTAS1, NOTAS2, ya que es el más parsimonioso 

# PRIOR PREDICTIVE CHECK PARA EL MODELO CON NOTAS 1 Y NOTAS 2

set.seed(1211)
s <- 100 # Cantidad de iteraciones/réplicas requeridas
N_obs <- nrow(datos_std)

# Extraer la matriz de diseño del Modelo 8 (4 predictores)
K_vars <- ncol(X_notas12)

# Simular los vectores de hiperparámetros a priori globales (las muestras "_rvs")
beta0_rvs <- rnorm(s, mean = 5, sd = 1.7)
sigma_rvs <- abs(rnorm(s, mean = 0, sd = 1))
beta_rvs_matrix <- matrix(rnorm(s * K_vars, mean = 0, sd = 2.5), nrow = s, ncol = K_vars)


# --- 1. Simulación para una sola réplica (Tu primera estructura) ---
# Multiplicación matricial %*% para combinar las 4 pendientes con los datos de los alumnos
mu_unica <- beta0_rvs[1] + as.vector(X_notas12 %*% beta_rvs_matrix[1, ])
y_pp <- rnorm(N_obs, mean = mu_unica, sd = sigma_rvs[1])

# Gráfico aislado de densidad para esa única réplica
ggplot(data.frame(x = y_pp)) + 
  geom_density(aes(x = x), color = "darkred", linewidth = 1) +
  labs(title = "Densidad A Priori - Una Sola Réplica (Modelo 8)", x = "Nota Simulada", y = "Densidad") +
  theme_bw()


# --- 2. Simulación matricial iterativa (Tu segunda estructura con el bucle for) ---
# Inicializamos la matriz vacía con s filas y N_obs columnas
y_pp_matrix <- matrix(0, nrow = s, ncol = N_obs)

# El for recorre las FILAS y devuelve la base de datos sintética completa en cada iteración
for(j in seq_len(s)){
  # Componente sistemático matricial para la iteración j
  mu_j <- beta0_rvs[j] + as.vector(X_notas12 %*% beta_rvs_matrix[j, ])
  
  y_pp_matrix[j, ] <- rnorm(N_obs, 
                            mean = mu_j, 
                            sd = sigma_rvs[j])
}


# --- 3. Construcción del Gráfico Colectivo (Overlay puro sin datos reales) ---
# Transponemos la matriz para que cada columna sea una réplica y pivotamos a formato largo
df_overlay_m12 <- as.data.frame(t(y_pp_matrix)) %>%
  mutate(obs_id = 1:n()) %>%
  pivot_longer(
    cols = starts_with("V"), 
    names_to = "replica", 
    values_to = "nota_sim"
  )

# Renderizado final con las s curvas superpuestas
ggplot(df_overlay_m12, aes(x = nota_sim, group = replica)) +
  geom_density(color = "#491211", alpha = 0.25, linewidth = 0.4) + # Color celeste de tu script
  labs(
    x = "Nota Final a Priori",
    y = "Densidad"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12)
  )

# Visualización de bandas de regresión a priori solo para el modelo con notas 1 y 2

set.seed(1211)
n_sim <- 1000
N_obs <- nrow(datos_std)

# 1. Recuperar la matriz de diseño del Modelo 8 (sexo + tiempo_estudio + quiere_superior + ausencias)
K_vars <- ncol(X_notas12)

# Matriz para almacenar las predicciones a priori (filas: simulaciones, columnas: estudiantes)
y_prior_m12 <- matrix(nrow = n_sim, ncol = N_obs)

# 2. Simular parámetros bajo la distribución a priori vágamente informativa definida
beta0_sim <- rnorm(n_sim, mean = 5, sd = 1.7)    
sigma_sim <- abs(rnorm(n_sim, mean = 0, sd = 1)) 
beta_sim_matrix <- matrix(rnorm(n_sim * K_vars, mean = 0, sd = 2.5), nrow = n_sim, ncol = K_vars)

# 3. Generar la distribución predictiva a priori conjunta
for (s in 1:n_sim) {
  mu_i <- beta0_sim[s] + as.vector(X_notas12 %*% beta_sim_matrix[s, ])
  y_prior_m12[s, ] <- rnorm(N_obs, mean = mu_i, sd = sigma_sim[s])
}

# 4. Calcular los intervalos del 50% y 95% para cada estudiante a lo largo de las observaciones
# Usamos el índice de la observación (1:N_obs) como nuestro eje X estructural
df_y_prior_50_ci <- apply(y_prior_m12, 2, quantile, probs = c(0.25, 0.75)) |>
  t() |>
  as.data.frame() |>
  mutate(x = 1:N_obs) |>
  rename(li = `25%`, ls = `75%`)

df_y_prior_95_ci <- apply(y_prior_m12, 2, quantile, probs = c(0.025, 0.975)) |>
  t() |>
  as.data.frame() |>
  mutate(x = 1:N_obs) |>
  rename(li = `2.5%`, ls = `97.5%`)

# 5. Construcción del gráfico sin observaciones reales (Únicamente la dispersión teórica a priori)
ggplot() +
  # Banda exterior de mayor cobertura (95%)
  geom_ribbon(
    data = df_y_prior_95_ci,
    aes(x = x, ymin = li, ymax = ls, fill = "Intervalo 95%"),
    alpha = 0.25
  ) +
  # Banda interior (50%)
  geom_ribbon(
    data = df_y_prior_50_ci,
    aes(x = x, ymin = li, ymax = ls, fill = "Intervalo 50%"),
    alpha = 0.45
  ) +
  # Configuración estética consistente con el trabajo
  labs(
    x = "Alumnos Seleccionados",
    y = "Calificación Final a Priori",
    fill = "Bandas A Priori"
  ) +
  scale_fill_manual(values = c("Intervalo 50%" = "#491211", "Intervalo 95%" = "#C6B39A")) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  ) + ylim(-40,60)

# INTERVALOS DE CREDIBILIDAD PREDICTIVOS A POSTERIORI PARA EL MODELO CON NOTAS 1 Y 2

# Forzamos a bayesplot a regresar a su esquema de colores por defecto
color_scheme_set("blue")

set.seed(1211) # Para que la selección de los 60 estudiantes sea reproducible

# 1. Extraer la matriz y_rep del modelo ajustado
y_rep_m12 <- rstan::extract(modelo_notas12_fit, "y_rep")$y_rep

# 2. Seleccionar exactamente 60 índices usando el dataframe que tenga los datos finales
n_mostrar <- 60
indices_60 <- sample(1:nrow(datos_std), size = n_mostrar)

# 3. Filtrar el vector real 'y' y las columnas de la matriz 'y_rep' para esos 60 casos
y_subgrupo_60 <- datos_std$nota3[indices_60]
y_rep_subgrupo_60 <- y_rep_m12[, indices_60]

# 4. Graficar pasando ÚNICAMENTE las variables filtradas del subgrupo
ppc_intervals(
  y = y_subgrupo_60,          
  yrep = y_rep_subgrupo_60,   
  prob = 0.5,       
  prob_outer = 0.95 
) +
  labs(
    caption = "Intervalos de Credibilidad Predictivos A Posteriori - Modelo con notas 1 y 2",
    x = "Alumnos Seleccionados",
    y = "Calificación Final a Posteriori"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 8)
  )

#POSTERIOR PREDICTIVE CHECK PARA EL MODELO CON NOTAS 1 Y 2 

ppc_dens_overlay(datos_std$nota3, y_rep_m12[1:50,])

# • ¿Las notas previas explican casi toda la variación relevante o siguen apareciendo otros factores?

# Al ser el modelo mas parsimonioso y ademas tener un mayor elpd podemos concluir que no necesita de otras 
# variables para explicar mejor la respuesta

#POSTERIOR MARGINALES PARA LOS PARÁMETROS DEL MODELO CON NOTAS 1 Y 2

################################################################################
#      DISTRIBUCIONES MARGINALES A POSTERIORI (MODELO CON NOTAS 1 Y 2) - SOLUCIÓN DEFINITIVA
################################################################################

library(ggplot2)
library(tidyverse)

# 1. Extraer las muestras del posterior de forma limpia
muestras_rstan <- rstan::extract(modelo_notas12_fit, c("beta0", "beta", "sigma"))

# 2. Reconstruir el dataframe indexando numéricamente las columnas de la matriz beta
df_draws_12 <- data.frame(
  `beta0 (Intercepto)`          = muestras_rstan$beta0,
  `beta1 (nota1)`               = muestras_rstan$beta[, 1],
  `beta2 (nota2)`               = muestras_rstan$beta[, 2],
  `sigma (Error)`               = muestras_rstan$sigma,
  check.names = FALSE # Evita que R reemplace los espacios por puntos en los nombres
)

# Eliminar cualquier fila incompleta por seguridad
df_draws_12 <- na.omit(df_draws_12)

# 3. Pivotar a formato largo usando exactamente "value" (como el qmd original)
df_draws_long_12 <- df_draws_12 %>%
  tidyr::pivot_longer(
    cols = everything(), 
    names_to = "parametro", 
    values_to = "value" # <--- CLAVE: Genera la columna 'value' que busca ggplot
  )

# Color naranja claro e identidades estéticas del script guía
c_orange_light <- "#491211"

# 4. Construcción del Gráfico de Histogramas (Alineado con el qmd de Australia)
ggplot(df_draws_long_12) +
  geom_histogram(
    aes(x = value, y = after_stat(density)), # <--- Ahora 'value' sí existe
    bins = 40,
    fill = c_orange_light,
    color = "white",
    alpha = 0.8
  ) +
  facet_wrap(~ parametro, scales = "free", nrow = 2) + 
  labs(
    caption = "Distribuciones Marginales A Posteriori - Modelo con notas1 y 2",
    x = "Valor",
    y = "Densidad"
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "#C6B39A"), 
    strip.text = element_text(color = "white", face = "bold")
  )


# INTERPRETACIÓN DE LOS PARÁMETROS:

#μ_i = β_0 
#          + β_1 * (nota1_i)
#          + β_2 * (nota2_i)

# β_0: es la nota final cuando las notas 1 y 2 son ambas cero
# β_1: es el cambio en la nota final cuando la nota 1 aumenta en un punto, manteniendo constante la nota 2
# β_2: es el cambio en la nota final cuando la nota 2 aumenta en un punto, manteniendo constante la nota 1


#### Modelo de Regresión Logística ####

# Modelo para predecir la probabilidad de que un estudiante apruebe el curso.
# Se define una nueva variable binaria:
#  aprobado = { 1 si nota3 ≥ 5
#               0 si nota3 < 5

# En este caso, la variable respuesta ya no es continua sino binaria.

# Creación de la variable respuesta binaria (1 si aprobó con nota >= 5, 0 si no)
datos_logistica <- datos %>% 
  mutate(aprobado = ifelse(nota3 >= 5, 1, 0))

# Ver cuántos alumnos aprobaron y cuántos no en la muestra real
table(datos_logistica$aprobado)
prop.table(table(datos_logistica$aprobado))

# Para cada sexo

prop.table(table(datos_logistica$sexo, datos_logistica$aprobado), margin = 1)

# Para cada escuela

prop.table(table(datos_logistica$escuela, datos_logistica$aprobado), margin = 1)

# Para si tiene pareja

prop.table(table(datos_logistica$pareja, datos_logistica$aprobado), margin = 1)

# Para cada domicilio

prop.table(table(datos_logistica$domicilio, datos_logistica$aprobado), margin = 1)

# Para apoyo escolar ----> hay diferencias

prop.table(table(datos_logistica$apoyo_escolar, datos_logistica$aprobado), margin = 1)

# Para apoyo familiar

prop.table(table(datos_logistica$apoyo_familiar, datos_logistica$aprobado), margin = 1)


# --- ESTRATEGIA: Ajustamos el modelo con las mejores variables predictoras (Nota 1 y Nota 2) ---

# MODELO LOGÍSTICO CON NOTAS1 Y NOTAS2

# Matriz de diseño para las notas previas
X_logistica <- model.matrix(aprobado ~ nota1 + nota2, data = datos_logistica)[, -1, drop = FALSE]
nombres_betas_log <- colnames(X_logistica)

# Lista de datos para Stan
stan_data_logistica <- list(
  N        = nrow(datos_logistica),
  K        = ncol(X_logistica),
  X        = X_logistica,
  aprobado = datos_logistica$aprobado
)

# Ajuste del modelo logístico en Stan
# NOTA: crear un archivo "modelo_logistica.stan" con verosimilitud bernoulli_logit
modelo_logistico_fit <- sampling(
  object  = stan_model("modelo_logistica.stan"), 
  data    = stan_data_logistica,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)

# Resumen de los coeficientes (en escala log-odds)
summary_log <- summary(modelo_logistico_fit, pars = c("beta0", "beta"))$summary
cat("\n--- ESTIMACIONES POSTERIORI: REGRESIÓN LOGÍSTICA (LOG-ODDS) ---\n")
print(round(summary_log[, c("mean", "sd", "2.5%", "97.5%", "Rhat")], 4))

# INTERVALOS DE CREDIBILIDAD PARA LA PROBABILIDAD DE APROBAR DADA LA NOTA1

# 1. Crear la grilla de predicción pura para la Nota 1
df_prediccion_solo_notas <- tidyr::expand_grid(
  nota1 = seq(from = min(datos_std$nota1), to = max(datos_std$nota1), length.out = 100),
  nota2 = 0 # Fijamos la nota 2 en su promedio
)

# 2. Extraer 50 draws de los parámetros globales de su modelo sin apoyo
# Nota: Asegúrense de usar el objeto de ajuste correcto de Stan para este modelo (ej. modelo_notas12_fit)
df_draws_global_50 <- modelo_notas12_fit |> 
  tidybayes::spread_draws(beta0, beta[predictor_idx], ndraws = 50) |> 
  # Pasamos de formato largo a ancho para tener beta1 y beta2 en columnas separadas
  tidyr::pivot_wider(names_from = predictor_idx, names_prefix = "beta", values_from = beta)

# 3. Calcular el eta lineal uniendo la grilla con los draws
df_lines_solo_n1 <- df_prediccion_solo_notas |> 
  dplyr::cross_join(df_draws_global_50) |> 
  dplyr::mutate(
    # Ecuación global: beta1 es el coeficiente de nota1 y beta2 el de nota2
    eta = beta0 + beta1 * nota1 + beta2 * nota2
  )

# 4. Graficar las bandas con el truco del factor para el alpha
df_lines_solo_n1 |>
  ggplot(aes(x = nota1, y = plogis(eta))) +
  tidybayes::stat_lineribbon(
    aes(alpha = as.factor(after_stat(.width))), 
    fill = "#C6B39A", 
    color = "grey20", 
    linewidth = 0.5
  ) +
  scale_alpha_manual(values = c("0.5" = 0.8, "0.8" = 0.5, "0.95" = 0.2), guide = "none") +
  labs(
    x = "Nota 1 (Estandarizada)", 
    y = "P(Aprobado = 1)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 12))

# INTERVALOS DE CREDIBILIDAD PARA LA PROBABILIDAD DE APROBAR DADA LA NOTA2

# 1. Crear la grilla invirtiendo los roles
df_prediccion_solo_notas2 <- tidyr::expand_grid(
  nota1 = 0, # Fijamos la nota 1 en su promedio
  nota2 = seq(from = min(datos_std$nota2), to = max(datos_std$nota2), length.out = 100)
)

# 2. Calcular el eta lineal para la Nota 2 usando los mismos draws de arriba
df_lines_solo_n2 <- df_prediccion_solo_notas2 |> 
  dplyr::cross_join(df_draws_global_50) |> 
  dplyr::mutate(
    eta = beta0 + beta1 * nota1 + beta2 * nota2
  )

# 3. Graficar las bandas para la Nota 2
df_lines_solo_n2 |>
  ggplot(aes(x = nota2, y = plogis(eta))) +
  tidybayes::stat_lineribbon(
    aes(alpha = as.factor(after_stat(.width))), 
    fill = "#C6B39A", 
    color = "grey20", 
    linewidth = 0.5
  ) +
  scale_alpha_manual(values = c("0.5" = 0.8, "0.8" = 0.5, "0.95" = 0.2), guide = "none") +
  labs(
    x = "Nota 2 (Estandarizada)", 
    y = "P(Aprobado = 1)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 12))

# MODELO LOGÍSTICO CON NOTAS1 NOTAS2 Y APOYO ESCOLAR

# Matriz de diseño (ya incluye nota1, nota2 y la dummy de apoyo_escolar)
X_logistica_cat <- model.matrix(aprobado ~ nota1 + nota2 + apoyo_escolar, data = datos_logistica)[, -1, drop = FALSE]
nombres_betas_log_cat <- colnames(X_logistica_cat)

stan_data_logistica_cat <- list(
  N                 = nrow(datos_logistica),
  aprobado          = datos_logistica$aprobado,
  # as.numeric de un factor con 2 niveles crea un vector de 1s y 2s
  apoyo_escolar_idx = as.numeric(datos_logistica$apoyo_escolar), 
  nota1             = datos_logistica$nota1,
  nota2             = datos_logistica$nota2
)

# 3. Muestrear el modelo
modelo_logistico_cat_fit <- sampling(
  object  = stan_model("modelo_log_cat.stan"), 
  data    = stan_data_logistica_cat,
  chains  = 4,
  iter    = 2000,                            
  seed    = 1211, 
  refresh = 500
)


# Resumen de los coeficientes (en escala log-odds)
summary_log_cat <- summary(modelo_logistico_cat_fit, pars = c("a", "b1", "b2"))$summary
cat("\n--- ESTIMACIONES POSTERIORI: REGRESIÓN LOGÍSTICA (LOG-ODDS) ---\n")
print(summary_log_cat[, c("mean", "sd", "2.5%", "97.5%", "Rhat")])

library(tidybayes)

# CADENAS DE MARKOV CHAIN MONTE CARLO TRACE PLOT
# 1. Extraer las muestras de forma limpia usando tidybayes
df_draws <- spread_draws(modelo_logistico_cat_fit, a[apoyo_escolar], b1[apoyo_escolar], b2[apoyo_escolar])
head(df_draws)

# 2. Pivotar especificando el nombre de la columna destino ("variable_base")
df_draws_long <- df_draws |> 
  tidyr::pivot_longer(
    cols = c("a", "b1", "b2"),
    names_to = "variable_base", # <-- CLAVE: Así "variable_base" sí existe abajo
    values_to = "value"
  )

# 3. Mutar y etiquetar de forma consistente con los índices 1 y 2
df_draws_long <- df_draws_long |>
  dplyr::mutate(
    parametro = case_when(
      variable_base == "a"  ~ "Intercepto",
      variable_base == "b1" ~ "Pendiente 1 - nota 1",
      variable_base == "b2" ~ "Pendiente 2 - nota 2"
    ),
    apoyo_escolar_label = case_when(
      apoyo_escolar == 1 ~ "No recibe apoyo", # <-- CORREGIDO: índice 1 de R
      apoyo_escolar == 2 ~ "Recibe apoyo"     # <-- CORREGIDO: índice 2 de R
    )
  )

head(df_draws_long)

ggplot(df_draws_long, aes(x = .iteration, y = value, color = as.factor(.chain))) +
  geom_line(alpha = 0.6) +
  # Separa un gráfico por cada parámetro y condición de apoyo escolar
  facet_grid(parametro ~ apoyo_escolar_label, scales = "free_y") +
  scale_color_viridis_d(name = "Cadena") +
  labs(
    title = "Trace Plots de las Cadenas MCMC",
    x = "Iteración",
    y = "Valor del parámetro"
  ) +
  theme_minimal()


df_draws_long |>
  ggplot() +
  # Usamos un color directo o el vector de su paleta (ej. c_orange_light o colores[3])
  geom_histogram(aes(x = value), bins = 50, fill = "#491211", color = "white", alpha = 0.8) +
  labs(
    x = "Valor del parámetro (Escala Log-Odds)", 
    y = "Densidad",
    caption = "Distribuciones Marginales A Posteriori por condición de Apoyo Escolar"
  ) +
  # CORREGIDO: Cambiamos 'partido' por 'apoyo_escolar_label'
  facet_grid(
    vars(apoyo_escolar_label), 
    vars(parametro), 
    labeller = labeller(apoyo_escolar_label = label_value, parametro = label_value), 
    scales = "free_x"
  ) +
  theme_bw() +
  theme(
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank(),
    strip.background = element_rect(fill = "#C6B39A"), 
    strip.text = element_text(color = "white", face = "bold")
  )

library(rstantools)

# 1. Crear la grilla de predicción basada en sus datos estandarizados reales
df_prediccion <- tidyr::expand_grid(
  # Hacemos variar nota1 desde su mínimo a su máximo real estandarizado
  nota1 = seq(from = min(datos_std$nota1), to = max(datos_std$nota1), length.out = 100),
  # Dejamos nota2 fija en 0 (que es su media por estar estandarizada)
  nota2 = 0,
  # Índices numéricos que entiende su modelo de Stan: 1 = No, 2 = Sí
  apoyo_escolar = c(1, 2)
)

# 2. Extraer los 50 draws aleatorios de los parámetros desde su modelo de Stan
df_draws_50 <- modelo_logistico_cat_fit |> 
  tidybayes::spread_draws(a[apoyo_escolar], b1[apoyo_escolar], b2[apoyo_escolar], ndraws = 50)

# 3. Construir df_lines combinando la grilla con los draws y calculando eta manualmente
df_lines <- df_prediccion |> 
  dplyr::inner_join(df_draws_50, by = "apoyo_escolar", relationship = "many-to-many") |> 
  dplyr::mutate(
    # Calculamos la ecuación del logit que definieron en su archivo .stan
    eta = a + b1 * nota1 + b2 * nota2,
    
    # Traducimos los índices a etiquetas claras para el gráfico
    apoyo_label = ifelse(apoyo_escolar == 1, "No recibe apoyo", "Recibe apoyo")
  )

# 4. Generar el Spaghetti Plot con la paleta de colores de su equipo
df_lines |>
  ggplot() +
  geom_line(
    aes(
      x = nota1, 
      y = plogis(eta), # plogis() es la función de R base equivalente a expit
      group = interaction(.draw, apoyo_label), 
      color = apoyo_label
    ), 
    alpha = 0.4,       # Transparencia fundamental para ver la incertidumbre
    linewidth = 0.6
  ) +
  # Usamos la paleta verde azulado y naranja que vienen manejando
  scale_color_manual(values = c("No recibe apoyo" = "#2a9d8f", "Recibe apoyo" = "#f4a261")) +
  labs(
    title = "Evolución de la Probabilidad de Aprobar",
    subtitle = "Efecto marginal de Nota 1 (Nota 2 fijada en su promedio)",
    x = "Nota 1", 
    y = "P(Aprobado = 1)",
    color = "Condición de Apoyo Escolar"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12)
  )

# 1. Crear la grilla invirtiendo las variables
df_prediccion_nota2 <- tidyr::expand_grid(
  # Ahora nota1 se queda fija en su promedio (0 por estar estandarizada)
  nota1 = 0,
  # Hacemos variar nota2 desde su mínimo a su máximo real estandarizado
  nota2 = seq(from = min(datos_std$nota2), to = max(datos_std$nota2), length.out = 100),
  # Índices numéricos para Stan: 1 = No, 2 = Sí
  apoyo_escolar = c(1, 2)
)

# 2. Extraer los mismos 50 draws aleatorios (o pueden usar df_draws_50 si ya lo ejecutaron arriba)
df_draws_50_n2 <- modelo_logistico_cat_fit |> 
  tidybayes::spread_draws(a[apoyo_escolar], b1[apoyo_escolar], b2[apoyo_escolar], ndraws = 50)

# 3. Construir df_lines para la Nota 2
df_lines_nota2 <- df_prediccion_nota2 |> 
  dplyr::inner_join(df_draws_50_n2, by = "apoyo_escolar", relationship = "many-to-many") |> 
  dplyr::mutate(
    # La ecuación matemática del modelo sigue siendo la misma
    eta = a + b1 * nota1 + b2 * nota2,
    
    # Etiquetas para el gráfico
    apoyo_label = ifelse(apoyo_escolar == 1, "No recibe apoyo", "Recibe apoyo")
  )

# 4. Generar el Spaghetti Plot para la Nota 2
df_lines_nota2 |>
  ggplot() +
  geom_line(
    aes(
      x = nota2, # <-- CLAVE: Ahora nota2 va en el eje X
      y = plogis(eta), 
      group = interaction(.draw, apoyo_label), 
      color = apoyo_label
    ), 
    alpha = 0.4, 
    linewidth = 0.6
  ) +
  scale_color_manual(values = c("No recibe apoyo" = "#2a9d8f", "Recibe apoyo" = "#f4a261")) +
  labs(
    title = "Evolución de la Probabilidad de Aprobar",
    subtitle = "Efecto marginal de Nota 2 (Nota 1 fijada en su promedio)",
    x = "Nota 2", 
    y = "P(Aprobado = 1)",
    color = "Condición de Apoyo Escolar"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12)
  )

################################################################################
#  1. PRUEBAS PREDICTIVAS A PRIORI: PROPORCIÓN ESPERADA DE APROBADOS
################################################################################

# PRUEBAS PREDICTIVAS A PRIORI 

#install.packages("rstanarm") # Solo la primera vez
library(rstanarm)

modelo_logit_priori <- stan_glm(
  aprobado ~ nota1 + nota2 + apoyo_escolar, # <-- Descomentado y con tus variables del TP
  data = datos_logistica,                  # <-- Usamos el dataset que tiene "aprobado"
  family = binomial, 
  prior_intercept = normal(0, 10),
  prior = normal(0, 5),
  chains = 4,
  iter = 5000 * 2,
  seed = 84735,
  prior_PD = TRUE
)

#Proporción esperada de aprobados a priori 

datos_logistica %>%
  add_predicted_draws(modelo_logit_priori, ndraws = 100) %>%
  group_by(.draw) %>%                                     # <- Corregido: .draw con punto
  summarize(proporcion_aprobados = mean(.prediction == 1)) %>% # <- Corregido: .prediction y "=="
  ggplot(aes(x = proporcion_aprobados)) +                 # <- Corregido el espacio en ggplot
  geom_histogram(
    color = "white",
    fill = "#C6B39A",                                     # <- Reemplazá por paleta[3] si la tenés cargada
    bins = 10
  ) +
  labs(                                                   # <- Corregido: labs en minúscula y 'y' en minúscula
    x = "Proporción de aprobados a Priori", 
    y = "Frecuencia"
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_minimal()

mean(datos_logistica$aprobado) #0.7422969

################################################################################
#  1. PRUEBAS PREDICTIVAS A POSTERIORI: PROPORCIÓN ESPERADA DE APROBADOS
################################################################################

# PRUEBAS PREDICTIVAS A POSTERIORI 

#install.packages("rstanarm") # Solo la primera vez

modelo_logit_posteriori <- stan_glm(
  aprobado ~ nota1 + nota2 + apoyo_escolar, # <-- Descomentado y con tus variables del TP
  data = datos_logistica,                  # <-- Usamos el dataset que tiene "aprobado"
  family = binomial, 
  prior_intercept = normal(0, 10),
  prior = normal(0, 5),
  chains = 4,
  iter = 5000 * 2,
  seed = 84735,
)

#Proporción esperada de aprobados a posteriori 

datos_logistica %>%
  add_predicted_draws(modelo_logit_posteriori, ndraws = 100) %>%
  group_by(.draw) %>%                                     # <- Corregido: .draw con punto
  summarize(proporcion_aprobados = mean(.prediction == 1)) %>% # <- Corregido: .prediction y "=="
  ggplot(aes(x = proporcion_aprobados)) +                 # <- Corregido el espacio en ggplot
  geom_histogram(
    color = "white",
    fill = "#C6B39A",                                     # <- Reemplazá por paleta[3] si la tenés cargada
    bins = 10
  ) +
  labs(                                                   # <- Corregido: labs en minúscula y 'y' en minúscula
    x = "Proporción de aprobados a predictivas a Posteriori", 
    y = "Frecuencia"
  ) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_minimal()


# Se compara este análisis con el modelo para la nota final: 
# ¿Los factores asociados a obtener una mayor nota son los mismos que los asociados a aprobar el curso?

# se agregó la variable apoyo escolar dado que existen diferencias en las probabilidades de aprobar dado que recibió apoyo

# ¿Cambia la interpretación de los coeficientes? 

# --- TRANSFORMAR A ODDS RATIO (CHANCES) PARA EL MODELO CON APOYO ESCOLAR ---

# 1. Extraer las muestras de las pendientes (b1 y b2) desde el modelo indexado
muestras_b1 <- rstan::extract(modelo_logistico_cat_fit, "b1")$b1  # Matriz de iteraciones x 2 grupos
muestras_b2 <- rstan::extract(modelo_logistico_cat_fit, "b2")$b2  # Matriz de iteraciones x 2 grupos

# 2. Calcular las medias a posteriori de cada parámetro por grupo
media_b1_no_apoyo <- mean(muestras_b1[, 1])
media_b1_si_apoyo <- mean(muestras_b1[, 2])

media_b2_no_apoyo <- mean(muestras_b2[, 1])
media_b2_si_apoyo <- mean(muestras_b2[, 2])

# 3. Transformar a la escala de Odds Ratios usando la función exponencial exp()
or_resultados <- c(
  "Nota 1 (No recibe apoyo)" = exp(media_b1_no_apoyo),
  "Nota 1 (Recibe apoyo)"    = exp(media_b1_si_apoyo),
  "Nota 2 (No recibe apoyo)" = exp(media_b2_no_apoyo),
  "Nota 2 (Recibe apoyo)"    = exp(media_b2_si_apoyo)
)

cat("\n--- ODDS RATIOS MEDIOS POSTERIORI (MODELO INDEXADO) ---\n")
print(round(or_resultados, 4))

# θ_10: Para aquellos alumnos que no recibieron apoyo escolar, la chance de aprobar aumenta 
# un 171% cuando la nota 1 aumenta en una unidad

# θ_11: Para aquellos alumnos que recibieron apoyo escolar, la chance de aprobar disminuye 
# un 3% cuando la nota 1 aumenta en una unidad

# θ_20: Para aquellos alumnos que no recibieron apoyo escolar, la chance de aprobar aumenta 
# un 13127% cuando la nota 2 aumenta en una unidad

# θ_21: Para aquellos alumnos que recibieron apoyo escolar, la chance de aprobar aumenta 
# un 2151% cuando la nota 2 aumenta en una unidad

# ¿Qué ventajas y limitaciones tiene transformar la nota final en una variable de aprobación o desaprobación?

# VENTAJAS ✌
# se puede interpretar en términos de probabilidades
# interpretabilidad
# foco en el umbral crítico: Mide el éxito o fracaso en el punto exacto donde cambia la condición jurídica del estudiante en la facultad (el corte de la nota ≥5). 

# LIMITACIONES 😩
# Pérdida Masiva de Información (Variabilidad): Al binarizar la nota, están tratando exactamente igual a un alumno que sacó un 0  
# que a uno que obtuvo un 4.5 (estuvo a un paso de aprobar). Al mismo tiempo, el modelo considera idénticos a un alumno que aprobó raspando con 5 
# que a uno brillante que sacó un 10. Toda esa variabilidad interna de los datos se destruye.
# Sensibilidad al Punto de Corte: El modelo se vuelve totalmente dependiente del umbral elegido de forma arbitraria (en este caso, 5).

# "Transformar la nota final en una variable cualitativa de aprobación es ventajoso para la gestión educativa y la comunicación de resultados, 
# pero metodológicamente costoso, ya que penaliza la riqueza de los datos, oculta los matices del rendimiento estudiantil y aumenta la incertidumbre epistémica de las estimaciones analizadas en el posterior."
