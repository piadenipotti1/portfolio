# ==============================================================================
# Sesgo algorítmico: ¿el sistema "Centinela Delta" discrimina a una hinchada?
# Estadística Bayesiana · Lic. en Estadística (FCEyE, UNR) · 2026 · Trabajo en equipo
# Ejecutar desde la carpeta codigo/
# ==============================================================================
#-------------------------------------------------------------------------------
#                                   TP                                          
#-------------------------------------------------------------------------------

# Librerias --------------------------------------------------------------------

library(dplyr)
library(ggplot2)


# Datos ------------------------------------------------------------------------

datos <- read.csv("../datos/hinchas.csv", encoding = "UTF-8")
datos$G <- ifelse(datos$G == "Albatros", "Albatros", "Buhos")  # normaliza el acento de "Búhos"  


# Un sistema de IA, Centinela Delta, se encarga de gestionar el derecho
# de admision en los estadios: Templo de la Almeja (localia de Albatros) y 
# Frontin de la Bruma (localia de Buhos)

# A cada persona que quiere entrar, CD asigna un SCORE de riesgo S ε (0,100), y
# se considera a S como la probabilidad estimada de que la persona cometa un incidente

# REGLA DE DECISION CD: (T es un umbral fijo)
#     S  > T ==> se prohibe el ingreso ==> prediccion: "violento"
#     S <= T ==> se permite el ingreso ==> prediccion: "pacifico"

# La empresa dice que su score esta perfectamente calibrado

# Meses despues de su imprementacion, Gradas de paz presenta la siguiente denuncia:
# "A los hinchas pacíficos (inocentes) de los Búhos les prohíben entrar más seguido 
# que a los pacíficos del Albatros."


# Analizamos los datos de la temporada anterior, en donde se registro:
#       Y = 1 si participo en incidentes.
#         = 0 si no participo en incidentes.
#       score: puntje donde se entreno el sistema CD



# ------------------------------------------------------------------------------
#                                  PRIMERA PARTE                                
# ------------------------------------------------------------------------------

# 1) Cual es la prevalencia de hinchas violentos en cada equipo? (proporcion)

prevalencia_equipos <- datos %>%
  group_by(G) %>%
  summarise(
    total_hinchas = n(),             # Contamos cuántos hinchas hay en total por equipo
    casos_violentos = sum(Y == 1),   # Contamos cuántos tienen Y = 1
    prevalencia = mean(Y == 1)       # La media nos da directamente la proporción
  )

print(prevalencia_equipos)

# Prevalencia Albatros: 26%
# Prevalencia Buhos:    58%

# ------------------------------------------------------------------------------
# 2) Que sesgos puede presentar el conjunto de datos retrospectivo entregado?

# (-) En nuestro conjunto de datos, la variable Y significa que el hincha fue violento
#     y hubo un policia presente que registro el incidente. Como bien destaca la
#     ONG Gradas de Paz, el estadio de los Buhos contaba con mayor presencia policial,
#     lo que implica que, mientras un incidente de un hincha de Buhos tiene altisimas
#     probabilidades de quedar registrado, uno cometidos por un hincha de Albatros
#     tiene mas chances de pasar desapercibido y no dejar registros. Por lo tanto,
#     es posible que la diferencia de prevalencias en ambos equipos no necesariamente
#     refleje una diferencia real en la violencia de los grupos, sino una asimetria
#     en la vigilancia que afecta a los datos.


# (-) Lo mencionado anteriormente resulta en otro sesgo posible. Al haber entrenado
#     la IA con los datos historicos donde los Buhos estan sobre representados en
#     la variable de incidentes, el sistema aprendera a asociar a esta hinchada con
#     un mayor riesgo, asignandoles scores mas altos en la actualidad. Esto puede
#     provocar que la policia y los controles del estadio vigieln aun mas de cerca
#     a los Buhos. De esta manera, la IA terminara autoconfirmando y amplificando
#     la discriminacion inicial en un circulo vicioso


# ------------------------------------------------------------------------------
# 3) Hacer un Calibration Plot para verificar que los scores esten calibrados


datos_calibracion <- datos %>%
  mutate(prob_predicha = Score / 100) %>%    # Pasamos el score a probabilidad (0 a 1)
  mutate(intervalo = cut(prob_predicha, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%   # Cortamos los datos en 10 intervalos (0-0.1, 0.1-0.2, etc.)
  group_by(intervalo) %>%             # Agrupamos por intervalo
  summarise(
    media_pred = mean(prob_predicha), # Promedio de lo que predijo el modelo
    media_obs = mean(Y),              # Porcentaje REAL de violentos (Y=1)
    cantidad = n()                    # Cantidad de hinchas en este balde (opcional)
  ) %>%
  filter(!is.na(media_pred))          # Limpiamos por si quedó algún balde vacío



# Grafico 1, la abscisa de cada punto es la probabilidad predicha media del grupo

ggplot(datos_calibracion, aes(x = media_pred, y = media_obs)) +     # !! x = media_prob !!
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 1) + # Línea de calibración perfecta
  geom_line(color = "darkred", linewidth = 1) +                           
  geom_point(aes(size = cantidad), color = "darkred", alpha = 0.8) +      
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  theme_minimal() +
  labs(
    title = "Gráfico de Calibración Global: Centinela Delta",
    subtitle = "Los puntos cercanos a la diagonal indican una buena calibración",
    x = "Probabilidad Predicha Promedio",
    y = "Proporción Observada Real (Violentos)",
    size = "Cantidad de Hinchas"
  )



# Grafico 2, la abscisa de cada punto es la etiqueta "intervalo"

ggplot(datos_calibracion, aes(x = intervalo, y = media_obs)) +     # !! x = intervalo !!
  geom_abline(intercept = -0.05, slope = 0.1, linetype = "dashed", color = "gray50") + # cambio el intercepto y la pendiente
  geom_line(aes(group = 1),color = "darkred", linewidth = 1) +     # 'group = 1' para que tome todo como un = grupo y una los puntos
  geom_point(aes(size = cantidad), color = "darkred", alpha = 0.8) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +  # !! saco scale_x_continuous !!
  theme_minimal() +
  labs(
    title = "Gráfico de Calibración Global: Centinela Delta",
    subtitle = "Los puntos cercanos la diagonal indican una buena calibración",
    x = "Intervalos de Probabilidad Predicha por la IA (Score)",
    y = "Proporción Observada Real (Violentos)",
    size = "Cantidad de Hinchas"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))    # p/ inclinar las etiquetas y q no se superpongan



# Teniendo en cuenta que la linea punteada, y = x,  representa la calibracion perfecta, 
# cuanto mas se asemejen a la misma, mejor calibrado estara el modelo. En
# este caso podemos ver que, efectivamente la empresa Nebula Guard Analytics estaba 
# en lo cierto al afirmar que su score calculado con Centinela Delta esta muy bien calibrado.

# Sin embargo, podemos destacar que en los scores mas altos el modelo pierde un poco
# de exactitud y tiende a ser "alarmista". Esto quiere decir que, cuando el score
# toma valores entre 0.8 y 0.9 (segun IA score 80% ==> 80% de ellos son violentos),
# en realidad, luego de mirar los datos, notamos que aproximadamente un 70% de ellos
# fueron verdaderamente violentos. En consecuencia, se termina prohibiendo la entrada
# a gente que, si bien tenia score alto, en realidad eran hinchas pacificos.



# ------------------------------------------------------------------------------
# 4) considerando un umbral T = 50, ¿cuál es la accuracy del modelo?

T_umbral <- 50

# Prediccion de la IA, usando el criterio S > T   ==> VIOLENTO
#                                         S <= T  ==> PACIFICO

datos <- datos %>%
  mutate(Prediccion = ifelse(Score > T_umbral, 1, 0))


# La accuracy del modelo es la EXACTITUD, y responde a la simple pregunta: 
# "De todas las personas evaluadas por el algoritmo, en que pct acerto su prediccion?"
# Por eso es que se considera EXITOS a todas evaluaciones que coincidieron con los
# datos que se usaron (evaluaciones correctas / tota de evaluaciones)


# Calculamos la Accuracy 
accuracy <- mean(datos$Prediccion == datos$Y)

# La Accuracy del modelo con T=50 es: 0.7


# ------------------------------------------------------------------------------
# 5) Con T = 50, para cada grupo, calcule el número de falsos negativos, verdaderos negativos,
#    falsos positivos y verdaderos positivos.


# Creamos las nuevas categorias en nuestro df
datos <- datos %>%
  mutate(Falso_Positivo = ifelse(Prediccion == 1 & Y == 0, 1,0)) %>%
  mutate(Verdadero_Positivo = ifelse(Prediccion == 1 & Y == 1, 1,0)) %>%
  mutate(Falso_Negativo = ifelse(Prediccion == 0 & Y == 1, 1,0)) %>%
  mutate(Verdadero_Negativo = ifelse(Prediccion == 0 & Y == 0, 1,0)) 

# TOTALES sin despreciar por grupo (no lo pide)
sum(datos$Falso_Positivo)         # 16
sum(datos$Verdadero_Positivo)     # 28
sum(datos$Falso_Negativo)         # 14
sum(datos$Verdadero_Negativo)     # 42


# TOTALES POR GRUPO

resultados <- datos %>%
  group_by(G) %>%
  summarise(             
    Falso_Positivo = sum(Falso_Positivo),   
    Verdadero_Positivo = sum(Verdadero_Positivo),
    Falso_Negativo = sum(Falso_Negativo),
    Verdadero_Negativo = sum(Verdadero_Negativo)
  )

print(resultados)


# ------------------------------------------------------------------------------
# 6) Que representa la tasa de falsos positivos?

# FPR = FP / (FP + VN)

# Representa, especialmente en el contexto que estamos trabajando, el costo de la 
# injusticia algoritmica, es la probabilidad de qe el sistema te rechace el ingreso
# siendo vos una persona 100% pacifica. 

# Es el error que mas duele socialmente, en la justicia tradicional hay un principio
# que dice "Es preferible dejr a 10 culpables libres que meter a un inocente preso"
#
# Representa que tan propenso es el Centinela Delta a clasificar como violentos a
# hinchas pacificos.


# ------------------------------------------------------------------------------
# 7) ¿Qué supuestos son necesarios para modelar el número de falsos positivos como una
#    variable binomial?

# Que cada prediccion del Centinela Delta no afecte a las demas (INDEPENDENCIA),
# es decir que si el sistema se equivoca y le prohibe la entrada a un hincha pacifico
# no afecte en nada a la probabilidad de que se equivoque con el siguiente hincha
# en la fila.

# Que todos los hicnhas pacificos dentro de un mismo grupo (G) tienen exactamente la
# misma probabilidad de ser un Falso Positivo (PBB DE EXITO CONSTANTE P/ TODOS LOS DATOS)

# Solo hay dos resultados posibles: Falso Positivo - Verdadero Negativo (DICOTOMIA)


# ------------------------------------------------------------------------------
# 8) ¿Cuál es un prior razonable para el FPR de cada grupo? Escriba el modelo bayesiano y
# encuentre la distribución a posteriori de FPRg.

# Como inicialmente lo unico que sabemos es que la empresa Nebula Guard asegura que
# su IA Centinela Delta es un algoritmo imparcial (no favorece a ninguno) y que 
# funciona relativamente bien planteamos:

# PRIORS:  FPR_A ~ Beta (3,9)
#          FPR_B ~ Beta (3,9)

α_prior_A <- 3
ß_prior_A <- 9

α_prior_B <- α_prior_A
ß_prior_B <- ß_prior_A

# De esta, en principio, asumimos que el sistema tiene igual funcionamiento para 
# clasificar a hinchas de cualquiera de los dos equipos. Ademas de que creemos que
# funciona relativamente bien, por lo que la FPR va a ser levemente asimetrica a la
# derecha, con una media 0.25 y un desvio de 0.125 (desvio no es muy chico???)

# X: numero de falsos positivos --> X/FPR ~ Bi(FP + VN, FPR)


# VEROSIMILITUDES: x_A/FPR_A ~ Bi(n_A = 37, Θ_A = FPR_A)
#                  x_B/FPR_B ~ Bi(n_B = 21, Θ_B = FPR_B)

# x_A = FP_A = 1
# x_B = FP_B = 15

x_A <-  1
x_B <-  15

n_A <-  37
n_B <-  21


α_post_A <- x_A + α_prior_A
ß_post_A <- n_A - x_A + ß_prior_A

α_post_B <- x_B + α_prior_B
ß_post_B <- n_B - x_B + ß_prior_B

# POSTERIORS:   FPR_A/X_A ~ Beta(4, 45)
#               FPR_B/X_B ~ Beta(18, 15)

# ------------------------------------------------------------------------------
# 9) Halle la distribucion a posteriori de la diferencia entre FPR_B Y FPR_A.
#    Cual es la P(FPR_B > FPR_A)? Que se puede concluir en relacion a la denuncia
#    de la ONG Gradas de Paz?


simulaciones <- 1000000

# Simulamos 1.000.000 de escenarios posibles de las distribuciones a posteriori
FPR_A_sim <- rbeta(simulaciones, shape1 = α_post_A, shape2 = ß_post_A)   
FPR_B_sim <- rbeta(simulaciones, shape1 = α_post_B, shape2 = ß_post_B)  

# (FPR_B > FPR_A) --> (FPR_B - FPR_A)
diferencia <- FPR_B_sim - FPR_A_sim

# P(FPR_B > FPR_A) = P(FPR_B - FPR_A > 0) --> calculamos la proporcion de veces qu
#                                             la diferencia fue positiva

pbb_B_mayor_A <- mean(diferencia > 0)
print(paste("Probabilidad de que el sistema castigue más a Búhos:", pbb_B_mayor_A))

# 4. Graficamos la distribución a posteriori de la diferencia
datos_dif <- data.frame(Diferencia = diferencia)

ggplot(datos_dif, aes(x = Diferencia)) +
  geom_density(fill = "darkred", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  labs(title = "Distribución a Posteriori de la Diferencia de Falsos Positivos",
       subtitle = "FPR Búhos - FPR Albatros",
       x = "Diferencia en la Tasa de Falsos Positivos",
       y = "Densidad")

# A partir de la simulación de las distribuciones a posteriori, se concluye que la 
# probabilidad de que el sistema se equivoque más frecuentemente con los Búhos es 
# prácticamente del 100%. La distribución de la diferencia ($FPR_B - FPR_A$) se concentra 
# casi en su totalidad en valores positivos, estimando que la tasa de bloqueos injustos 
# hacia los Búhos es, en promedio, unos 50 puntos porcentuales mayor que la de los Albatros.
# Esto aporta evidencia bayesiana irrefutable a favor de la denuncia de la ONG.

# ------------------------------------------------------------------------------
# 10) Construya una grilla de valores de T_A y T_B y calcule, para cada combinacion
#     la pbb de que la diferencia entre FPR_B y FPR_A sea menor al 5%. Que valores
#     de T_A y T_B "iguala" FPR_B y FPR_A

umbrales <- seq(0, 100, by = 1)

grilla <- expand.grid(T_A = umbrales, T_B = umbrales)

prob_FPR <- function(ta, tb, datos){
  
  # Filtramos los datos para quedarnos solo con los PACIFICOS
  pacificos <- datos %>%
    filter( Y == 0)
  
  # Calcula los FP y VN para cada grupo con su propio umbral 
  metrica <- pacificos %>%
    summarise( 
      FP_A = sum(G == "Albatros" & Score > ta),
      VN_A = sum(G == "Albatros" & Score <= ta),
      FP_B = sum(G == "Buhos" & Score > tb),
      VN_B = sum(G == "Buhos" & Score <= tb)
    )
  
  # Simulo posteriors con las nuevas metricas
  
  N_sim <- 1000
  
  post_A <- rbeta(N_sim, 3 + metrica$FP_A, 9 + metrica$VN_A)
  post_B <- rbeta(N_sim, 3 + metrica$FP_B, 9 + metrica$VN_B)
  
  # Calculo P(|FPR_B - FPR_A| < 0.05) como la proporcion de veces que se cumplio esa condicion
  
  return(mean(abs(post_B - post_A) < 0.05))
}

# Agrego a la grilla las probabilidades de que la diferencia sea menor a 0.05
grilla$prob <- mapply(prob_FPR, grilla$T_A, grilla$T_B, MoreArgs = list(datos = datos))

 # mapply() aplica la funcion a todos los valores de la grilla

# Ahora buscamos la combinacion que maximice la  probabilidad de que se obtenga un error
# del 5%, es decir que el algoritmo sea JUSTO (los FP son PARECIDOS)

which.max(grilla$prob)


data.frame(grilla)

probs_positivas <- grilla %>%
  filter(prob > 0.6)


which.max(probs_positivas$prob)

cat("La combinacion de umbrales, T_A =", probs_positivas$T_A[which.max(probs_positivas$prob)], "y T_B = ", probs_positivas$T_B[which.max(probs_positivas$prob)], 
    ", es aquella que maximiza la probabilidad de que las tasas de falsos positivos de cada grupo sean parecidas entre sí,  P(|T_B - T_A| < 0.05) = ", probs_positivas$prob[which.max(probs_positivas$prob)],".")



# ------------------------------------------------------------------------------
# 11) ¿Considera que esta modificación del mecanismo de decisión es justa?

# Usar umbrales distintos por grupo iguala las tasas de falsos positivos (los
# pacíficos de ambas hinchadas pasan a tener el mismo riesgo de ser bloqueados),
# pero implica tratar distinto a dos personas con el MISMO score según su grupo.
# Además, si la diferencia de prevalencias se debe a la mayor vigilancia policial
# sobre los Búhos (punto 2), el problema está en las etiquetas Y y el ajuste de
# umbrales solo corrige el síntoma. Es más justo en términos de error, pero no
# resuelve el sesgo de origen.

# ------------------------------------------------------------------------------
# 12) Construya las funciones f_A y f_B, convierta los scores y compare las 
#     distribuciones a posteriori de FPR_B y FPR_A.

# A) Definimos los umbrales óptimos encontrados en el punto 10
T_A_opt <- probs_positivas$T_A[which.max(probs_positivas$prob)] #51
T_B_opt <- probs_positivas$T_B[which.max(probs_positivas$prob)] #93

# B) Función generadora de la transformación lineal por tramos
# Esta función asegura que f(0)=0, f(Tg)=50 y f(100)=100
f_transformacion <- function(S, Tg) {
  ifelse(S <= Tg, 
         S * (50 / Tg),                          # Tramo 1: de 0 a Tg -> mapea a 0-50
         50 + (S - Tg) * (50 / (100 - Tg)))      # Tramo 2: de Tg a 100 -> mapea a 50-100
}

# C) Creamos la nueva columna con los scores transformados
datos <- datos %>%
  mutate(Score_Transf = ifelse(G == "Albatros", 
                               f_transformacion(Score, T_A_opt), 
                               f_transformacion(Score, T_B_opt)))

# D) Calculamos las predicciones y métricas con el nuevo Score y umbral fijo T=50
# Nota: Ahora todos se miden contra 50 porque el sesgo ya está "licuado" en el score.
datos <- datos %>%
  mutate(
    Pred_Transf = ifelse(Score_Transf > 50, 1, 0),
    FP_Transf = as.integer(Pred_Transf == 1 & Y == 0),
    VN_Transf = as.integer(Pred_Transf == 0 & Y == 0)
  )

# E) Calculamos los nuevos parámetros de la distribución a posteriori (Prior 3,9)
resumen_post_transf <- datos %>%
  group_by(G) %>%
  summarise(
    n_pacificos = sum(Y == 0),
    x_fp = sum(FP_Transf)
  )

# Parámetros para Albatros
alpha_A_tr <- 3 + resumen_post_transf$x_fp[resumen_post_transf$G == "Albatros"]
beta_A_tr  <- 9 + (resumen_post_transf$n_pacificos[resumen_post_transf$G == "Albatros"] - 
                     resumen_post_transf$x_fp[resumen_post_transf$G == "Albatros"])

# Parámetros para Búhos
alpha_B_tr <- 3 + resumen_post_transf$x_fp[resumen_post_transf$G == "Buhos"]
beta_B_tr  <- 9 + (resumen_post_transf$n_pacificos[resumen_post_transf$G == "Buhos"] - 
                     resumen_post_transf$x_fp[resumen_post_transf$G == "Buhos"])

# F) Simulación para comparar
simulaciones <- 100000
post_A_transf <- rbeta(simulaciones, alpha_A_tr, beta_A_tr)
post_B_transf <- rbeta(simulaciones, alpha_B_tr, beta_B_tr)

# G) Visualización de la comparación
df_plot <- data.frame(
  Valor = c(post_A_transf, post_B_transf),
  Grupo = rep(c("Albatros (Transf)", "Búhos (Transf)"), each = simulaciones)
)

ggplot(df_plot, aes(x = Valor, fill = Grupo)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(
    title = "Comparación de Posteriores: Tasas de Falsos Positivos",
    subtitle = "Usando Scores Transformados (Umbrales Diferenciados)",
    x = "Tasa de Falsos Positivos (FPR)",
    y = "Densidad"
  )

# H) Probabilidad de que Búhos sea mayor a Albatros ahora:
prob_final <- mean(post_B_transf > post_A_transf)
print(paste("Probabilidad final de disparidad tras transformación:", prob_final))
#"Probabilidad final de disparidad tras transformación: 0.5446"

# ------------------------------------------------------------------------------
# 13) Para los scores transformados, realice nuevamente el calibration plot y 
#     construya también un calibration plot por grupo.

# A) Preparación de datos globales
calib_global_transf <- datos %>%
  mutate(bin = cut(Score_Transf / 100, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(
    prob_predicha = mean(Score_Transf) / 100,
    prop_real = mean(Y),
    n = n()
  )

# B) Gráfico Global Transformado
ggplot(calib_global_transf, aes(x = prob_predicha, y = prop_real)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_line(color = "darkgreen") +
  geom_point(aes(size = n), color = "darkgreen") +
  labs(title = "Calibration Plot Global (Scores Transformados)",
       x = "Probabilidad Predicha (Transformada)", y = "Proporción Real") +
  theme_minimal()

# C) Preparación de datos por Grupo
calib_grupo_transf <- datos %>%
  mutate(bin = cut(Score_Transf / 100, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
  group_by(G, bin) %>%
  summarise(
    prob_predicha = mean(Score_Transf) / 100,
    prop_real = mean(Y),
    n = n()
  )

# D) Gráfico por Grupo Transformado
ggplot(calib_grupo_transf, aes(x = prob_predicha, y = prop_real, color = G)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_line() +
  geom_point(aes(size = n)) +
  facet_wrap(~G) + # Esto separa los gráficos de Albatros y Búhos
  labs(title = "Calibration Plot por Grupo (Scores Transformados)",
       x = "Probabilidad Predicha (Transformada)", y = "Proporción Real") +
  theme_minimal()
# ------------------------------------------------------------------------------
#                                  SEGUNDA PARTE                                
# ------------------------------------------------------------------------------

# Realizaremos ahora un estudio por simulación para observar el comportamiento de
# las métricas analizadas y las decisiones tomadas.


# ------------------------------------------------------------------------------
# 14) Por qué los scores s1 y s2 resultan calibrados?

# Porque Y se genera como Bernoulli(s): por construcción, P(Y = 1 | S = s) = s
# para cualquier s y en ambos grupos. El score es exactamente la probabilidad del
# evento, que es la definición de calibración. Los grupos difieren solo en la
# distribución de los scores (Beta(2,5) vs Beta(4,2)), es decir, en la prevalencia.

# ------------------------------------------------------------------------------
# Simulación: 1000 repeticiones del proceso de generación de datos
# ------------------------------------------------------------------------------

set.seed(123)
n_sim <- 1000
lista_tablas <- list() # Una "carpeta" para guardar las 1000 tablas

for (i in 1:n_sim) {
  N1 <- 50
  N2 <- 50
  
  # Generamos los scores para cada grupo
  s1 <- rbeta(N1, 2, 5)
  s2 <- rbeta(N2, 4, 2)
  
  # Generamos la realidad Y basándonos en esos scores
  y_true <- rbinom((N1 + N2), 1, c(s1, s2))
  
  # Creamos la tabla de ESTA repetición
  tabla_actual <- data.frame(
    Repeticion = i,  # Guardamos el número de vuelta
    G = c(rep("Albatros", N1), rep("Buhos", N2)),
    Score = c(s1, s2) * 100,
    Y = y_true
  )
  
  # Guardamos la tabla en la lista
  lista_tablas[[i]] <- tabla_actual
}

# Pegamos las 1000 tablas una abajo de la otra
datos_completos_14 <- bind_rows(lista_tablas)

# Verificación objetiva
nrow(datos_completos_14) # Debería dar 100.000

# ------------------------------------------------------------------------------
# 15) Construya un calibration plot global y uno por grupo que incluya barras de 
#     error de acuerdo a las simulaciones.
# ------------------------------------------------------------------------------
# Procesamiento y Gráficos de Calibración
# ------------------------------------------------------------------------------

# A) PROCESAMIENTO: Agrupamos los datos en intervalos de 10 puntos
# Primero calculamos qué pasó en cada una de las 1000 repeticiones
resumen_por_repeticion <- datos_completos_14 %>%
  mutate(bin = cut(Score, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%  # bin son los intervalos
  group_by(Repeticion, G, bin) %>%
  summarise(
    p_obs = mean(Y),            # Realidad en esta repetición
    p_pred = mean(Score) / 100, # Predicción en esta repetición
    .groups = 'drop'
  )

# B) CÁLCULO DE MÉTRICAS FINALES: Promedio y Error Estándar
# Esto nos permite ver la variabilidad entre las 1000 simulaciones
datos_grafico_15 <- resumen_por_repeticion %>%
  group_by(G, bin) %>%
  summarise(
    media_obs = mean(p_obs),
    media_pred = mean(p_pred),
    se = sd(p_obs) / sqrt(n()), # Error estándar
    ic_inf = media_obs - 1.96 * se,
    ic_sup = media_obs + 1.96 * se,
    .groups = 'drop'
  )

# C) GRÁFICO POR GRUPO (Facetado)
ggplot(datos_grafico_15, aes(x = bin, y = media_obs, color = G)) +
  geom_abline(slope = 0.1, intercept = -0.05, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.02) +
  geom_line(aes(group = 1)) +
  geom_point() +
  facet_wrap(~G) +
  theme_minimal() +
  labs(
    title = "Calibration Plot por Grupo: 1000 Simulaciones",
    subtitle = "Las barras representan el Intervalo de Confianza al 95%",
    x = "Intervalos de Score (Probabilidad Predicha)",
    y = "Proporción Real de Incidentes (Observada)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# D) GRÁFICO GLOBAL (Toda la población)
datos_grafico_global <- resumen_por_repeticion %>%
  group_by(bin) %>%
  summarise(
    media_obs = mean(p_obs),
    media_pred = mean(p_pred),
    se = sd(p_obs) / sqrt(n()),
    ic_inf = media_obs - 1.96 * se,
    ic_sup = media_obs + 1.96 * se,
    .groups = 'drop'
  )

ggplot(datos_grafico_global, aes(x = bin, y = media_obs)) +
  geom_abline(slope = 0.1, intercept = -0.05, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.02, color = "darkblue") +
  geom_line( aes(group = 1),color = "darkblue") +
  geom_point(color = "darkblue") +
  theme_minimal() +
  labs(
    title = "Calibration Plot Global: 1000 Simulaciones",
    x = "Intervalo de Score (Probabilidad Predicha)",
    y = "Proporción Real de Incidentes (Observada)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------------------------
# 16) Realice un gráfico que permita comparar las distribuciones a posteriori FPR_B 
#     y FPR_A a lo largo de las simulaciones.

# ------------------------------------------------------------------------------
# OPCIÓN A: Comparación de Intervalos de Credibilidad (FPR) - 100 Simulaciones
# ------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# 1. Filtramos las primeras 100 repeticiones y solo los PACÍFICOS (Y=0)
resumen_16 <- datos_completos_14 %>%
  filter(Repeticion <= 80, Y == 0) %>%
  group_by(Repeticion, G) %>%
  summarise(
    FP = sum(Score > 50),
    VN = sum(Score <= 50),
    .groups = 'drop'
  ) %>%
  # 2. Calculamos los parámetros de la posterior y los cuantiles (Intervalo 95%)
  mutate(
    alpha_post = 3 + FP,
    beta_post = 9 + VN,
    media_post = alpha_post / (alpha_post + beta_post),
    inf = qbeta(0.025, alpha_post, beta_post), # Límite inferior 2.5%
    sup = qbeta(0.975, alpha_post, beta_post)  # Límite superior 97.5%
  )

# 3. El Gráfico: Eje X = FPR, Eje Y = Simulación
ggplot(resumen_16, aes(x = media_post, y = as.factor(Repeticion), color = G)) +
  # Usamos pointrange para dibujar el punto medio y el intervalito
  geom_pointrange(aes(xmin = inf, xmax = sup), 
                  position = position_dodge(width = 0.8), # Para que no se encimen A y B
                  size = 0.3) +
  scale_color_manual(values = c("Albatros" = "#1F77B4", "Buhos" = "#D62728")) +
  scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.1)) +
  # Invertimos el eje Y para que la simulación 1 esté arriba
  scale_y_discrete(limits = rev(as.character(1:80))) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5), # Texto chiquito porque son 100 filas
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "FPR por Simulación: Intervalos de Credibilidad (95%)",
    subtitle = "Comparación de 80 escenarios independientes (Prior Beta 3,9)",
    x = "Tasa de Falsos Positivos (FPR)",
    y = "Número de Simulación",
    color = "Equipo"
  )

# ------------------------------------------------------------------------------
# 17) Para los valores de T_A y T_B que encontró para igualar los FPR_g, compare 
#     las distribuciones a posteriori de FPR_B y FPR_A
# ------------------------------------------------------------------------------

# 2. Calculamos las estadísticas usando los umbrales específicos por grupo
resumen_17 <- datos_completos_14 %>%
  filter(Repeticion <= 80, Y == 0) %>% # Solo pacíficos
  group_by(Repeticion, G) %>%
  summarise(
    # Aplicamos T_A a Albatros y T_B a Buhos
    FP = sum(ifelse(G == "Albatros", Score > T_A_opt, Score > T_B_opt)),
    VN = sum(ifelse(G == "Albatros", Score <= T_A_opt, Score <= T_B_opt)),
    .groups = 'drop'
  ) %>%
  # 3. Calculamos parámetros y cuantiles de la posterior
  mutate(
    alpha_post = 3 + FP,
    beta_post = 9 + VN,
    media_post = alpha_post / (alpha_post + beta_post),
    inf = qbeta(0.025, alpha_post, beta_post),
    sup = qbeta(0.975, alpha_post, beta_post)
  )

# 4. El Gráfico "Espejo" (Donde se ve la igualdad)
ggplot(resumen_17, aes(x = media_post, y = as.factor(Repeticion), color = G)) +
  geom_pointrange(aes(xmin = inf, xmax = sup), 
                  position = position_dodge(width = 0.6), # Un poquito más cerca
                  size = 0.3) +
  scale_color_manual(values = c("Albatros" = "#1F77B4", "Buhos" = "#D62728")) +
  scale_x_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.05)) + # Ajustamos eje X para ver el detalle
  scale_y_discrete(limits = rev(as.character(1:80))) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  labs(
    title = "Punto 17: FPR Ajustado con Umbrales Diferenciados",
    subtitle = paste("Escenario Único: T_A =", T_A_opt, "| T_B =", T_B_opt),
    x = "Tasa de Falsos Positivos (FPR)",
    y = "Número de Simulación",
    color = "Equipo"
  )

# ------------------------------------------------------------------------------
# 18) Con las funciones f_A y f_B, ajuste los scores y repita el punto 15.
# ------------------------------------------------------------------------------

# 1. Definimos la función de transformación lineal por tramos (la del punto 12)
f_transf <- function(S, Tg) {
  ifelse(S <= Tg, 
         S * (50 / Tg), 
         50 + (S - Tg) * (50 / (100 - Tg)))
}

# 2. Aplicamos la transformación a los 100.000 datos simulados
# Usamos los umbrales óptimos T_A y T_B que encontramos antes

datos_18 <- datos_completos_14 %>%
  mutate(Score_Transf = ifelse(G == "Albatros", 
                               f_transf(Score, T_A_opt), 
                               f_transf(Score, T_B_opt)))

# 3. Resumen Global por Repetición y Bin del Score Transformado
resumen_18_global <- datos_18 %>%
  mutate(bin = cut(Score_Transf, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  group_by(Repeticion, bin) %>%
  summarise(
    p_pred = mean(Score_Transf) / 100, # Probabilidad predicha "mentirosa"
    p_obs = mean(Y),                   # Realidad observada
    .groups = 'drop'
  )

# 4. Calculamos promedios finales y barras de error
datos_grafico_18 <- resumen_18_global %>%
  group_by(bin) %>%
  summarise(
    media_obs = mean(p_obs),
    media_pred = mean(p_pred),
    se = sd(p_obs) / sqrt(n()),
    ic_inf = media_obs - 1.96 * se,
    ic_sup = media_obs + 1.96 * se,
    .groups = 'drop'
  )

# 5. Gráfico Final
ggplot(datos_grafico_18, aes(x = media_pred, y = media_obs)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.02, color = "#E67E22") +
  geom_line(color = "#E67E22", linewidth = 1) +
  geom_point(color = "#E67E22", size = 3) +
  theme_minimal() +
  labs(title = "Punto 18: Calibración Global de los Scores Transformados",
       subtitle = "Simulación de 1000 datasets con corrección por equidad",
       x = "Probabilidad Predicha (Score Transformado / 100)",
       y = "Proporción Real Observada")

# ------------------------------------------------------------------------------
# 19) A partir del análisis realizado, reflexione: ¿el sistema Centinela Delta es
#     injusto? ¿Qué opciones existen para mitigar la disparidad? ¿Qué se gana o 
#     se pierde?

# El sistema está bien calibrado y, aun así, es injusto en un sentido concreto:
# con un umbral común, un hincha pacífico de los Búhos tiene muchísima más
# probabilidad de que le prohíban la entrada que uno de los Albatros (FPR ~0.71
# vs ~0.03, P(FPR_B > FPR_A) ≈ 1). La simulación muestra que esto no es un
# accidente de la muestra: cuando las prevalencias difieren, calibración y
# tasas de error iguales no pueden cumplirse a la vez.
#
# Opciones: (a) umbrales por grupo o transformar los scores (puntos 10-12):
# iguala las FPR pero se pierde la calibración por grupo y se usa la pertenencia
# al grupo para decidir; (b) mejorar los datos (registro de incidentes con la
# misma vigilancia en ambos estadios), que ataca la causa; (c) usar el score como
# apoyo a una decisión humana y no como regla automática.
# Se gana equidad en los errores; se pierde calibración y algo de exactitud.



