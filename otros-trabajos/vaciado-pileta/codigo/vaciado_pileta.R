#Ejercicio 5 
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Definimos los parámetros iniciales
h0 <- 50
k1 <- 2
k2 <- 4

# 2. Calculamos los tiempos de vaciado (límites superiores)
t_vaciado_1 <- 2 * sqrt(h0) / k1  # Límite para k = 2
t_vaciado_2 <- 2 * sqrt(h0) / k2  # Límite para k = 4

# 3. Creamos la secuencia de tiempo
tiempo <- seq(0, max(t_vaciado_1, t_vaciado_2), length.out = 200)

# 4. Armamos el dataframe usando ifelse para cortar la curva
df <- data.frame(t = tiempo) %>%
  mutate(
    # Si t es menor o igual al tiempo de vaciado, calcula la fórmula. Si no, devuelve NA.
    altura_k1 = ifelse(t <= t_vaciado_1, (sqrt(h0) - (k1 / 2) * t)^2, NA),
    altura_k2 = ifelse(t <= t_vaciado_2, (sqrt(h0) - (k2 / 2) * t)^2, NA)
  ) %>%
  pivot_longer(cols = starts_with("altura"), 
               names_to = "Modelo", 
               values_to = "h") %>%
  mutate(Modelo = ifelse(Modelo == "altura_k1", "k = 2", "k = 4"))

# 5. Creamos un dataframe chiquito para marcar los puntos finales
puntos_finales <- data.frame(
  t = c(t_vaciado_1, t_vaciado_2),
  h = c(0, 0),
  Modelo = c("k = 2", "k = 4")
)

# 6. Graficamos
ggplot(df, aes(x = t, y = h, color = Modelo)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) + # na.rm = TRUE evita que R tire un warning por los NAs
  geom_point(data = puntos_finales, size = 3) + # Agrega el punto en el límite superior
  labs(
    title = "Curvas de vaciado para h0 = 50 cm",
    subtitle = "La curva finaliza al alcanzar el tiempo máximo de vaciado",
    x = "Tiempo (horas)",
    y = "Altura del agua (cm)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("k = 2" = "#2c7bb6", "k = 4" = "#d7191c"))

#Ejercicio 9
# Cargamos las librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Fijamos una semilla para que los resultados sean reproducibles
set.seed(2026)

# 1. Definimos la cantidad de simulaciones a priori
n_sims <- 100

# 2. Muestreamos parámetros desde nuestros Priors (Punto 9)
beta0_prior <- rnorm(n_sims, mean = 8.36, sd = 0.5)
beta1_prior <- rnorm(n_sims, mean = -2.5, sd = 0.8)
# (No graficamos sigma acá porque estamos viendo la recta teórica de vaciado)

# 3. Creamos un vector de tiempo (ej: de 0 a 6 horas)
tiempo_seq <- seq(0, 6, length.out = 100)

# 4. Armamos el dataframe con todas las simulaciones combinadas
df_prior <- expand.grid(sim = 1:n_sims, t = tiempo_seq) %>%
  left_join(data.frame(sim = 1:n_sims, beta0 = beta0_prior, beta1 = beta1_prior), by = "sim") %>%
  mutate(
    # Calculamos la recta en la escala transformada (raíz de h)
    mu_transformado = beta0 + beta1 * t,
    
    # Volvemos a la escala original (h en centímetros). 
    # Usamos pmax(0, ...) para evitar que la altura vuelva a subir matemáticamente
    # una vez que la pileta se vació.
    h_predicha = pmax(0, mu_transformado)^2
  )

# 5. Graficamos las curvas (Punto 10)
ggplot(df_prior, aes(x = t, y = h_predicha, group = sim)) +
  geom_line(alpha = 0.3, color = "#2c7bb6") +
  labs(
    title = "Pruebas Predictivas a Priori",
    subtitle = "100 trayectorias posibles de vaciado antes de observar los datos",
    x = "Tiempo transcurrido (horas)",
    y = "Altura del agua en la pileta (cm)"
  ) +
  theme_minimal()
#Ejercicio 11

# Asegurate de tener cargada la librería
library(brms)

# 1. Cargamos los datos reales de la historia de Roberto
datos_roberto <- data.frame(
  t = c(0, 0.5, 2),
  h = c(70, 55, 15)
)

# Creamos la variable respuesta transformada y = sqrt(h)
datos_roberto$y <- sqrt(datos_roberto$h)

# Apagamos el centrado cambiando la forma de escribir el prior
mis_priors_arreglados <- c(
  prior(normal(8.36, 0.5), class = "b", coef = "Intercept"),
  prior(normal(-2.5, 0.8), class = "b", coef = "t"),
  prior(exponential(1), class = "sigma")
)

# Ajustamos el modelo forzando el intercepto real (0 + Intercept)
modelo_ajustado <- brm(
  formula = y ~ 0 + Intercept + t,
  data = datos_roberto,
  family = gaussian(),
  prior = mis_priors_arreglados,
  seed = 2026,
  chains = 4,
  iter = 2000,
  warmup = 1000
)

# Imprimimos el resumen del ajuste para verificar que todo corrió bien
summary(modelo_ajustado)

#Ejercicio 15
#intervalo de predicción para la altura de agua a las 00:00

# 1. Creamos un data frame nuevo, limpio, solo con t = 2.5
tiempo_exacto <- data.frame(t = 2.5)

# 2. Hacemos la predicción asegurándonos de usar ESE data frame
pred_y_pura <- posterior_predict(modelo_ajustado, newdata = tiempo_exacto)

# 3. Transformamos a centímetros cortando en cero
pred_h_pura <- pmax(0, pred_y_pura)^2

# 4. Calculamos los cuantiles
mean(pred_h_pura)
quantile(pred_h_pura, probs = c(0.025, 0.975))

#Ejercicio 16
#probabilidad de que a las 00:30 queden más de 5 cm de agua

# 1. Definimos el tiempo a evaluar (t = 3 horas corresponde a las 00:30 hs)
tiempo_16 <- data.frame(t = 3)

# 2. Generamos las predicciones a posteriori (sumando la incertidumbre del modelo y de medición)
predicciones_y_16 <- posterior_predict(modelo_ajustado, newdata = tiempo_16)

# 3. Transformamos las muestras a centímetros reales
# Nuevamente cortamos en 0 para mantener la coherencia física
predicciones_h_16 <- pmax(0, predicciones_y_16)^2

# 4. Calculamos la probabilidad solicitada (P(h > 5))
# mean() sobre una condición lógica calcula automáticamente la proporción de casos favorables
round(mean(predicciones_h_16 > 5)*100,2)

#Ejercicio 17
#probabilidad de que la pileta ya esté vacía a las 00:30

# Usamos las predicciones de "y" a las 3 horas (predicciones_y_16) del punto anterior.
# Calculamos en qué proporción de los escenarios simulados el nivel de agua fue 0 o negativo.
round(mean(predicciones_h_16 <= 0)*100,2)

#Ejercicio 18
# 1. Extraemos la matriz de log-verosimilitudes (4000 simulaciones x 3 observaciones)
matriz_ll <- log_lik(modelo_ajustado)

# 2. Revertimos el logaritmo aplicando la función exponencial para tener las verosimilitudes
verosimilitudes <- exp(matriz_ll)

# 3. Calculamos el promedio para cada una de las 3 observaciones (promedio por columna)
promedio_verosim <- colMeans(verosimilitudes)

# 4. Aplicamos el logaritmo final y sumamos los valores de los 3 datos
lppd_valor <- sum(log(promedio_verosim))

cat("El valor del lppd intra-muestra para el modelo ajustado es:", round(lppd_valor, 3), "\n")




library(ggplot2)
library(patchwork)

# 1. Generamos un vector de tiempo continuo (100 puntos entre 0 y 5 horas)
# Esto garantiza que la curva original se vea perfectamente suave y redonda.
t_teorico <- seq(0, 5, length.out = 100)

# 2. Definimos el modelo teórico linealizado
# Supongamos que arranca en 100 cm (raíz = 10) y se vacía a un ritmo constante de -2.
y_teorico <- 10 - 2 * t_teorico

# 3. Volvemos a la escala física original elevando al cuadrado
h_teorico <- y_teorico^2

# Armamos el dataframe de ejemplo
datos_ejemplo <- data.frame(
  tiempo = t_teorico,
  altura = h_teorico,
  raiz_altura = y_teorico
)

# 4. Gráfico 1: Escala Original (Muestra la parábola física real)
g1_ideal <- ggplot(datos_ejemplo, aes(x = tiempo, y = altura)) +
  geom_line(color = "darkorange", size = 1.2) +
  labs(
    title = "Escala Original",
    subtitle = "Vaciado físico (Curva cuadrática)",
    x = "Tiempo transcurrido (Horas)",
    y = "Altura del agua (cm)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# 5. Gráfico 2: Escala Transformada (Muestra la recta perfecta)
g2_ideal <- ggplot(datos_ejemplo, aes(x = tiempo, y = raiz_altura)) +
  geom_line(color = "dodgerblue3", size = 1.2) +
  labs(
    title = "Escala Transformada",
    subtitle = "Modelo linealizado",
    x = "Tiempo transcurrido (Horas)",
    y = expression(paste("Raíz de la altura (", sqrt(h), ")"))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# 6. Despliegue lado a lado
g1_ideal + g2_ideal
