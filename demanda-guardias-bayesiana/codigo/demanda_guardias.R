# ==============================================================================
# Demanda en guardias hospitalarias — Modelos bayesianos para conteos
# Estadística Bayesiana · Lic. en Estadística (FCEyE, UNR) · 2026 · Trabajo en equipo
# Ejecutar desde la carpeta codigo/
# ==============================================================================
#==============================================================================#
# TRABAJO PRÁCTICO 2: ESTADÍSTICA BAYESIANA 
# Análisis de demanda: Hospital Dr. Manuel Puch vs. Dr. Juan José Malbrán
#==============================================================================#

library(ggplot2)
library(tidyverse)
library(lubridate)
library(coda)
library(mvtnorm)
library(patchwork)


#-----------------------------------------------------------------------------------------

#                                PARTE 1                                                  

#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#                                PUNTO 2                                                  
#-----------------------------------------------------------------------------------------

# Definición de parámetros previos

alpha_prior <- 1

beta_prior <- 0.1

# Se utiliza alpha = 1 para asegurar un soporte positivo y evitar el spike en 0, 

# asumiendo una demanda mínima existente en el Hospital Puch.
# Datos observados (calculados de tp2_llegadas_h1.csv)

sum_y <- 863  # Total de pacientes

n_obs <- 124  # Total de horas (22 a 00 hs durante el periodo)

# Cálculo de la Posteriori Exacta

alpha_post <- alpha_prior + sum_y

beta_post <- beta_prior + n_obs

# Resultados objetivos

# Posteriori Exacta: Gamma( 864 , 124.1 )

# Media a posteriori de mu: 6.962127 pacientes/hora

(1 / (alpha_post / beta_post)) * 60  #Tiempo promedio entre llegadas estimado: 8.618056 minutos

#-----------------------------------------------------------------------------------------
#                                PUNTO 3                                                  
#-----------------------------------------------------------------------------------------


datos_h1 <- read.csv("../datos/llegadas_hospital_puch.csv")

# 2. Procesamiento de los horarios

# Convertimos el texto a objeto de tiempo y extraemos fecha y hora

datos_h1 <- datos_h1 %>%
  
  mutate(ingreso = as.POSIXct(ingreso),
         
         fecha = as.Date(ingreso),
         
         hora = hour(ingreso))

# 3. Agrupamos para obtener las llegadas por cada hora (nuestra variable Y)

y_h1 <- datos_h1 %>%
  
  group_by(fecha, hora) %>%
  
  summarise(llegadas = n(), .groups = 'drop')



# 4. Estadísticos descriptivos

resumen_h1 <- y_h1 %>%
  
  summarise(
    
    n_horas = n(),
    
    total_pacientes = sum(llegadas),
    
    media = mean(llegadas),
    
    varianza = var(llegadas),
    
    minimo = min(llegadas),
    
    maximo = max(llegadas)
    
  )



print(resumen_h1)

# Verificación: los totales usados en el Punto 2 coinciden con los datos
stopifnot(sum(y_h1$llegadas) == sum_y, nrow(y_h1) == n_obs)

# n_horas total_pacientes media varianza minimo maximo

#  124             863  6.96     7.80      1     15



# 5. Visualización: Histogramas y Tendencias

# Histograma

ggplot(y_h1, aes(x = llegadas)) +
  
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  
  theme_minimal() +
  
  labs(title = "Distribución de llegadas por hora (Hospital Puch)",
       
       x = "Cantidad de personas", y = "Frecuencia")



# Serie de tiempo (para ver tendencias)

ggplot(y_h1, aes(x = fecha, y = llegadas)) +
  
  geom_line(color = "darkblue") +
  
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Línea de tendencia
  
  theme_minimal() +
  
  labs(title = "Evolución de llegadas por hora (Feb - Abr 2026)",
       
       x = "Fecha", y = "Llegadas por hora")

# 1. Calculamos los minutos transcurridos desde las 22:00 para cada paciente

datos_acumulados <- datos_h1 %>%
  
  mutate(
    
    # Convertimos a minutos desde las 22:00 (0 a 120)
    
    minutos_desde_inicio = as.numeric(difftime(ingreso, floor_date(ingreso, "day"), units = "mins")),
    
    minutos_relativos = minutos_desde_inicio - (22 * 60)
    
  ) %>%
  
  arrange(minutos_relativos) %>%
  
  # Creamos el conteo acumulado
  
  mutate(pacientes_acumulados = row_number())



# 2. Graficamos con una línea de referencia (diagonal perfecta)

ggplot(datos_acumulados, aes(x = minutos_relativos, y = pacientes_acumulados)) +
  
  geom_line(color = "#3b78b0", linewidth = 1) + # Color del Hospital Puch
  
  geom_abline(intercept = 0, slope = nrow(datos_acumulados)/120, 
              
              linetype = "dashed", color = "red", alpha = 0.5) +
  
  theme_minimal() +
  
  labs(
    
    title = "Tendencia de Llegadas Acumuladas (22:00 - 00:00)",
    
    subtitle = "La línea punteada representa una tasa constante",
    
    x = "Minutos transcurridos desde las 22:00",
    
    y = "Total de pacientes acumulados"
    
  ) +
  
  scale_x_continuous(breaks = seq(0, 120, 30), labels = c("22:00", "22:30", "23:00", "23:30", "00:00"))

#-----------------------------------------------------------------------------------------
#                                PUNTO 4                                                  
#-----------------------------------------------------------------------------------------
# 2. Rango de personas (y*) que podrían llegar en una hora

y_star <- 0:25

# 3. Calculamos la probabilidad para cada valor usando la Binomial Negativa

# Esta es la predictiva a posteriori analítica

prob_predictiva <- dnbinom(y_star, size = alpha_post, prob = beta_post / (beta_post + 1))

df_predictiva <- data.frame(pacientes = y_star, prob = prob_predictiva)

# 4. Graficamos la predictiva sobre el histograma de los datos observados

ggplot() +
  
  # Datos observados (Hospital Puch)
  
  geom_histogram(data = y_h1, aes(x = llegadas, y = ..density..), 
                 
                 binwidth = 1, fill = "lightblue", color = "white", alpha = 0.6) +
  
  # Distribución predictiva (puntos y líneas)
  
  geom_line(data = df_predictiva, aes(x = pacientes, y = prob), 
            
            color = "red", linewidth = 1) +
  
  geom_point(data = df_predictiva, aes(x = pacientes, y = prob), 
             
             color = "red", size = 2) +
  
  theme_minimal() +
  
  labs(title = "Distribución Predictiva a Posteriori (Punto 4)",
       
       subtitle = "Hospital Puch: Predicción de llegadas para una hora futura",
       
       x = "Cantidad de pacientes por hora (y*)",
       
       y = "Probabilidad / Densidad")



#-----------------------------------------------------------------------------------------
#                                PUNTO 5                                                  
#-----------------------------------------------------------------------------------------
# Parámetros de la predictiva (Binomial Negativa)

p_param <- beta_post / (beta_post + 1)

# 1. Probabilidad de que lleguen entre 5 y 10 personas por hora: P(5 <= Y* <= 10)

# Usamos sum() con dnbinom o la diferencia de pnbinom

prob_5_10 <- sum(dnbinom(5:10, size = alpha_post, prob = p_param))
# 2. Probabilidad de que lleguen 10 o más: P(Y* >= 10)

# Es lo mismo que 1 - P(Y* <= 9)

prob_10_mas <- 1 - pnbinom(9, size = alpha_post, prob = p_param)
# Resultados

round(prob_5_10, 2) #Probabilidad entre 5 y 10 personas: 0.7257

round(prob_10_mas, 2) #Probabilidad de 10 o más personas: 0.1665



#-----------------------------------------------------------------------------------------
#                                PUNTO 6                                                  
#-----------------------------------------------------------------------------------------
# 1. Definición de la función Log-Posterior (como antes)

log_posterior <- function(mu, sum_y, n_obs, alpha_prior, beta_prior) {
  
  if (mu <= 0) return(-Inf)
  
  return((sum_y + alpha_prior - 1) * log(mu) - (n_obs + beta_prior) * mu)
  
}
# 2. Configuración de las 4 cadenas

set.seed(42) # Para reproductibilidad

n_iter <- 5000

n_chains <- 4

sigma_prop <- 0.25

# Generamos 4 arranques aleatorios entre 5 y 10

starts <- runif(n_chains, min = 5, max = 10)

# Matriz para guardar los resultados (filas = iteraciones, columnas = cadenas)

all_chains <- matrix(NA, nrow = n_iter, ncol = n_chains)

# 3. Ejecución de Metropolis-Hastings para las 4 cadenas

for (j in 1:n_chains) {
  
  mu_current <- starts[j]
  for (i in 1:n_iter) {
    
    mu_prop <- rnorm(1, mean = mu_current, sd = sigma_prop)
    
    log_r <- log_posterior(mu_prop, 863, 124, 1, 0.1) - 
      
      log_posterior(mu_current, 863, 124, 1, 0.1)
    
    if (log(runif(1)) < log_r) {
      
      mu_current <- mu_prop
      
    }
    
    all_chains[i, j] <- mu_current
    
  }
  
}
# 4. Preparación de datos para los Traceplots (formato largo para ggplot)

df_chains <- as.data.frame(all_chains)

colnames(df_chains) <- paste0("Cadena_", 1:n_chains)

df_chains$iter <- 1:n_iter

df_long <- df_chains %>%
  
  pivot_longer(cols = starts_with("Cadena"), 
               
               names_to = "Cadena", 
               
               values_to = "mu")

# Para ver los valores iniciales guardados en el vector

print(starts) #9.574030 9.685377 6.430698 9.152238


#-----------------------------------------------------------------------------------------
#                                PUNTO 7                                                  
#-----------------------------------------------------------------------------------------
ggplot(df_long, aes(x = iter, y = mu, color = Cadena)) +
  
  geom_line(alpha = 0.7, linewidth = 0.5) +
  
  facet_wrap(~Cadena, ncol = 2) + # Los separa en 4 cuadros
  
  theme_minimal() +
  
  labs(title = "Traceplots de 4 Cadenas Independientes (Punto 7)",
       
       subtitle = "Arranques aleatorios entre 5 y 10",
       
       x = "Iteración", y = expression(mu)) +
  
  scale_color_brewer(palette = "Set1") +
  
  theme(legend.position = "none")

#BURN-IN

# 1. Definimos cuántas muestras descartar

# Tiramos las primeras 500 iteraciones (el 10% de la cadena)

burn_in <- 500

all_chains_post <- all_chains[(burn_in + 1):n_iter, ]
# 2. Transformamos a formato largo para ggplot

df_post <- as.data.frame(all_chains_post)

colnames(df_post) <- paste0("Cadena_", 1:n_chains)

df_post$iter <- (burn_in + 1):n_iter # Mantenemos el número de iteración real

df_post_long <- df_post %>%
  
  pivot_longer(cols = starts_with("Cadena"), 
               
               names_to = "Cadena", 
               
               values_to = "mu")



# 3. Graficamos las 4 cadenas ya estabilizadas

ggplot(df_post_long, aes(x = iter, y = mu, color = Cadena)) +
  
  geom_line(alpha = 0.6, linewidth = 0.4) +
  
  facet_wrap(~Cadena, ncol = 2) +
  
  theme_minimal() +
  
  labs(title = "Traceplots Post-Burn-in (Punto 7)",
       
       subtitle = paste("Se descartaron las primeras", burn_in, "muestras"),
       
       x = "Iteración", y = expression(mu)) +
  
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  
  theme(legend.position = "none")

# 1. Convertimos la matriz de cadenas post-burnin a un objeto 'mcmc.list'

# Esto es necesario para que las funciones de la librería 'coda' entiendan que son cadenas paralelas

lista_cadenas <- mcmc.list(
  
  mcmc(all_chains_post[,1]),
  
  mcmc(all_chains_post[,2]),
  
  mcmc(all_chains_post[,3]),
  
  mcmc(all_chains_post[,4])
  
)



# 2. Cálculo del Diagnóstico de Gelman-Rubin (R-hat)

# Queremos ver que el 'Point est.' sea casi 1.00

diag_rhat <- gelman.diag(lista_cadenas)

print(diag_rhat)



# 3. Cálculo del Tamaño Efectivo de Muestra (Neff)

# Suma el Neff de las 4 cadenas para ver el total de info útil
neff_C1<-effectiveSize(lista_cadenas[[1]])
print(sum(neff_C1))

neff_C2<-effectiveSize(lista_cadenas[[2]])
print(sum(neff_C2))

neff_C3<-effectiveSize(lista_cadenas[[3]])
print(sum(neff_C3))

neff_C4<-effectiveSize(lista_cadenas[[4]])
print(sum(neff_C4))

neff_total <- effectiveSize(lista_cadenas)

print(sum(neff_total))

#-----------------------------------------------------------------------------------------
#                                PUNTO 8                                                  
#-----------------------------------------------------------------------------------------
# 1. Colapsamos las 4 cadenas en un solo vector

mu_total_mcmc <- as.vector(all_chains_post)

# 2. Creamos el data frame para ggplot

df_comparacion <- data.frame(mu = mu_total_mcmc)
# 3. Graficamos
ggplot(df_comparacion, aes(x = mu)) +
  
  # Histograma de las 18,000 muestras del MCMC
  
  geom_histogram(aes(y = ..density..), bins = 50, 
                 
                 fill = "steelblue", color = "white", alpha = 0.6) +
  
  # Curva teórica de la Posteriori Exacta: Gamma(864, 124.1)
  
  stat_function(fun = dgamma, 
                
                args = list(shape = 864, rate = 124.1),
                
                color = "red", linewidth = 1.2) +
  
  theme_minimal() +
  
  labs(title = "Punto 8: Validación del Modelo MCMC",
       
       subtitle = "Muestras de Metropolis-Hastings vs. Posteriori Gamma Analítica",
       
       x = expression(mu), 
       
       y = "Densidad") +
  
  annotate("text", x = 7.5, y = 1.5, label = "Curva Gamma Exacta", color = "red", fontface = "bold")


#-----------------------------------------------------------------------------------------
#                                PUNTO 9                                                  
#-----------------------------------------------------------------------------------------
# 1. Generamos la muestra predictiva

# Por cada mu del MCMC, simulamos una observación de Poisson

set.seed(456)

y_pred_mcmc <- rpois(n = length(mu_total_mcmc), lambda = mu_total_mcmc)

# 2. Calculamos las probabilidades solicitadas en el Punto 5

# Probabilidad de que lleguen entre 5 y 10 personas: P(5 <= Y* <= 10)

prob_5_10_mcmc <- mean(y_pred_mcmc >= 5 & y_pred_mcmc <= 10)

# Probabilidad de que lleguen 10 o más: P(Y* >= 10)

prob_10_mas_mcmc <- mean(y_pred_mcmc >= 10)

# 3. Resultados

round(prob_5_10_mcmc, 2) #Probabilidad entre 5 y 10 personas: 0.7268

round(prob_10_mas_mcmc, 2) #Probabilidad de 10 o más personas: 0.1683

# 4. Comparación rápida con los valores analíticos que tenías

# (valores analíticos del Punto 5: 72.57% y 16.65%)

comparacion_probas <- data.frame(
  
  Escenario = c("Entre 5 y 10", "10 o más"),
  
  Analitico = c(0.7257, 0.1665),
  
  MCMC = c(prob_5_10_mcmc, prob_10_mas_mcmc)
  
)

print(comparacion_probas)

#-----------------------------------------------------------------------------------------
#                                PUNTO 11-12                                              
#-----------------------------------------------------------------------------------------

# 1. Definición de la Log-Posterior para eta = log(mu)

# Incluimos el término del Jacobiano: log(exp(eta)) = eta

log_posterior_eta <- function(eta, sum_y, n_obs, alpha_prior, beta_prior) {
  
  # mu = exp(eta)
  
  # La log-densidad de la Gamma en mu evaluada en exp(eta) mas el log-jacobiano
  
  return((sum_y + alpha_prior) * eta - (n_obs + beta_prior) * exp(eta))
  
}

# 2. Configuración: 4 cadenas de 5000 pasos

set.seed(123)

n_iter <- 5000

n_chains <- 4

sigma_eta <- 0.1 # Desvío para la propuesta en escala logarítmica

all_chains_eta <- matrix(NA, nrow = n_iter, ncol = n_chains)



# Arranques aleatorios para eta (log de valores entre 5 y 10)

starts_eta <- log(runif(n_chains, min = 5, max = 10))



# 3. Algoritmo de Metropolis-Hastings

for (j in 1:n_chains) {
  
  eta_curr <- starts_eta[j]
  
  for (i in 1:n_iter) {
    
    eta_prop <- rnorm(1, mean = eta_curr, sd = sigma_eta)
    
    
    
    log_r <- log_posterior_eta(eta_prop, 863, 124, 1, 0.1) - 
      
      log_posterior_eta(eta_curr, 863, 124, 1, 0.1)
    
    
    
    if (log(runif(1)) < log_r) {
      
      eta_curr <- eta_prop
      
    }
    
    all_chains_eta[i, j] <- eta_curr
    
  }
  
}



# 4. Transformación inversa para obtener muestras de mu

all_chains_mu_transf <- exp(all_chains_eta)



# 5. Visualización: Traceplots de mu (Punto 12)

df_post_transf <- as.data.frame(all_chains_mu_transf)

colnames(df_post_transf) <- paste0("Cadena_", 1:n_chains)

df_post_transf$iter <- 1:n_iter



df_long_transf <- df_post_transf %>%
  
  pivot_longer(cols = starts_with("Cadena"), names_to = "Cadena", values_to = "mu")



ggplot(df_long_transf, aes(x = iter, y = mu, color = Cadena)) +
  
  geom_line(alpha = 0.6, linewidth = 0.3) +
  
  facet_wrap(~Cadena, ncol = 2) +
  
  theme_minimal() +
  
  labs(title = "Traceplots con Transformación Logarítmica (Punto 12)",
       
       x = "Iteración", y = expression(mu)) +
  
  theme(legend.position = "none")



# 6. Cálculo de diagnósticos (quitando burn-in de 500)

cadenas_mcmc <- mcmc.list(lapply(1:n_chains, function(i) mcmc(all_chains_mu_transf[501:5000, i])))



r_hat_v2 <- gelman.diag(cadenas_mcmc)

n_eff_v2 <- effectiveSize(cadenas_mcmc)



print(r_hat_v2)

print(sum(n_eff_v2))



#-----------------------------------------------------------------------------------------
#                                PUNTO 13                                                 
#-----------------------------------------------------------------------------------------
# 1. Combinamos las 4 cadenas y descartamos el burn-in (500 muestras por cadena)

mu_final_v2 <- as.vector(all_chains_mu_transf[501:5000, ])

# 2. Generamos la Distribución Predictiva a Posteriori (y*)

# Simulamos un valor de Poisson para cada mu de nuestra muestra

set.seed(321)

y_pred_v2 <- rpois(n = length(mu_final_v2), lambda = mu_final_v2)

# 3. Calculamos las probabilidades solicitadas

# P(5 <= Y* <= 10)

prob_5_10_v2 <- mean(y_pred_v2 >= 5 & y_pred_v2 <= 10)

# P(Y* >= 10)

prob_10_mas_v2 <- mean(y_pred_v2 >= 10)
# 4. Resultados finales

round(prob_5_10_v2, 2) #Probabilidad entre 5 y 10 personas: 0.7291

round(prob_10_mas_v2, 2) #Probabilidad de 10 o más personas: 0.1649



#-----------------------------------------------------------------------------------------

#                                PARTE 2                                                  

#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#                                PUNTO 14                                                 
#-----------------------------------------------------------------------------------------
# Leer los datos del Hospital Malbrán

datos_h2 <- read.csv("../datos/llegadas_hospital_malbran.csv")
# 3. Procesar las fechas para agrupar por hora

# Convertimos la columna 'ingreso' a formato fecha-hora

datos_h2$ingreso <- as.POSIXct(datos_h2$ingreso, format="%Y-%m-%d %H:%M:%S")

# Creamos la tabla de conteos por hora

llegadas_por_hora_h2 <- datos_h2 %>%
  
  mutate(fecha = as.Date(ingreso),
         
         hora = hour(ingreso)) %>%
  
  group_by(fecha, hora) %>%
  
  summarise(cantidad_personas = n(), .groups = 'drop')

# 4. Crear el Histograma

ggplot(llegadas_por_hora_h2, aes(x = cantidad_personas)) +
  
  geom_histogram(binwidth = 1, fill = "coral", color = "white", alpha = 0.8) +
  
  scale_x_continuous(breaks = seq(0, max(llegadas_por_hora_h2$cantidad_personas), by = 2)) +
  
  theme_minimal() +
  
  labs(title = "Distribución de la Demanda - Hospital Malbrán (Punto 14)",
       
       subtitle = "Frecuencia de la cantidad de personas que llegan por hora",
       
       x = "Cantidad de personas por hora",
       
       y = "Frecuencia (Horas observadas)") +
  
  # Agregamos una línea para la media observada
  
  geom_vline(aes(xintercept = mean(cantidad_personas)), 
             
             color = "darkred", linetype = "dashed", size = 1) +
  
  annotate("text", x = mean(llegadas_por_hora_h2$cantidad_personas) + 2, y = 15, 
           
           label = paste("Media =", round(mean(llegadas_por_hora_h2$cantidad_personas), 2)), 
           
           color = "darkred")

#-----------------------------------------------------------------------------------------
#                                PUNTO 15                                                 
#-----------------------------------------------------------------------------------------
# 1. Datos del Hospital Malbrán

# n = 124 horas observadas, sum_y = 961 pacientes totales

n_h2 <- 124

sum_y_h2 <- 961

alpha_prior <- 1

beta_prior <- 0.1



# 2. Función Log-Posterior para eta = log(mu)

log_post_eta_h2 <- function(eta, sum_y, n, a, b) {
  
  # Incluye el log-jacobiano (eta)
  
  return((sum_y + a) * eta - (n + b) * exp(eta))
  
}

# 3. Configuración del MCMC

set.seed(123)

n_iter <- 5000

n_chains <- 4

sigma_eta <- 0.08 # Ajustamos el salto para el Malbrán

all_chains_eta_h2 <- matrix(NA, nrow = n_iter, ncol = n_chains)

# Arranques aleatorios para eta entre log(5) y log(15) 

# (el Malbrán tiene picos más altos, así que ampliamos el rango)

starts_eta_h2 <- log(runif(n_chains, 5, 15))

# 4. Loop de Metropolis-Hastings

for (j in 1:n_chains) {
  
  eta_curr <- starts_eta_h2[j]
  
  for (i in 1:n_iter) {
    
    eta_prop <- rnorm(1, eta_curr, sigma_eta)
    log_r <- log_post_eta_h2(eta_prop, sum_y_h2, n_h2, alpha_prior, beta_prior) - 
      
      log_post_eta_h2(eta_curr, sum_y_h2, n_h2, alpha_prior, beta_prior)
    
    if (log(runif(1)) < log_r) {
      
      eta_curr <- eta_prop
      
    }
    
    all_chains_eta_h2[i, j] <- eta_curr
    
  }
  
}

# 5. Transformación inversa a mu

all_chains_mu_h2 <- exp(all_chains_eta_h2)

# 6. Visualización: Traceplots (Punto 15)

df_h2 <- as.data.frame(all_chains_mu_h2)

colnames(df_h2) <- paste0("Cadena_", 1:n_chains)

df_h2$iter <- 1:n_iter

df_h2_long <- df_h2 %>%
  
  pivot_longer(cols = starts_with("Cadena"), names_to = "Cadena", values_to = "mu")

ggplot(df_h2_long, aes(x = iter, y = mu, color = Cadena)) +
  
  geom_line(alpha = 0.6, linewidth = 0.3) +
  
  facet_wrap(~Cadena, ncol = 2) +
  
  theme_minimal() +
  
  labs(title = "Traceplots Hospital Malbrán (Punto 15)",
       
       subtitle = "MCMC con transformación logarítmica",
       
       x = "Iteración", y = expression(mu)) +
  
  theme(legend.position = "none")

#BURN-IN

# 1. Aplicamos el Burn-in (descartamos los primeros 500 pasos de cada cadena)

# Conservamos de la iteración 501 a la 5000

burn_in <- 500

cadenas_h2_clean <- all_chains_mu_h2[(burn_in + 1):5000, ]



# 2. Preparamos los datos para los nuevos Traceplots

df_h2_clean <- as.data.frame(cadenas_h2_clean)

colnames(df_h2_clean) <- paste0("Cadena_", 1:4)

df_h2_clean$iter <- (burn_in + 1):5000



df_h2_clean_long <- df_h2_clean %>%
  
  pivot_longer(cols = starts_with("Cadena"), names_to = "Cadena", values_to = "mu")

# 3. Graficamos los Traceplots Post-Burn-in

ggplot(df_h2_clean_long, aes(x = iter, y = mu, color = Cadena)) +
  
  geom_line(alpha = 0.6, linewidth = 0.3) +
  
  facet_wrap(~Cadena, ncol = 2) +
  
  theme_minimal() +
  
  labs(title = "Traceplots Estabilizados - Hospital Malbrán (Punto 15)",
       
       subtitle = paste("Se eliminaron las primeras", burn_in, "muestras de calentamiento"),
       
       x = "Iteración", y = expression(mu)) +
  
  theme(legend.position = "none")

# 4. Cálculo de Diagnósticos Finales (R-hat y Neff)
# Convertimos a objeto mcmc.list para la librería coda

lista_mcmc_h2 <- mcmc.list(lapply(1:4, function(i) mcmc(cadenas_h2_clean[, i])))



r_hat_h2 <- gelman.diag(lista_mcmc_h2) #     Point est. Upper C.I.

#                                       [1,]          1          1



n_eff_h2 <- effectiveSize(lista_mcmc_h2) #4258.46



#-----------------------------------------------------------------------------------------
#                                PUNTO 16                                                 
#-----------------------------------------------------------------------------------------
# 1. Consolidamos todas las muestras post-burn-in de las 4 cadenas

mu_total_h2 <- as.vector(cadenas_h2_clean)

# 2. Generamos la Distribución Predictiva a Posteriori (y_pred)

# Simulamos una observación Poisson(mu) por cada valor de la cadena

set.seed(987)

y_pred_h2 <- rpois(n = length(mu_total_h2), lambda = mu_total_h2)

# 3. Calculamos las probabilidades solicitadas en el Punto 16

# P(5 <= Y* <= 10)

prob_5_10_h2 <- mean(y_pred_h2 >= 5 & y_pred_h2 <= 10) #0.7223333

# P(Y* >= 10)

prob_10_mas_h2 <- mean(y_pred_h2 >= 10) #0.2566111

#-----------------------------------------------------------------------------------------
#                                PUNTO 17                                                 
#-----------------------------------------------------------------------------------------

# 1. Función General de PPC (Basada en las Págs 4 y 5 del TP)

graficar_PPC <- function(y_reales, mu_muestras, titulo) {
  
  n_obs <- length(y_reales)
  
  S <- length(mu_muestras)
  # Simulacion de muestras predictivas a posteriori (y_rep)
  
  # Generamos una matriz de n_obs filas y S columnas
  
  set.seed(123)
  
  y_rep <- matrix(rpois(n_obs * S, lambda = rep(mu_muestras, each = n_obs)), 
                  
                  nrow = n_obs)
  
  # Definición de intervalos para obtener conteos
  
  max_val <- max(max(y_reales), max(y_rep))
  
  breaks <- seq(-0.5, max_val + 0.5, by = 1)
  
  # Frecuencia relativa observada
  
  y_obs_hist <- hist(y_reales, plot = FALSE, breaks = breaks)$counts
  
  y_obs_rel <- y_obs_hist / sum(y_obs_hist)
  
  # Frecuencia relativa predictiva
  
  props <- apply(y_rep, 2, function(x) {
    
    counts <- hist(x, plot = FALSE, breaks = breaks)$counts
    
    counts / sum(counts)
    
  })
  
  
  # Obtencion de media y bandas
  
  y_mean    <- apply(props, 1, mean)
  
  lower_90  <- apply(props, 1, quantile, 0.05)
  
  upper_90  <- apply(props, 1, quantile, 0.95)
  
  lower_50  <- apply(props, 1, quantile, 0.25)
  
  upper_50  <- apply(props, 1, quantile, 0.75)
  
  # Preparacion de datos para el gráfico
  
  df_plot <- data.frame(
    
    x = 0:(length(y_mean)-1),
    
    y_obs = y_obs_rel,
    
    y_mean = y_mean,
    
    lower_90 = lower_90,
    
    upper_90 = upper_90,
    
    lower_50 = lower_50,
    
    upper_50 = upper_50
    
  )
  
  
  # Grafico estilo ggplot (Pág 5 del PDF)
  
  ggplot(df_plot) +
    
    geom_pointrange(aes(x = x + 0.25, y = y_mean, ymin = lower_90, ymax = upper_90, color = "Predictiva"),
                    
                    size = 0.5, linewidth = 1) +
    
    geom_pointrange(aes(x = x + 0.25, y = y_mean, ymin = lower_50, ymax = upper_50, color = "Predictiva"),
                    
                    size = 0.5, linewidth = 1.4) +
    
    geom_segment(aes(x = x, xend = x, y = 0, yend = y_obs, color = "Observada"), linewidth = 1) +
    
    geom_point(aes(x = x, y = y_obs, color = "Observada"), size = 3) +
    
    scale_color_manual(values = c("Observada" = "#3b78b0", "Predictiva" = "grey60"), name = NULL) +
    
    labs(title = titulo, x = expression(tilde(y)), y = expression("p(" * tilde(y) * " | y)")) +
    
    theme_classic()
  
}

# 2. Ejecución para Hospital Puch (H1)

# Usamos: y_h1$llegadas y mu_total_mcmc

graficar_PPC(y_h1$llegadas, mu_total_mcmc, "PPC - Hospital Dr. Manuel Puch")

# 3. Ejecución para Hospital Malbrán (H2)

# Usamos: llegadas_por_hora_h2$cantidad_personas y mu_total_h2

graficar_PPC(llegadas_por_hora_h2$cantidad_personas, mu_total_h2, "PPC - Hospital Dr. Juan José Malbrán")

#-----------------------------------------------------------------------------------------
#                                PUNTO 18                                                 
#-----------------------------------------------------------------------------------------
# Función para generar datos comparativos

comparar_distribuciones <- function(mu_val, phi_val, titulo) {
  
  x_vals <- 0:25
  
  data.frame(
    
    pacientes = x_vals,
    
    Poisson = dpois(x_vals, lambda = mu_val),
    
    NB = dnbinom(x_vals, mu = mu_val, size = 1/phi_val)
    
  ) %>%
    
    pivot_longer(cols = c("Poisson", "NB"), names_to = "Modelo", values_to = "Prob") %>%
    
    ggplot(aes(x = pacientes, y = Prob, fill = Modelo)) +
    
    geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
    
    scale_fill_manual(values = c("Poisson" = "#3b78b0", "NB" = "coral")) +
    
    theme_minimal() +
    
    labs(title = titulo, subtitle = paste0("mu = ", mu_val, " | phi = ", phi_val),
         
         x = "Pacientes/hora", y = "Probabilidad")
  
}

# Hospital 1 (Puch): Usamos una sobredispersión muy baja (phi casi 0)

# donde el modelo Poisson y NB deberían casi solaparse.

g1 <- comparar_distribuciones(mu_val = 6.96, phi_val = 0.01, "Hospital Puch (H1)")

# Hospital 2 (Malbrán): Usamos la sobredispersión observada (phi ~ 0.16)

# donde se nota que la NB se "estira" para capturar la varianza.

g2 <- comparar_distribuciones(mu_val = 7.75, phi_val = 0.16, "Hospital Malbrán (H2)")

g1 / g2 # Muestra ambos uno arriba del otro


# Definimos los valores de lambda que queremos probar

lambdas <- c(0.5, 1, 2)

# Rango de valores para phi (sobredispersión)

phi_vals <- seq(0, 5, length.out = 500)

# Generamos los datos para graficar

df_prior <- expand.grid(phi = phi_vals, lambda = lambdas)

df_prior$densidad <- dexp(df_prior$phi, rate = df_prior$lambda)

df_prior$lambda <- as.factor(df_prior$lambda)

# Graficamos

ggplot(df_prior, aes(x = phi, y = densidad, color = lambda)) +
  
  geom_line(linewidth = 1.2) +
  
  theme_minimal() +
  
  labs(
    
    title = "Distribución A Priori Exponencial para phi (Punto 19)",
    
    subtitle = "Comparación de diferentes valores de lambda",
    
    x = expression(phi),
    
    y = "Densidad de Probabilidad",
    
    color = "Valor de lambda"
    
  ) +
  
  geom_vline(xintercept = 0.16, linetype = "dashed", color = "darkgrey") +
  
  annotate("text", x = 0.6, y = 1.5, label = "H2 ~ 0.16", color = "darkgrey")


#-----------------------------------------------------------------------------------------
#                                PUNTO 21                                                
#-----------------------------------------------------------------------------------------

# 1. Función Log-Posterior Conjunta (mu, phi)

# Usamos transformaciones logarítmicas para asegurar positividad

log_posterior_nb <- function(theta, y) {
  
  # theta[1] = eta = log(mu)
  
  # theta[2] = zeta = log(phi)
  
  eta <- theta[1]
  
  zeta <- theta[2]
  
  mu <- exp(eta)
  
  phi <- exp(zeta)
  
  # a. Log-Verosimilitud (Binomial Negativa)
  
  # R usa la parametrización mu y size (donde size = 1/phi)
  
  log_lik <- sum(dnbinom(y, mu = mu, size = 1/phi, log = TRUE))
  
  # b. Log-Priores
  
  log_prior_mu <- dgamma(mu, shape = 1, rate = 0.1, log = TRUE)
  
  log_prior_phi <- dexp(phi, rate = 2, log = TRUE)
  
  
  # c. Log-Jacobiano de la transformación
  
  # Como mu = exp(eta), dmu/deta = exp(eta). En log es eta.
  
  log_jacobiano <- eta + zeta
  
  
  
  return(log_lik + log_prior_mu + log_prior_phi + log_jacobiano)
  
}

# 2. Algoritmo Metropolis-Hastings 2D

# Esta función es genérica para cualquier hospital

correr_mcmc_2d <- function(datos, n_iter = 10000, sigma_prop) {
  
  muestras <- matrix(NA, nrow = n_iter, ncol = 2)
  
  # Inicio: media de los datos y una sobredispersión inicial pequeña
  
  muestras[1, ] <- c(log(mean(datos)), log(0.1))
  
  
  
  set.seed(42)
  
  for (i in 2:n_iter) {
    
    curr_theta <- muestras[i-1, ]
    
    # Propuesta Normal Bivariada
    
    prop_theta <- rmvnorm(1, mean = curr_theta, sigma = sigma_prop)[1, ]
    # Probabilidad de aceptación
    
    log_r <- log_posterior_nb(prop_theta, datos) - log_posterior_nb(curr_theta, datos)
    if (log(runif(1)) < log_r) {
      
      muestras[i, ] <- prop_theta
      
    } else {
      
      muestras[i, ] <- curr_theta
      
    }
    
  }
  
  
  
  # Transformamos de vuelta a escala original mu y phi
  
  df_res <- as.data.frame(exp(muestras))
  
  colnames(df_res) <- c("mu", "phi")
  
  df_res$iter <- 1:n_iter
  
  return(df_res)
  
}

# 3. Ejecución por Hospital con calibración de propuesta

# Nota: sigma_prop controla qué tan grandes son los saltos de la "hormiga"

sigma_p <- diag(c(0.005, 0.05)) # Matriz de covarianza de la propuesta



# Hospital Puch (H1)

mcmc_puch <- correr_mcmc_2d(y_h1$llegadas, n_iter = 10000, sigma_prop = sigma_p) %>%
  
  filter(iter > 1000) %>%
  
  mutate(Hospital = "Puch (H1)")



# Hospital Malbrán (H2)

mcmc_malbran <- correr_mcmc_2d(llegadas_por_hora_h2$cantidad_personas, n_iter = 10000, sigma_prop = sigma_p) %>%
  
  filter(iter > 1000) %>%
  
  mutate(Hospital = "Malbrán (H2)")

# 4. Consolidación de resultados

df_nb_final <- rbind(mcmc_puch, mcmc_malbran)
# 5. Visualización: Mapa de Calor (Heatmap)

ggplot(df_nb_final, aes(x = mu, y = phi)) +
  
  geom_density_2d_filled(alpha = 0.9) +
  
  facet_wrap(~Hospital, scales = "free") +
  
  theme_minimal() +
  
  labs(title = "Posterior Conjunta (mu, phi) - Mapa de Calor",
       
       subtitle = "Punto 21: Comparación de la demanda media y sobredispersión",
       
       x = expression(mu), y = expression(phi), fill = "Densidad")

# Gráfico final con escalas UNIFICADAS para comparar bien

# Unimos los dos resultados en un solo objeto para que ggplot pueda facetar

df_final <- rbind(mcmc_puch, mcmc_malbran)

ggplot(df_final, aes(x = mu, y = phi)) +
  
  geom_density_2d_filled(alpha = 0.9) +
  
  # USAMOS SCALES = "FIXED" para que la comparación sea real
  
  facet_wrap(~Hospital, scales = "fixed") + 
  
  theme_minimal() +
  
  labs(
    
    title = "Comparación Real de Posteriores: Puch vs. Malbrán",
    
    subtitle = "Ahora con escalas fijas para apreciar la sobredispersión",
    
    x = expression(mu), y = expression(phi),
    
    fill = "Densidad"
    
  ) +
  
  # Ajustamos los límites para ver bien ambos centros
  
  coord_cartesian(xlim = c(6, 9), ylim = c(0, 0.3))



#VERIFICACION DE BUEN FUNCIONAMIENTO

# 1. Función de MCMC mejorada (con contador de aceptación)

mcmc_verificado <- function(datos, n_iter = 10000, sigma_prop) {
  
  muestras <- matrix(NA, nrow = n_iter, ncol = 2)
  
  aceptados <- 0
  
  
  
  # Inicio aleatorio para dar robustez al R-hat
  
  muestras[1, ] <- c(log(runif(1, 5, 10)), log(runif(1, 0.01, 0.5)))
  
  
  
  for (i in 2:n_iter) {
    
    curr_theta <- muestras[i-1, ]
    
    prop_theta <- rmvnorm(1, mean = curr_theta, sigma = sigma_prop)[1, ]
    
    
    
    log_r <- log_posterior_nb(prop_theta, datos) - log_posterior_nb(curr_theta, datos)
    
    if (log(runif(1)) < log_r) {
      
      muestras[i, ] <- prop_theta
      
      aceptados <- aceptados + 1
      
    } else {
      
      muestras[i, ] <- curr_theta
      
    }
    
  }
  
  
  
  return(list(
    
    muestras = as.data.frame(exp(muestras)) %>% setNames(c("mu", "phi")) %>% mutate(iter = 1:n_iter),
    
    tasa_acep = aceptados / n_iter
    
  ))
  
}

# 2. Correr 4 cadenas por Hospital

set.seed(42)

sigma_p <- diag(c(0.005, 0.05)) # Propuesta calibrada para 2D



cadenas_h1 <- lapply(1:4, function(i) mcmc_verificado(y_h1$llegadas, 10000, sigma_p))

cadenas_h2 <- lapply(1:4, function(i) mcmc_verificado(llegadas_por_hora_h2$cantidad_personas, 10000, sigma_p))

# 3. Función para graficar Traceplots de 4 cadenas

graficar_trazas_completas <- function(lista_cadenas, titulo) {
  
  df_trazas <- bind_rows(lapply(1:4, function(i) lista_cadenas[[i]]$muestras %>% mutate(cadena = as.factor(i))))
  
  
  
  p1 <- ggplot(df_trazas, aes(x = iter, y = mu, color = cadena)) +
    
    geom_line(alpha = 0.5) + theme_minimal() + labs(title = paste(titulo, "- mu"))
  
  
  
  p2 <- ggplot(df_trazas, aes(x = iter, y = phi, color = cadena)) +
    
    geom_line(alpha = 0.5) + theme_minimal() + labs(title = paste(titulo, "- phi"))
  
  
  
  print(p1); print(p2)
  
}

# 4. Cálculo de Diagnósticos (Post Burn-in)

diagnosticar <- function(lista_cadenas) {
  
  # Convertimos a mcmc.list quitando 1000 de burn-in
  
  mcmc_obj <- mcmc.list(lapply(1:4, function(i) mcmc(lista_cadenas[[i]]$muestras[1001:10000, 1:2])))
  
  
  
  cat("\n--- R-hat (Gelman-Rubin) ---\n")
  
  print(gelman.diag(mcmc_obj))
  
  
  
  cat("\n--- Neff (Tamaño Efectivo) ---\n")
  
  print(effectiveSize(mcmc_obj))
  
  
  
  cat("\n--- Tasas de Aceptación Promedio ---\n")
  
  tasas <- sapply(1:4, function(i) lista_cadenas[[i]]$tasa_acep)
  
  print(mean(tasas))
  
}



# --- RESULTADOS FINALES ---



# Hospital Puch

graficar_trazas_completas(cadenas_h1, "H1 Puch")

diagnosticar(cadenas_h1)



# Hospital Malbrán

graficar_trazas_completas(cadenas_h2, "H2 Malbrán")

diagnosticar(cadenas_h2)
#==============================================================================#
# PUNTO 23: DISTRIBUCIÓN DE MÉDICOS (TRANSFORMACIÓN DE LA POSTERIORI)
#==============================================================================#

# 0. Preparación de datos (cadenas del Punto 21)
# Unimos las 4 cadenas de cada hospital y quitamos el burn-in de 1000 iteraciones
df_h1_final <- bind_rows(lapply(1:4, function(i) cadenas_h1[[i]]$muestras)) %>% 
  filter(iter > 1000)

df_h2_final <- bind_rows(lapply(1:4, function(i) cadenas_h2[[i]]$muestras)) %>% 
  filter(iter > 1000)

# REGLA: 1 médico cada 3 pacientes
k <- 3 

# 1. Generamos la Predictiva (y*) REUTILIZANDO los resultados del MCMC 2D
set.seed(789)
S <- nrow(df_h1_final) # Cantidad total de muestras

# Generamos y* usando mu y phi de cada iteración del MCMC para Hospital Puch
y_star_h1_nb <- rnbinom(S, mu = df_h1_final$mu, size = 1/df_h1_final$phi)

# Generamos y* para Hospital Malbrán
y_star_h2_nb <- rnbinom(S, mu = df_h2_final$mu, size = 1/df_h2_final$phi)

# 2. Aplicamos la Regla de Decisión: N = techo(y* / k)
medicos_h1 <- ceiling(y_star_h1_nb / k)
medicos_h2 <- ceiling(y_star_h2_nb / k)

# 3. Consolidamos resultados para el informe
df_medicos_final <- data.frame(
  Cant_Medicos = c(medicos_h1, medicos_h2),
  Hospital = rep(c("Puch (H1)", "Malbrán (H2)"), each = S)
)

# 4. Tabla resumen objetiva
tabla_final_p23 <- df_medicos_final %>%
  group_by(Hospital, Cant_Medicos) %>%
  summarise(Probabilidad = n() / S, .groups = 'drop') %>%
  filter(Cant_Medicos <= 8) # Limitamos a 8 para que la tabla sea legible

print(tabla_final_p23)

# 5. Visualización Final comparativa
ggplot(tabla_final_p23, aes(x = factor(Cant_Medicos), y = Probabilidad, fill = Hospital)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Puch (H1)" = "#3b78b0", "Malbrán (H2)" = "coral")) +
  theme_minimal() +
  labs(
    title = "Distribución a Posteriori de Médicos Necesarios (Punto 23)",
    subtitle = "Basado en el modelo Binomial Negativa (mu, phi) consolidado",
    x = "Cantidad de Médicos (N = techo(y*/3))",
    y = "Probabilidad"
  )
