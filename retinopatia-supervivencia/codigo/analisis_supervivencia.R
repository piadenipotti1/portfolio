# ==============================================================================
# Tiempo hasta la ceguera en retinopatía diabética — Análisis de supervivencia
# Análisis de Datos de Duración · Lic. en Estadística (FCEyE, UNR) · 2026
# Autoras: M. Eugenia Mogetta, Emilia Díaz Romero, Pía Denipotti
# Ejecutar desde la carpeta codigo/
# ==============================================================================

library(ggplot2)
library(survival)
library(survminer)
library(tidyverse)

datos <- read.csv("../datos/retinopatia_centro4.csv")
datos <- unique(datos)  # el archivo trae 26 filas duplicadas: quedan 94 ojos


# Renombrar las columnas
datos <- datos %>% 
  rename(
    id = id,                  # Se mantiene igual
    tiempo = time,            # tiempo en meses
    estado = status,          # 1: ceguera, 0: censura
    edad = age,               # edad al diagnóstico
    ojo = eye,                # derecho o izquierdo
    riesgo = risk,            # puntaje de riesgo
    tratamiento = trt,        # 1: tratado, 0: no tratado
    laser = laser,            # tipo de láser (Argón/Xenón)
    tratamiento_laser = tratamiento_laser # combinación final
  )

summary(datos)

datos %>% 
  ggplot() +
  aes(x = tratamiento_laser, y = tiempo) +
  geom_boxplot(fill = "lightblue3", outlier.shape = NA) + 
  geom_jitter(aes(color = as.factor(estado)), width = 0.2, alpha = 0.6) + 
  scale_color_manual(values = c("0" = "red4", "1" = "green4"), 
                     labels = c("Censurado", "Evento"), 
                     name = "Estado") +
  ggtitle("Distribución de tiempos según tratamiento") +
  xlab("Tratamiento") +
  ylab("Tiempo de seguimiento") +
  labs(caption = "Nota: Los puntos rojos indican tiempos censurados (observación incompleta)") +
  theme_bw()

datos$S <- Surv(time = datos$tiempo, event = datos$estado)
print(datos$S)

# Estimador de Kaplan-Meier (Curva de supervivencia general)

fit_global <- survfit(S ~ 1, data = datos)

# Resumen de los tiempos 
summary(fit_global)

# Estimación por grupo de tratamiento
fit_trat <- survfit(S ~ tratamiento, data = datos)

# Visualización con ggsurvplot 

ggsurvplot(fit_trat, 
           data = datos,
           conf.int = TRUE,           
           pval = TRUE,               
           risk.table = TRUE,         
           legend.title = "",               
           risk.table.title = "Número en riesgo", 
           risk.table.y.text = FALSE,        
           legend.labs = c("No tratado", "Tratado"),
           palette = c("red4", "lightblue4"),
           title = "Curvas de Supervivencia según Tratamiento",
           xlab = "Tiempo (meses)", 
           ylab = "Probabilidad de visión conservada",
           ggtheme = theme_bw()
)


# Cálculo del porcentaje de censura 
tabla_evento <- table(datos$estado)
porcentaje_censura <- (tabla_evento["0"] / sum(tabla_evento)) * 100
# El porcentaje de observaciones censuradas es: 60.64 %

# Test de Log-Rank
# Ho: No hay diferencias en las funciones de supervivencia entre los grupos
test_logrank <- survdiff(S ~ tratamiento, data = datos)
test_logrank

# Comparamos los 2 tratamientos:

# Creamos un dataset solo con los ojos tratados
datos_solo_laser <- datos %>% 
  filter(tratamiento == 1)

# Ajustamos la curva de supervivencia para estos dos grupos
fit_comparacion <- survfit(S ~ laser, data = datos_solo_laser)

# Graficamos la comparación Xenón vs Argón
ggsurvplot(fit_comparacion, 
           data = datos_solo_laser,
           pval = TRUE,             
           risk.table = TRUE,
           risk.table.title = "Número en riesgo",
           risk.table.y.text = FALSE,
           legend.title = "",               
           legend.labs = c("Argon", "Xenon"),
           palette = c("lightblue4", "red4"),
           title = "Comparación de Eficacia: Argón vs. Xenón",
           xlab = "Tiempo (meses)", 
           ylab = "Probabilidad de visión conservada",
           ggtheme = theme_bw())

# Test de Log-Rank: Argón vs Xenón
test_laser <- survdiff(S ~ laser, data = datos_solo_laser)
test_laser


# CALCULOS EXTRA
# Resumen por grupo de tratamiento
tabla_resumen <- datos %>%
  group_by(tratamiento) %>%
  summarise(
    n_eventos = sum(estado),
    tiempo_total = sum(tiempo),
    tasa_riesgo = n_eventos / tiempo_total
  )

print(tabla_resumen)

# Ajuste básico para obtener la RH
m1 <- coxph(S ~ tratamiento, data = datos)

# Extraer la Razón de Hazards (exp(beta))
rh_valor <- exp(coef(m1))
print(paste("La Razón de Hazards es:", round(rh_valor, 3)))

# FUNCION DE RIESGO ACUMULADO
# Estimación del riesgo acumulado por grupo de tratamiento
# (Nelson-Aalen se obtiene a través de la relación con el estimador de KM)
fit_haz <- survfit(S ~ tratamiento, data = datos)

# Graficar la Función de Riesgo Acumulado
ggsurvplot(fit_haz, 
           data = datos,
           fun = "cumhaz",        
           conf.int = TRUE,
           legend.title = "",
           legend.labs = c("No tratado", "Tratado"),
           palette = c("red4", "lightblue4"),
           title = "Función de Riesgo Acumulado (Nelson-Aalen)",
           xlab = "Tiempo (meses)",
           ylab = "Riesgo Acumulado H(t)",
           ggtheme = theme_bw())


#-------------------------------------------------------------------------------------------
#                      SELECCION DEL MODELO                                                  
#-------------------------------------------------------------------------------------------


modelo_completo <- coxph(Surv(tiempo, estado) ~ edad + riesgo*tratamiento_laser , ties = "breslow", data = datos)
modelo_sin_inter <- coxph(Surv(tiempo, estado) ~ edad + riesgo + tratamiento_laser , ties = "breslow", data = datos)


# comparamos el M-completo contra el M-sin int.:

# H0) ß4 = ß5 = 0
anova1 <- anova(modelo_sin_inter, modelo_completo)  # reducido, completo
EP = -2*anova1$loglik[1] - (-2*anova1$loglik[2])  # 2.159319
EP < qchisq(0.95, df = 2) # TRUE
# ==> no rechazamos H0, nos quedamos con el modelo sin interaccion. 


# hi(t) = h0(t).exp(ß1.edad + ß2.riesgo + ß3.TL1 + ß4.TL2)


modelo_sin_trat <- coxph(Surv(tiempo, estado) ~ edad + riesgo , ties = "breslow", data = datos)
modelo_sin_edad <- coxph(Surv(tiempo, estado) ~ riesgo + tratamiento_laser , ties = "breslow", data = datos)
modelo_sin_riesgo <- coxph(Surv(tiempo, estado) ~ edad + tratamiento_laser , ties = "breslow", data = datos)



# comparamos el M-completo contra el M-sin int.:

# H0) ß4 = ß5 = 0
anova1 <- anova(modelo_sin_trat,modelo_sin_inter)  # reducido, completo
EP = -2*anova1$loglik[1] - (-2*anova1$loglik[2])  # 2.159319
EP < qchisq(0.95, df = 1) # FALSE
# ==> rechazamos H0, nos quedamos con el modelo sin interaccion con tratamiento 



anova2 <- anova(modelo_sin_edad,modelo_sin_inter)  # reducido, completo
EP2 = -2*anova2$loglik[1] - (-2*anova2$loglik[2])  # 2.159319
EP2 < qchisq(0.95, df = 1) # TRUE
# ==> no rechazamos H0, nos quedamos con el modelo sin interaccion sin edad !!



anova3 <- anova(modelo_sin_riesgo,modelo_sin_inter)  # reducido, completo
EP3 = -2*anova3$loglik[1] - (-2*anova3$loglik[2])  # 2.159319
EP3 < qchisq(0.95, df = 1) # FALSE
# ==> rechazamos H0, nos quedamos con el modelo sin interaccion con riesgo 



# hi(t) = h0(t).exp( ß1.riesgo + ß2.TL1 + ß3.TL2)

modelo2_sin_trat <- coxph(Surv(tiempo, estado) ~ riesgo , ties = "breslow", data = datos)
modelo2_sin_riesgo <- coxph(Surv(tiempo, estado) ~ tratamiento_laser , ties = "breslow", data = datos)

anova4 <- anova(modelo2_sin_trat, modelo_sin_edad) 
EP4 = -2*anova4$loglik[1] - (-2*anova4$loglik[2])  
EP4 < qchisq(0.95, df = 1) # FALSE 
# rechzao, me qeudo con trat. 


anova(modelo2_sin_riesgo, modelo_sin_edad) #---> rechazo, me quedo con riesgo

# MODELO FINAL: # hi(t) = h0(t).exp( ß1.riesgo + ß2.TL1 + ß3.TL2)

modelo_final <- modelo_sin_edad


# LINEALIDAD
#               r1  r2  r3  
# Hb | =< 8   | 1   0    0 --> referencia
#    | [9-10] | 2   1    0  
#    | 11 >=  | 3   0    1  


datos <-  datos %>% 
  mutate(
    r1 = case_when(riesgo <= 8 ~ 1,
                   riesgo > 8 & riesgo <= 10 ~ 2,
                   riesgo >= 11 ~ 3),
    # Variables dummies
    r2 = case_when(riesgo <= 8 | riesgo >= 11 ~ 0,
                   riesgo > 8 & riesgo <= 10 ~ 1),
    
    r3 = case_when(riesgo <= 10 ~ 0,
                   riesgo >= 11 ~ 1))


modelo_dummies <- coxph(Surv(tiempo, estado) ~ tratamiento_laser + r2 + r3, ties = "breslow", data = datos)
modelo_orden <- coxph(Surv(tiempo, estado) ~ tratamiento_laser + r1, ties = "breslow", data = datos)

# H0) el efecto de riesgo sobre el evento de interes es lineal (sin dummies) 
# H1) ... no lineal
anova(modelo_dummies, modelo_orden)
# p_values no significativo ---> no rechazo H0, el efecto de riesgo es lineal. 


#-------------------------------------------------------------------------------------------
#                      Evalucacion de supuestos                                             
#-------------------------------------------------------------------------------------------

# Test global e individual de proporcionalidad
test_proporcionalidad <- cox.zph(modelo_final)
print(test_proporcionalidad)

# Graficar los residuos para ver el comportamiento en el tiempo

graficos_supuestos <- ggcoxzph(test_proporcionalidad)

#  Modificar el gráfico de Tratamiento Láser 
graficos_supuestos[[2]] <- graficos_supuestos[[2]] + 
  labs(
    title = "Verificación del supuesto de Hazards Proporcionales",
    subtitle = "Residuos de Schoenfeld para Tratamiento Láser",
    x = "Tiempo (meses)",
    y = "Beta Tratamiento Láser"
  )

# Modificar el gráfico de Riesgo 
graficos_supuestos[[1]] <- graficos_supuestos[[1]] + 
  labs(
    title = "Verificación del supuesto de Hazards Proporcionales",
    subtitle = "Residuos de Schoenfeld para Puntaje de Riesgo",
    x = "Tiempo (meses)",
    y = "Beta Riesgo"
  )

graficos_supuestos


# Interpretacion de las Razones de Hazards                             


# Cambio la referencia de Argon a No Tratado
# "No tratado" sea la categoría de referencia 
datos$tratamiento_laser <- relevel(factor(datos$tratamiento_laser), ref = "No tratado")

# Volvemos a ajustar el modelo definitivo con el factor modificado
modelo_final_ref <- coxph(Surv(tiempo, estado) ~ riesgo + tratamiento_laser, 
                       ties = "breslow", data = datos)

summary(modelo_final_ref)

# Pacientes con riesgo = 9

datos_R9 <- data.frame(
  riesgo = c(9,9,9),
  tratamiento_laser = factor(
    c("No tratado","Argon","Xenon"),
    levels = levels(datos$tratamiento_laser)
  )
)
fit_R9 <- survfit(
  modelo_final,
  newdata = datos_R9
)

ggsurvplot(
  fit_R9,
  data = datos_R9,
  conf.int = FALSE,
  legend.title = "",
  legend.labs = c("No tratado","Argon","Xenon"),
  palette = c("green4","red4","lightblue4"),
  title = "Curvas de Supervivencia Ajustadas (Riesgo = 9)",
  xlab = "Tiempo (meses)",
  ylab = "Probabilidad de visión conservada",
  ggtheme = theme_bw()
)

summary(fit_R9, times = c(12,30,60))

