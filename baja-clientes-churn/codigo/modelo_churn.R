# ==============================================================================
# Factores asociados a la baja de clientes (churn) en una empresa de telecomunicaciones
# Modelos Lineales Generalizados · Lic. en Estadística (FCEyE, UNR) · 2026
# Autoras: Shirli Kierzkowski y Pía Denipotti
# Ejecutar desde la carpeta codigo/
# ==============================================================================
#------------------------------------------------------------------------------

#                           TRABAJO PRACTICO                                   

#------------------------------------------------------------------------------


library(ggplot2)
library(dplyr)
library(pROC)
library(ResourceSelection)
library(caret)
library(statmod)



datos<-read.csv("../datos/telco_churn.csv", sep = ',')

datos <- datos %>%
  mutate(across(where(is.character), as.factor))%>%mutate(Churn=ifelse(Churn=="Yes",1,0))

summary(datos)

datos_limpio <- datos[datos$tenure > 0, ]   # se excluyen 11 clientes con antigüedad 0 (log(0) no está definido)
nrow(datos_limpio)               # 7032
sum(datos_limpio$Churn)          # 1869 bajas
mean(datos_limpio$Churn)         # 0.266

datos_limpio%>%filter(Churn==1)%>%group_by(Contract)%>% summarise(n=n(), prop=n()/sum(datos_limpio$Churn==1)*100)

# ANALISIS DESCRIPTIVO

# TABLA 1: CLIENTES, % DEL TOTAL Y % DE BAJAS SEGUN CONTRATO E INTERNET
tabla_bajas <- function(variable) {
  n <- table(variable)
  data.frame(Clientes   = as.vector(n),
             Pct_total  = round(100 * as.vector(n) / sum(n), 1),
             Pct_bajas  = round(100 * as.vector(tapply(datos_limpio$Churn, variable, mean)), 1),
             row.names  = names(n))
}
tabla_bajas(datos_limpio$Contract)
#                Clientes Pct_total Pct_bajas
# Month-to-month     3875      55.1      42.7
# One year           1472      20.9      11.3
# Two year           1685      24.0       2.8
tabla_bajas(datos_limpio$InternetService)
#             Clientes Pct_total Pct_bajas
# DSL             2416      34.4      19.0
# Fiber optic     3096      44.0      41.9
# No              1520      21.6       7.4

# ANTIGUEDAD SEGUN BAJA
tapply(datos_limpio$tenure, datos_limpio$Churn, median)   # 0: 38 meses | 1: 10 meses

# FIGURA 1
par(mfrow = c(1, 3), cex = 0.95)
pct_contrato <- 100 * tapply(datos_limpio$Churn, datos_limpio$Contract, mean)
b <- barplot(pct_contrato, names.arg = c("Mes a mes", "Un año", "Dos años"),
             col = "#E8862A", border = NA, ylim = c(0, 50), cex.names = 0.85,
             ylab = "% de bajas", main = "Bajas según tipo de contrato")
text(b, pct_contrato + 2.5, paste0(round(pct_contrato, 1), "%"))

pct_internet <- 100 * tapply(datos_limpio$Churn, datos_limpio$InternetService, mean)
b <- barplot(pct_internet, names.arg = c("DSL", "Fibra óptica", "Sin internet"),
             col = "#3A6EA5", border = NA, ylim = c(0, 50), cex.names = 0.85,
             ylab = "% de bajas", main = "Bajas según servicio de internet")
text(b, pct_internet + 2.5, paste0(round(pct_internet, 1), "%"))

boxplot(tenure ~ factor(Churn, labels = c("No", "Sí")), data = datos_limpio,
        col = c("grey85", "#E8862A"), xlab = "Baja", ylab = "Antigüedad (meses)",
        main = "Antigüedad según baja")
par(mfrow = c(1, 1), cex = 1)


# MODELO 0: ASOCIACION HOMOGENEA: INTERACCIONES DOBLES
modelo0<-glm(Churn~tenure*Contract+tenure*InternetService+Contract*InternetService, data=datos_limpio, family=binomial(link = 'logit'))
summary(modelo0)

# MODELO 01: SOLO INTERACCION ENTRE ANTIGUEDAD Y CONTRATO
modelo01<-glm(Churn~tenure*Contract+InternetService, data=datos_limpio, family=binomial(link = 'logit'))

anova(modelo01, modelo0) # ---> Modelo 01

# MODELO 1: SOLO EFECTOS PRINCIPALES
modelo1<-glm(Churn~tenure+Contract+InternetService, data=datos_limpio, family=binomial(link = 'logit'))
summary(modelo1)

anova(modelo0, modelo1)  # ---> Modelo 01

# PROBAMOS LINEALIDAD
datos_limpio$tenure_cat <- cut(datos_limpio$tenure,
                       breaks = quantile(datos_limpio$tenure, probs = seq(0, 1, 0.2)),
                       include.lowest = TRUE)
datos_limpio$Z <- as.numeric(datos_limpio$tenure_cat)

modelo_dummies <- glm(Churn~tenure_cat*Contract+InternetService, data=datos_limpio, family=binomial(link = 'logit'))
modelo_lineal <- glm(Churn~Z*Contract+InternetService, data=datos_limpio, family=binomial(link = 'logit'))

# H0) Hay linealidad    H1) No hay linealidad
anova(modelo_lineal, modelo_dummies, test='LRT')  # Delta D = 23.177, 9 gl, p-value = 0.005811 => RECHAZO H0 => NO HAY LINEALIDAD


# COMPARACION DE TRANFORMACIONES PARA ANTIGUEDAD
f <- function(term) glm(as.formula(paste("Churn ~", term, "+ Contract + InternetService")),
                        family = binomial, data = datos_limpio)

datos_limpio$tenure_cat <- cut(datos_limpio$tenure, breaks = unique(quantile(datos_limpio$tenure, 0:10/10)),
                               include.lowest = TRUE)

modelos <- list(
  "Lineal"            = f("tenure"),
  "Logarítmica"       = f("log(tenure)"),
  "Polinomio grado 2" = f("poly(tenure, 2)"),
  "Categorizada (10)" = f("tenure_cat")
)

data.frame(
  Modelo     = names(modelos),
  Parametros = sapply(modelos, function(m) length(coef(m))),
  Desvio     = sapply(modelos, deviance),
  AIC        = sapply(modelos, AIC),
  BIC        = sapply(modelos, BIC)
)

### MENOR AIC: logaritmica
### MENOR BIC: logaritmica


# PROBAMOS LA TRANSFORMACION LOGARITMO
modelo02<-glm(Churn~log(tenure)*Contract+InternetService, data=datos_limpio, family=binomial(link = 'logit'))

#SIGNIFICACION DE LOS EFECTOS
summary(modelo02)

# COMPARAMOS CON EL MODELO 01
AIC(modelo01, modelo02)   # modelo01: 6040.576 | modelo02: 5955.703


# SIGNIFICACION DE CADA VARIABLE CON TEST DE RV (CONJUNTO PARA LOS FACTORES)
modelo_SinAntig<-glm(Churn~Contract+InternetService, data=datos_limpio, family=binomial(link = 'logit'))
modelo_SinContrat<-glm(Churn~log(tenure)+InternetService, data=datos_limpio, family=binomial(link = 'logit'))
modelo_SinInter<-glm(Churn~log(tenure)*Contract, data=datos_limpio, family=binomial(link = 'logit'))
modelo_SinIterac<-glm(Churn~log(tenure)+Contract+InternetService, data=datos_limpio, family=binomial(link = 'logit'))


anova(modelo02, modelo_SinAntig)
anova(modelo02, modelo_SinContrat)
anova(modelo02, modelo_SinInter)
anova(modelo02, modelo_SinIterac) 

#### NOS QUEDAMOS CON EL MODELO DE EFECTOS PRINCIPALES Y ANTIGUEDAD TRANSFORMADA
modelo2<-glm(Churn~log(tenure)+Contract+InternetService, data=datos_limpio, family=binomial(link = 'logit'))
summary(modelo2)



# BONDAD DE AJUSTE
hoslem.test(datos_limpio$Churn, fitted(modelo2))   # 7.50, 8 gl, p-value = 0.48


# EVALUAR SI LA FUNCIONDE ENLACE ES LA CORRECTA - H0) gamma = 0
predichos_logit<-predict(modelo2)
datos_limpio$predichos2_logit<-predichos_logit^2
modelo_logit_test <- glm(Churn~log(tenure)+Contract+InternetService+predichos2_logit,
                         data=datos_limpio, family=binomial(link = 'logit'))

summary(modelo_logit_test)  #p-value gamma: 0.939  => ES CORRECTA



# CAPACIDAD PREDICTIVA


# Punto de corte que maximiza el índice de Youden (sensibilidad + especificidad - 1)
corte_youden <- function(y, p) {
  r <- roc(y, p, quiet = TRUE)
  as.numeric(coords(r, "best", best.method = "youden", ret = "threshold")[1, 1])
}

# VALIDACION CRUZADA (K = 10, ESTRATIFICADA)
# En cada partición: se ajusta el modelo y se elige el corte con el
# entrenamiento, y se evalúa en la prueba.

set.seed(123)
K <- 10
datos_limpio$particion <- NA
id_si <- which(datos_limpio$Churn == 1)
id_no <- which(datos_limpio$Churn == 0)
datos_limpio$particion[id_si] <- sample(rep(1:K, length.out = length(id_si)))
datos_limpio$particion[id_no] <- sample(rep(1:K, length.out = length(id_no)))

validacion <- data.frame()
for (k in 1:K) {
  entrenamiento <- datos_limpio[datos_limpio$particion != k, ]
  prueba        <- datos_limpio[datos_limpio$particion == k, ]

  mod_k <- glm(Churn ~ log(tenure) + Contract + InternetService,
               data = entrenamiento, family = binomial(link = 'logit'))

  corte_k <- corte_youden(entrenamiento$Churn, fitted(mod_k))
  p_k     <- predict(mod_k, newdata = prueba, type = "response")
  pred_k  <- as.integer(p_k >= corte_k)

  validacion <- rbind(validacion, data.frame(
    Particion     = k,
    Corte         = corte_k,
    AUC           = as.numeric(auc(roc(prueba$Churn, p_k, quiet = TRUE))),
    Sensibilidad  = mean(pred_k[prueba$Churn == 1] == 1),
    Especificidad = mean(pred_k[prueba$Churn == 0] == 0),
    Error         = mean(pred_k != prueba$Churn)))
}

round(validacion, 3)
round(colMeans(validacion[, -1]), 3)
# Promedios: Corte 0.286 | AUC 0.839 | Sens 0.804 | Espec 0.724 | Error 0.255
range(validacion$Corte)   # 0.276 a 0.292
range(validacion$AUC)     # 0.813 a 0.861


# MODELO FINAL (TODA LA MUESTRA): CURVA ROC Y AUC

y_obs <- modelo2$y            # 1 = baja
p_est <- fitted(modelo2)

Curva.ROC <- roc(y_obs, p_est)
par(pty = "s")
plot(Curva.ROC, print.thres = TRUE, print.auc = TRUE,
     ylab = "Sensibilidad", xlab = "Especificidad",
     col = "darkgreen", lwd = 3, main = "Curva ROC")
par(pty = "m")
auc(Curva.ROC)  # 0.8392


# PUNTO DE CORTE OPTIMO 

coords(Curva.ROC, "best", best.method = "closest.topleft")[1]  #0.2878031
coords(Curva.ROC, "best", best.method = "youden")[1]  #0.2878031



# 4. MATRIZ DE CONFUSION CON ESE CORTE

obs  <- factor(y_obs, levels = c(0, 1))
pred <- factor(as.integer(p_est >= 0.288), levels = c(0, 1))

confusionMatrix(data = pred, reference = obs, positive = "1")

# Confusion Matrix and Statistics
#             Reference
# Prediction    0    1
#            0 3750  369
#            1 1413 1500
# Sensitivity 0.803 | Specificity 0.726 | Accuracy 0.747

# COMPARACION CON EL CORTE 0.5
pred_05 <- factor(as.integer(p_est >= 0.5), levels = c(0, 1))
confusionMatrix(data = pred_05, reference = obs, positive = "1")
# Sensitivity 0.469 | Specificity 0.913


# INTERPRETACION

# RAZONES DE ODDS CON IC 95% (WALD)
coeficientes <- coef(summary(modelo2))
beta <- coeficientes[, "Estimate"]
ee   <- coeficientes[, "Std. Error"]

tabla_RO <- data.frame(Beta = beta, EE = ee,
                       RO = exp(beta),
                       LI = exp(beta - 1.96 * ee),
                       LS = exp(beta + 1.96 * ee))
round(tabla_RO[-1, ], 3)

# RO DE LA ANTIGUEDAD AL DUPLICARLA (k = log 2)
k <- log(2)
round(c(RO = exp(k * beta["log(tenure)"]),
        LI = exp(k * (beta["log(tenure)"] - 1.96 * ee["log(tenure)"])),
        LS = exp(k * (beta["log(tenure)"] + 1.96 * ee["log(tenure)"]))), 3)
# RO = 0.684 | IC 95%: 0.658 a 0.711

# PROBABILIDADES ESTIMADAS PARA ALGUNOS PERFILES
perfiles <- data.frame(
  Perfil = paste0("P", 1:6),
  tenure = c(1, 12, 60, 1, 1, 1),
  Contract = c("Month-to-month", "Month-to-month", "Month-to-month",
               "One year", "One year", "Month-to-month"),
  InternetService = c("Fiber optic", "Fiber optic", "Fiber optic",
                      "DSL", "No", "DSL"),
  stringsAsFactors = FALSE
)

perfiles$Contract        <- factor(perfiles$Contract,        levels = levels(datos_limpio$Contract))
perfiles$InternetService <- factor(perfiles$InternetService, levels = levels(datos_limpio$InternetService))

pred <- predict(modelo2, newdata = perfiles, type = "response", se.fit = F)
round(pred,3)

# GRAFICO: PROBABILIDAD SEGUN ANTIGUEDAD Y CONTRATO (FIBRA OPTICA)
grilla <- expand.grid(tenure = 1:72,
                      Contract = levels(datos_limpio$Contract),
                      InternetService = "Fiber optic")
grilla$prob <- predict(modelo2, newdata = grilla, type = "response")

colores <- c("darkorange", "steelblue", "darkgreen")
plot(NULL, xlim = c(1, 72), ylim = c(0, 1),
     xlab = "Antigüedad (meses)", ylab = "Probabilidad estimada de baja",
     main = "Probabilidad de baja según antigüedad y contrato\n(clientes con fibra óptica)")
for (i in 1:3) {
  g <- grilla[grilla$Contract == levels(datos_limpio$Contract)[i], ]
  lines(g$tenure, g$prob, col = colores[i], lwd = 3)
}
legend("topright", legend = c("Mes a mes", "Un año", "Dos años"),
       col = colores, lwd = 3, bty = "n")

# ANALISIS DE RESIDUOS

set.seed(123)
rQ <- qresid(modelo2)

par(mfrow = c(1, 3))

# Residuos vs. probabilidades estimadas
plot(fitted(modelo2), rQ, pch = 16, cex = 0.4, ylim = c(-4, 4),
     xlab = "Probabilidad estimada", ylab = "Residuos cuantil",
     main = "Residuos vs. probabilidades estimadas")
abline(h = c(-3, 0, 3), lty = c(2, 1, 2))
lines(lowess(fitted(modelo2), rQ), lwd = 2)

# Residuos vs. antigüedad
plot(datos_limpio$tenure, rQ, pch = 16, cex = 0.4, ylim = c(-4, 4),
     xlab = "Antigüedad (meses)", ylab = "Residuos cuantil",
     main = "Residuos vs. antigüedad")
abline(h = c(-3, 0, 3), lty = c(2, 1, 2))
lines(lowess(datos_limpio$tenure, rQ), lwd = 2)

# Gráfico QQ
qqnorm(rQ, pch = 16, cex = 0.4, main = "Gráfico QQ de los residuos cuantil",
       xlab = "Cuantiles teóricos", ylab = "Cuantiles de los residuos")
qqline(rQ)

par(mfrow = c(1, 1))

# Proporción de residuos fuera de la banda de +-3 (esperable bajo normalidad: 0.27%)
100 * mean(abs(rQ) > 3)   # alrededor de 0.3%

# Observaciones influyentes
max(cooks.distance(modelo2))   # 0.005 => No hay obs influyentes

