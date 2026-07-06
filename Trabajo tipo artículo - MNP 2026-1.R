## MODELOS LINEALES GENERALIZADOS
## Paquetes: 
library(readxl)
library(dplyr)
library(performance)
library(glmtoolbox)
library(statmod)
library(caret)
library(ggplot2)
library(GGally)
library(mgcv)

## Data: ####

data <- read_excel("D:/Descargas/Acoustic_Extinguisher_Fire_Dataset.xlsx")
View(data)

# Para trabajar con esta base de datos se decidio realizar una modificación sobre la 
# misma. Este cambio fue realizado sobre la variable SIZE donde se consideran dos
# categorias de LPG como 6 (gas medio) y 7 (gas completo), pues causan problemas con 
# el tratamiento de la variable. Considere entonces:

data1 <- data %>% filter(!SIZE %in% c(6,7))

# Además de ello, se realiza un cambio de los valores proporcionados por la base de datos,
# ya que en esta se consideran como categorias (1 - 5) los tamaños de la plataforma.
# Por ello, se decide trabajar con los valores en cm de cada una de las bases.

data1$SIZE <- ifelse(data1$SIZE == 1, 7, 
                     ifelse(data1$SIZE == 2, 12, 
                            ifelse(data1$SIZE == 3, 14, 
                                   ifelse(data1$SIZE == 4, 16, 20))))
## data1$SIZE <- as.factor(data1$SIZE)

# NOTA: Se tomó como posibilidad el hecho de considerar el area y no solamente el tamaño de 
# base, pues al ser circular esto podria tener algunos inconvenientes. Vea asi:

data2 <- data1
data2$AREA <- pi*(data2$SIZE/2)^2
data2 <- data2[,-1]

## Gráficas a considerar: ####
df_tab <- as.data.frame(table(data1$STATUS, data1$SIZE))
colnames(df_tab) <- c("STATUS", "SIZE", "Freq")

ggplot(df_tab, aes(x = SIZE, y = Freq, fill = STATUS)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribución de tamaño por estado",
    x = "Tamaño",
    y = "Frecuencia",
    fill = "Estado",
  ) +
  theme_minimal()

df_tab <- as.data.frame(table(data1$STATUS, data1$FUEL))
colnames(df_tab) <- c("STATUS", "FUEL", "Freq")

ggplot(df_tab, aes(x = FUEL, y = Freq, fill = STATUS)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribución de combustible por estado",
    x = "Combustible",
    y = "Frecuencia",
    fill = "Estado",
  ) +
  theme_minimal()

df_tab <- as.data.frame(table(data1$STATUS, data1$DISTANCE))
colnames(df_tab) <- c("STATUS", "DISTANCE", "Freq")

ggplot(df_tab, aes(x = DISTANCE, y = Freq, fill = STATUS)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribución de distancia por estado",
    x = "Distancia",
    y = "Frecuencia",
    fill = "Estado",
  ) +
  theme_minimal()

data1_1 <- data1
data1_1$STATUS <- as.factor(data1_1$STATUS)
ggpairs(data1_1, aes(color = STATUS))

#####################################
## Construcción del modelo: Data 1####
# Se decidió contruir un glm binomnial con función de enlace logit en el que se tiene como
# variable respuesta al estado del fuego (Relacionado a si se apago o no). Considere de esta
# manera un modelo completo donde se consideren no solo las variables sino tambien ciertas interacciones.

fit_comp <- glm(data = data1, STATUS~ FUEL*DISTANCE + FUEL*DESIBEL + 
                  FUEL*AIRFLOW + FUEL*FREQUENCY + FUEL*SIZE, 
                family = binomial(link="logit"))
summary(fit_comp)

# Con respecto a este modelo para data 1 tenemos que:

check_model(fit_comp)

# Con respecto al chequeo del modelo se observa que los intervalos de predicción están correcto y que
# no hay observaciones influyentes. Sin embargo, la distribución de los residuos esta siendo comparada
# contra una uniforme, hay un problema en las colas de los residuos y algunos indicios de multicolinealidad
# (esperable, pues se trabaja con interacciones)

check_overdispersion(fit_comp)
# Que nos muestra que estamos ante un caso de subdispersion

check_distribution(fit_comp)
# Que nos deja ver que lo más probable es que la variable respuesta venga de una bernoulli y que 
# lo mas probable es que los errores vengan de una distribución normal.

# Para la elección del modelo se decidio hacer con el uso de la función bestSubset del paquete glmtoolbox,
# donde el argumento que contiene viene dado por el modelo completo a considerar. Vea de tal manera que:
bestSubset(fit_comp)

# Donde se sugiere que:
fit1 <- glm(data = data1, STATUS~ FUEL + DISTANCE + DESIBEL + AIRFLOW + FREQUENCY +
              SIZE + FUEL*DISTANCE + FUEL*SIZE, 
            family = binomial(link="logit"))
summary(fit1)

# A su vez, por el problema de subdispersión, se decide considerar el siguiente modelo:
fit2 <- glm(data = data1, STATUS~ FUEL + DISTANCE + DESIBEL + AIRFLOW + FREQUENCY +
              SIZE + FUEL*DISTANCE + FUEL*SIZE, 
            family = quasibinomial(link="logit"))
summary(fit2)

# Por ultimo, se sugiere el siguiente modelo
fit3 <- glm(data = data1, STATUS~ FUEL + DISTANCE + DESIBEL + AIRFLOW + FREQUENCY +
              SIZE + FUEL*DISTANCE + FUEL*SIZE + FUEL*DESIBEL, 
            family = binomial(link="logit"))
summary(fit3)

# Comparando el AIC del modelo completo y del modelo 1 se tiene que:
cbind("AIC modelo completo" = AIC(fit_comp), "AIC modelo sugerido" = AIC(fit1))


## Evaluación del modelo (Fit 1): ####
check_model(fit1) ## Problemas en las colas y multicolinealidad (dadas interacciones)

# PRUEBA DE RESIDUOS NORMALES
# Por [1], se sugiere para probar esto, los siguientes graficos:
par(mfrow = c(1,3))
plot(density(resid(fit1, type = "response")))
plot(density(rstandard(fit1, type = "pearson")))
plot(density(rstandard(fit1, type = "deviance")))
# Sin embargo, se hace la aclaración de que para variables de respuesta discreta, se utiliza qresid, que
# está diseñado para que no se meustren patrones extraños, de esta manera se tiene que:
plot(density(qresid(fit1)))

# PRUEBA DE INDEPENDENCIA
# Nuevamente, si trabajamos con estos residuos u otros obtenemos resultados diferentes, sin embargo,
# en este caso se prefiere trabajar con los qresid, las gráficas obtenidas son:
par(mfrow=c(1,2))
scatter.smooth(1:15390, qresid(fit1)) # Sin patrón aparente.
scatter.smooth(1:15390, rstandard(fit1, type = "deviance")) # Con patrones marcados.

# PRUEBA DE PREDICCIÓN
par(mfrow=c(1,1))
plot(density(data1$STATUS))
lines(density(predict(fit1, type = "response")), col = "red")

# Con respecto a la matriz de confusión y medidas de precisión se tiene que:
pred <- predict(fit1, type = "response")
x<-ifelse(pred > 0.5,1,0)
table(x)
cof1 = confusionMatrix(as.factor(x),as.factor(data1$STATUS), positive = "1") # Accuracy de 0.9062
cof1 = cof1$overall
cof1 = cof1[c(1,2)]

## Evaluación del modelo (Fit 2): ####
check_model(fit2) ## Problemas en las colas y multicolinealidad (dadas interacciones)

# PRUEBA DE RESIDUOS NORMALES
# Por [1], se sugiere para probar esto, los siguientes graficos:
par(mfrow = c(1,3))
plot(density(resid(fit2, type = "response")))
plot(density(rstandard(fit2, type = "pearson")))
plot(density(rstandard(fit2, type = "deviance")))
# Sin embargo, se hace la aclaración de que para variables de respuesta discreta, se utiliza qresid, que
# está diseñado para que no se meustren patrones extraños, de esta manera se tiene que:
par(mfrow=c(1,1))
plot(density(qresid(fit2))) ## Note que aún así se nota un gran nivel de apuntamiento

# PRUEBA DE INDEPENDENCIA
# Nuevamente, si trabajamos con estos residuos u otros obtenemos resultados diferentes, sin embargo,
# en este caso se prefiere trabajar con los qresid, las gráficas obtenidas son:
par(mfrow=c(1,2))
scatter.smooth(1:15390, qresid(fit2)) # Con patrones marcados
scatter.smooth(1:15390, rstandard(fit2, type = "deviance")) # Con patrones marcados.

# PRUEBA DE PREDICCIÓN
par(mfrow = c(1,1))
plot(density(data1$STATUS))
lines(density(predict(fit2, type = "response")), col = "red")

# Con respecto a la matriz de confusión y medidas de precisión se tiene que:
pred <- predict(fit2, type = "response")
x<-ifelse(pred > 0.5,1,0)
table(x)
cof2 = confusionMatrix(as.factor(x),as.factor(data1$STATUS), positive = "1") # Accuracy de 0.9051
cof2 = cof2$overall
cof2 = cof2[c(1,2)]

## Evaluación del modelo (Fit 3): ####
check_model(fit3) ## Problemas en las colas y multicolinealidad (dadas interacciones)

# PRUEBA DE RESIDUOS NORMALES
# Por [1], se sugiere para probar esto, los siguientes graficos:
par(mfrow = c(1,3))
plot(density(resid(fit3, type = "response")))
plot(density(rstandard(fit3, type = "pearson")))
plot(density(rstandard(fit3, type = "deviance")))
# Sin embargo, se hace la aclaración de que para variables de respuesta discreta, se utiliza qresid, que
# está diseñado para que no se meustren patrones extraños, de esta manera se tiene que:
par(mfrow = c(1,1))
plot(density(qresid(fit3)))

# PRUEBA DE INDEPENDENCIA
# Nuevamente, si trabajamos con estos residuos u otros obtenemos resultados diferentes, sin embargo,
# en este caso se prefiere trabajar con los qresid, las gráficas obtenidas son:
par(mfrow=c(1,2))
scatter.smooth(1:15390, qresid(fit3)) # Sin patrón aparente.
scatter.smooth(1:15390, rstandard(fit3, type = "deviance")) # Con patrones marcados.

# PRUEBA DE PREDICCIÓN
par(mfrow=c(1,1))
plot(density(data1$STATUS))
lines(density(predict(fit3, type = "response")), col = "red")

# Con respecto a la matriz de confusión y medidas de precisión se tiene que:
pred <- predict(fit3, type = "response")
x<-ifelse(pred > 0.5,1,0)
table(x)
cof3 = confusionMatrix(as.factor(x),as.factor(data1$STATUS), positive = "1") # Accuracy de 0.9051
cof3 = cof3$overall
cof3 = cof3[c(1,2)]

##################################### 
## Construcción del modelo: Data 2 ####
fit_comp2 <- glm(data = data2, STATUS~ FUEL*DISTANCE + FUEL*DESIBEL + 
                   FUEL*AIRFLOW + FUEL*FREQUENCY + FUEL*AREA, 
                 family = binomial(link="logit"))
summary(fit_comp2)

# Chequeo del modelo
check_model(fit_comp2)

# Chequeo de sobredispersion
check_overdispersion(fit_comp2)

# Mejor modelo
bestSubset(fit_comp2)

# De tal forma:
fit4 <- glm(data = data2, STATUS ~ DISTANCE + DESIBEL + AIRFLOW +
              FREQUENCY + AREA + FUEL*DISTANCE + FUEL*AREA + FUEL*FREQUENCY, 
            family = binomial(link="logit"))
summary(fit4)

## Evaluación del modelo (Fit 4): ####
check_model(fit4) ## Problemas en las colas y multicolinealidad (dadas interacciones)

# PRUEBA DE RESIDUOS NORMALES
# Por [1], se sugiere para probar esto, los siguientes graficos:
par(mfrow = c(1,3))
plot(density(resid(fit4, type = "response")))
plot(density(rstandard(fit4, type = "pearson")))
plot(density(rstandard(fit4, type = "deviance")))
# Sin embargo, se hace la aclaración de que para variables de respuesta discreta, se utiliza qresid, que
# está diseñado para que no se meustren patrones extraños, de esta manera se tiene que:
par(mfrow = c(1,1))
plot(density(qresid(fit4)))

# PRUEBA DE INDEPENDENCIA
# Nuevamente, si trabajamos con estos residuos u otros obtenemos resultados diferentes, sin embargo,
# en este caso se prefiere trabajar con los qresid, las gráficas obtenidas son:
par(mfrow=c(1,2))
scatter.smooth(1:15390, qresid(fit4)) # Sin patrón aparente.
scatter.smooth(1:15390, rstandard(fit4, type = "deviance")) # Con patrones marcados.

# PRUEBA DE PREDICCIÓN
par(mfrow=c(1,1))
plot(density(data2$STATUS))
lines(density(predict(fit4, type = "response")), col = "red")

# Con respecto a la matriz de confusión y medidas de precisión se tiene que:
pred <- predict(fit4, type = "response")
x<-ifelse(pred > 0.5,1,0)
table(x)
cof4 = confusionMatrix(as.factor(x),as.factor(data1$STATUS), positive = "1") # Accuracy de 0.9042
cof4 = cof4$overall
cof4 = cof4[c(1,2)]

##################################### 
## Construcción del modelo: GAM ####
## El mejor modelo conseguido a traves de este tipo de modelos es:
fit5 <- gam(STATUS ~ s(SIZE, k = 4, bs = 'ps') + 
              s(DISTANCE, k = 12, bs = 'ps') + 
              s(DESIBEL, bs = 'ps') + 
              s(AIRFLOW, k = 15, bs = 'ps') +
              s(FREQUENCY, bs = 'ps') +
              FUEL,
            family = binomial(link="logit"),
            data = data1,
            method = 'REML')
summary(fit5)

set.seed(123)
par(mfrow = c(2,2))
gam.check(fit5)

# PRUEBA DE PREDICCIÓN
# Con respecto a la matriz de confusión y medidas de precisión se tiene que:
pred <- predict(fit5, type = "response")
x<-ifelse(pred > 0.5,1,0)
table(x)
cof5 = confusionMatrix(as.factor(x),as.factor(data1$STATUS), positive = "1") # Accuracy de 0.9409
cof5 = cof5$overall
cof5 = cof5[c(1,2)]

## COMPARACION ####
comp = data.frame(
  fit1 = round(cof1*100, 4),
  fit2 = round(cof2*100, 4),
  fit3 = round(cof3*100, 4),
  fit4 = round(cof4*100, 4),
  fit5 = round(cof5*100, 4)
)
comp

concurvity(fit5, full = TRUE)

plot(fit5, pages = 1, shade = TRUE)


################################################################################

# Realizando modificaciones al modelo 5 para lograr corregir la concurvidad
# del modelo al retirar variables y conseguir un modelo más parsimonioso.


# Alternativa visual moderna
library(mgcViz)
gv <- getViz(fit5)
plot(gv, allTerms = TRUE) # Esto te graficará las curvas reales de cada variable


##:: Para corregir los problemas de concurvidad se retiran las variables desibel
##:: y Airflow.

fit6 <- gam(STATUS ~ s(SIZE, k = 4, bs = 'ps') + 
              s(DISTANCE, k = 12, bs = 'ps') +
              s(FREQUENCY, k = 6, bs = 'ps') +
              FUEL,
            family = binomial(link="logit"),
            data = data1,
            method = 'REML')
summary(fit6)

par(mfrow = c(2,2))

set.seed(123)
gam.check(fit6)

concurvity(fit6, full = TRUE)

plot(fit6, pages = 1, shade = TRUE)

## PRUEBA DE PREDICCIÓN ####

pred <- predict(fit6, type = "response")
x<-ifelse(pred > 0.5,1,0)
table(x)
cof6 = confusionMatrix(as.factor(x),as.factor(data1$STATUS), positive = "1") # Accuracy de 0.9409
cof6 = cof6$overall
cof6 = cof6[c(1,2)]

## VERIFICACION DE RESIDUOS ####

par(mfrow = c(1,2))
stats::qqnorm(qresid(fit6))
abline(0,1, col = "#1B0A8A", lwd = 2)
hist(qresid(fit6), main = "Histograma", xlab = "Residuos cuantílicos",
     col = "#7061D4",
     border = "#1B0A8A")

