##############################################################
### Ayudantia 3 - Regresión Simple
##Profesor: Luis Maldonado
##Ayudantes: Catalina Rufs - José Daniel Conejeros
##############################################################

##############################################################
#Ejercicio 
##############################################################
##Configuraciones iniciales
rm(list = ls()) #Limpiamos la memoria
#install.packages("dplyr")#Para manipulación de datos
#install.packages("car")#Funciones para estimar regresiones
#install.packages("haven") #lectura/importar de bases de datos en stata

##Cargar librerías
library(dplyr)
library(car)
library(haven)
#search() #Revisamos los paquetes y herramientas instaladas
options(scipen=999) #Desactivamos la notación científica
##############################################################
##############################################################

#1. Desde el script: Carge los datos psu_sample y mantenga solamente las variables de interés.

#Importamos la base que está en formato dta
psu_sample <- read_dta("psu_sample.dta")
#Transformamos a un marco de datos
psu_sample <- as.data.frame(psu_sample)
#Seleccionamos las variables:
#V. Dependiente: Puntaje en la prueba de matemáticas (mate) 
#V. Independiente: Promedio notas medias (x_nem)
psu_sample <- subset(psu_sample, select = c(mate,x_nem))
head(psu_sample)
#Este es el modelo que nos interesa
#Puntaje matemáticas[i] = B[0] + B[1]*nem[i] + e[i]

##############################################################
##############################################################

#2. Analice con estadística descriptiva univariada ambas variables.
#Solicite un histograma para cada variable y evalúe el tipo de distribución que tienen.

summary(psu_sample$mate)
sd(psu_sample$mate) 
summary(psu_sample$x_nem)
sd(psu_sample$x_nem)

#Histogramas sin especificaciones
hist(psu_sample$mate)
hist(psu_sample$x_nem)

#Figura de calidad
?hist #Revisamos las opciones de la función
hist(psu_sample$mate, 
     main="Fig.1 Histograma PSU de matemáticas", #Nombre del histograma
     ylab = "Frecuencia", #Etiqueta el eje y
     xlab="Puntaje",      #Etiqueta el eje y
     border="black",      #Color del borde de cada barra
     col = "gray",        #Color al interior de cada barra
     xlim=c(200,850),     #Límites del eje x
     ylim=c(0,2500),      #Límites del eje y
     las=1,               #Orientación de los datos del eje x
     breaks=20)           #Quiebre de cada barra

hist(psu_sample$x_nem, 
     main="Fig. 2 Histograma Notas de enseñanza media",
     ylab = "Frecuencia",
     xlab="Promedio NEM",
     border="black",
     col = "gray",
     xlim=c(45,70), 
     ylim=c(0,2000),
     las=1,
     breaks=20)

##############################################################
##############################################################

#3. Formule los modelos de regresión simple teóricos y estimados para la hipótesis presentad
#Describa también los modelos teóricos explicando a qué corresponde cada uno de sus componentes.

#El modelo estimado:
modelo <- lm(mate ~ x_nem, data = psu_sample)

##############################################################
##############################################################

#4. Grafique la relación entre ambas variables. ¿Se observa una asociación?

scatterplot(psu_sample$mate, psu_sample$x_nem)
#General
#?scatterplot #Consultamos la función
scatterplot(psu_sample$x_nem, psu_sample$mate,
            boxplots=FALSE, #quitamos boxplots
            regLine = list(col="red"), #agregamos una línea de asociación
            grid = FALSE, #sacamos el fondo cuadriculado
            smooth=FALSE, #limpiamos de otras estimaciones
            cex = 0,    #Tamaño relativo de cada estimación 
            main = "Fig. 3 Asociación PSU de matemáticas y NEM",
            ylab = "Puntaje PSU de matemáticas",
            xlab = "Promedio NEM") 

#Matriz
scatterplotMatrix(~ mate + x_nem, id=list(n=2), data=psu_sample)
#?scatterplotMatrix #Exploramos la función
scatterplotMatrix(~x_nem + mate, data=psu_sample,
                  regLine = list(col="red"), #agregamos una línea de asociación
                  plot.points = FALSE, #Sacamos cada uno de los valores
                  smooth=FALSE, #limpiamos de otras estimaciones,
                  var.labels=c("Promedio NEM", "Puntaje PSU de matemáticas"), cex.labels = 1,
                  main = "Fig. 4 Asociación PSU de matemáticas y NEM", row1attop = F)


##############################################################
##############################################################

#5. Calcule la correlación entre ambas variables. ¿Cuál es la intensidad y sentido de la asociación?

cor(psu_sample$mate, psu_sample$x_nem, method = "pearson")

##############################################################
##############################################################

#6. Estime el modelo de regresión simple asociados a la hipótesis
#Interprete el coeficiente de regresión, el intercepto y el R2 no ajustado.

#Generamos un objeto con la información de la regresión 
m1 <- lm(mate~x_nem,data=psu_sample)
summary(m1)

#Estimación manual de cada coeficiente
#Coeficiente de regresión 
beta <- cov(psu_sample$x_nem,psu_sample$mate)/var(psu_sample$x_nem)
beta
#Intercepto
mean(psu_sample$mate) - beta*mean(psu_sample$x_nem)
# Modelo is Yi = -230.38 + 13.35*X 

#Ajustes del modelo
#R Cuadrado ajustado: 0.3572
summary(m1)$r.square
#Otra opción
sse = sum((fitted(m1) - mean(psu_sample$mate))^2) # SSE
ssr = sum((fitted(m1) - psu_sample$mate)^2) # SSR
1 - (ssr/(sse + ssr))

#R Cuadrado no ajustado: 0.3571
summary(m1)$adj.r.squared

#Graficamos la regresión 
plot(psu_sample$x_nem, psu_sample$mate,
     col  = "blue", pch = 19, cex = 0.3, lty = "solid", lwd = 2,
     xlim=c(40,70), 
     ylim=c(200,850),
     main = "Fig. 5 Asociación PSU de matemáticas y NEM",
     ylab = "Puntaje PSU de matemáticas",
     xlab = "Promedio NEM")
abline(lm(mate ~ x_nem,data=psu_sample), col="red")

##############################################################
##############################################################

#7. Calcule el valor predicho para un estudiante con el NEM más bajo, 
#para un estudiante en el 50% de la distribución, y para el estudiante con el NEM más alto. 
#Interprete sus resultados.

#Primero identificamos el promedio NEM más bajo
min(psu_sample$x_nem)
#Segundo identificamos el promedio NEM más bajo
median(psu_sample$x_nem)
#Tercero identificamos el promedio NEM más alto
max(psu_sample$x_nem)

#Podemos obtener esta información en una línea de código
summary(psu_sample$x_nem)

#Con esta información estimamos el valor predicho en la PSU de matemáticas para cada promedio:
#Puntaje PSU mate para el promedio NEM más bajo
-230.3834 + 13.3462*41
#Puntaje PSU mate para el promedio NEM del ubicado en el centro de la distribución
-230.3834 + 13.3462*57
#Puntaje PSU mate para el promedio NEM más alto
-230.3834 + 13.3462*70

##############################################################
##############################################################

#8. alcule el valor predicho para todos los estudiantes de la muestra. 
#Solicite descriptivos para esta nueva variable e interprete.

#Generamos una nueva variable (columna) a nuestro marco de datos con los valores predichos
#Manualmente
psu_sample$predict <- -230.3834 + 13.3462*psu_sample$x_nem
#Lo podemos extraer de nuestra regresión
psu_sample$predict <- predict(m1)
#Obtenemos los descriptivos de la variable
mean(psu_sample$predict)
sd(psu_sample$predict)

#Generamos un histograma de calidad
hist(psu_sample$predict, 
     main="Fig. 6 Histograma de valores predichos",
     ylab = "Frecuencia",
     xlab="Puntaje predicho",
     border="black",
     col = "gray",
     xlim=c(350,750), 
     ylim=c(0,2000),
     las=1,
     breaks=20)

##############################################################
##############################################################

#9. Calcule los residuos de la regresión anterior.
#Calcule la correlación y grafique la relación entre los valores predichos y los residuos
#¿Cómo se explican estos resultados ¿Cuáles son las observaciones de mayor y menor residuo?

#Estimamos los residuos
#Manualmente
psu_sample$residuo <- psu_sample$mate - psu_sample$predict
#Lo podemos extraer de nuestra regresión
psu_sample$residuo <- residuals(m1)
#Revisamos nuestra base de datos
head(psu_sample)

#Correlación de valores predichos y residuos
cor(psu_sample$predict, psu_sample$residuo)

#Gráfico entre valores predichos y residuos
plot(psu_sample$predict, psu_sample$residuo,
     col="blue", pch = 19, cex = 0.3, lty = "solid", lwd = 2,
     main = "Fig. 7 entre valores predichos y residuos",
     ylab = "Residuos",
     xlab = "Valores predichos PSU matemáticas")
abline(lm(predict ~ residuo,data=psu_sample), col="red")

#Generamos una nueva columna con los residuos en valor absoluto
min(abs(psu_sample$residuo))
max(abs(psu_sample$residuo))
psu_sample$absres <- abs(psu_sample$residuo)
#Identificamos la observación con menor residuo
minres <- psu_sample %>% 
  filter(absres==min(psu_sample$absres))
#Identificamos la observación con mayor residuo
maxres <- psu_sample %>% 
  filter(absres==max(psu_sample$absres))

#Podemos tener una idea general de nuestros residuos:
?arrange #Nos permite ordenar una base de datos
psu_sample <- arrange(psu_sample, absres)
brief(psu_sample)

##############################################################
##############################################################

#10. Mencione una variable no observada que podría estar sesgando las estimaciones OLS al no ser incluida en el modelo.
#¿Por qué se produciría este sesgo?

##############################################################
############### Ayudantia 3 - Regresión Simple ###############
##############################################################