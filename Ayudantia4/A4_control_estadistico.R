##############################################################
### Ayudantia 4 - Control Estadístico 
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
#install.packages("readstata13") #lectura/importar de bases de datos en stata
#install.packages("stargazer") #Paquete que nos permite visualizar regresiones
#install.packages("skimr") #Paquete de exploración de datos

##Cargar librerías
library(dplyr)
library(car)
library(readstata13) #Leer base de datos en formato stata
library(stargazer) #Paquete que nos permite visualizar regresiones
library(skimr)
#search() #Revisamos los paquetes y herramientas instaladas
options(scipen=999) #Desactivamos la notación científica

##############################################################
##############################################################

#1.Carge los datos `psu_sample` y mantenga solamente las variables 
#necesarias para responder a las hipótesis

#Importamos la base que está en formato dta con el paquete readstata13
psu <- read.dta13("psu_sample.dta", convert.factors = T)
#Seleccionamos las variables:
psu <- subset(psu, select = c(mate,x_nem,educpadre,educmadre,leng))
head(psu)

##############################################################
##############################################################

#2. Revise el comportamiento de sus datos. ¿Qué rango de valores ocupan? 
#¿Qué observaciones puede extraer a partir de esta descripción?.

#Obtenemos un summary con las principales variables de la base filtrada
summary(psu)
#También a modo exploratorio podríamos usar este paquete de trabajo
skim_with(numeric = list(missing=NULL, complete=NULL),factor = list(missing=NULL, complete=NULL), integer = list(missing=NULL, complete=NULL))
skim(psu) %>% pander()
#Podríamos también revisar los histogramas
hist(psu$x_nem, 
     main="Figura 1 Histograma Notas de enseñanza media",
     ylab = "Frecuencia",
     xlab="Promedio NEM",
     border="black",
     col = "gray",
     xlim=c(40,70), 
     ylim=c(0,2000),
     las=1,
     breaks=20)

hist(psu$mate, 
     main="Figura 2 Histograma PSU Matemáticas",
     ylab = "Frecuencia",
     xlab="Puntaje PSU",
     border="black",
     col = "gray",
     xlim=c(100,850), 
     ylim=c(0,2000),
     las=1,
     breaks=20)

##############################################################
##############################################################

#3. A partir de la hipótesis principal, se propone que el puntaje
#obtenido en la PSU de Lenguaje es una tercera variable que afecta a 
#la variable independiente. Utilice el método de residualización para 
#controlar la relación por esta nueva variable. 
#¿Por qué sería mejor este método que controlar 
#por un procedimiento de estratificación?

#PASO 1: Notar cuál es la VD y la VI
m1 <- lm(x_nem~leng, data=psu)
summary(m1)
#PASO 2: Extraemos los residuos de la regresión
psu$residuo_nem <- residuals(m1)
#PASO 3: regresión entre la variable dependiente y los residuos
m2 <- lm(mate~residuo_nem, data=psu)
summary(m2)

##############################################################
##############################################################

#4. Interprete el coeficiente de regresión y el R2 no ajustado 
#y compárelos con la estimación de la regresión original. 
#¿Qué problema podría tener el procedimiento de residualización?**?

#Estimamos nuestro modelo 0
m0 <- lm(mate~x_nem, data=psu)
m2 <- lm(mate~residuo_nem, data=psu)

#Comparemos los modelos
stargazer(m0,m2, title = "Comparación de modelos",column.labels=c("Original","Residualización"), type ='text')

##############################################################
##############################################################

#5. Realice un modelo de regresión múltiple del ejercicio anterior, 
#luego proponga un modelo teórico incorporando la educación del padre y estímelo

m3 <- lm(mate~x_nem+leng, data=psu)
stargazer(m2,m3, title = "Comparación de modelos",column.labels=c("Residualización", "Regresión Múltiple"), type ='text')

#Modelo de regresión múltiple (incorporando la educación del padre)
m4 <- lm(mate~x_nem+leng+educpadre, data=psu)
summary(m4)

##############################################################
##############################################################

#6.Interprete los coeficientes de regresión R2 ajustado y no ajustado 
#asociados al modelo anterior.

#Generamos una tabla con el modelo estimado
stargazer(m4, title = "Modelo Regresión Múltiple",column.labels=c("Modelo 1"), type ='text')

##############################################################
##############################################################

#7. Otros investigadores indican que el modelo mejoraría aún más 
#si se agregara a la educación del padre, la educación de la madre y 
#el género de los estudiantes. ¿Cómo podríamos evaluar esto? 
#¿Por qué sería necesario incluir múltiples variables independientes?.

#Borramos los modelos generados en el apartado anterior
rm(list=(ls()[!ls() %in% ("psu")]))
#1. Modelo original 
m1 <- lm(mate~x_nem, data=psu)
#2. Modelo original + educación del padre 
m2 <- lm(mate~x_nem+educpadre, data=psu)
#3. Modelo original + educación de la madre
m3 <- lm(mate~x_nem+educmadre, data=psu)

#4. MODELO FINAL: Modelo original + educación del padre +
# educación de la madre + género del estudiante
m4 <- lm(mate~x_nem+educpadre+educmadre, data=psu)

#Tabla de calidad para presentar regresiones múltiples
stargazer(m1,m2,m3,m4,m5, title = "Comparación de modelos",
          covariate.labels=c("NEM", "Educación Padre", "Educación Madre"),
          column.labels=c("Modelo","Modelo","Modelo","Modelo","Modelo"), type ='html', font.size = "small", align = TRUE, omit.stat=c("f", "ser"), column.sep.width = "-15pt")

##############################################################
##############################################################

#8. ¿Cuáles son los problemas de los coeficientes tal como están? 
#¿Cómo se podría solucionar?

##############################################################
##############################################################

#9. ¿Qué problema tienen los coeficientes estandarizados?

##############################################################
############ Ayudantia 4 - Control Estadístico ###############
##############################################################