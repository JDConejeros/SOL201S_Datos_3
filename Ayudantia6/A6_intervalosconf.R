#################################################################
#Ayudantia 6 - Intervalos de confianza y restricciones múltiples#
##Profesor: Luis Maldonado
##Ayudantes: Catalina Rufs - José Daniel Conejeros
#################################################################

#################################################################
#Ejercicio 
#################################################################
##Configuraciones iniciales
rm(list = ls()) #Limpiamos la memoria
#Instalamos liberías de trabajo:
#install.packages("dplyr")#Para manipulación de datos
#install.packages("car")#Funciones para estimar regresiones
#install.packages("readstata13") #lectura/importar de bases de datos en stata
#install.packages("stargazer") #Paquete que nos permite visualizar regresiones
#install.packages("skimr") #Paquete de exploración de datos
#install.packages("texreg") #visualizar regresiones

##Cargar librerías:
library(dplyr)
library(car)          
library(readstata13)  
library(stargazer)  
library(skimr)      

options(scipen=999) #Desactivamos la notación científica

#################################################################
#################################################################

#1. Seleccione las variables con las cuales va a trabajar, 
#realice los ajustes que considere necesarios y observe los 
#valores posibles en las variables de interés

#Abrimos nuestra base de datos 
casen <- read.dta13("extracto_casen_2017.dta", convert.factors = TRUE)
#Exploramos nuestra base de datos 
#str(casen)
head(casen)
#skim(casen)
#Seleccionamos las variables que nos interensan para las hipótesis de investigación
casen2 <- subset(casen, select = c(ytrabajocor, esc, edad, o10, numper))
#Revisamos
head(casen2)
skim_with(numeric = list(missing=NULL, complete=NULL),factor = list(missing=NULL, complete=NULL), integer = list(missing=NULL, complete=NULL))
#Salario (Variable dependiente)
skim(casen2[1]) %>% pander()
#Variables Independientes 
skim(casen2[2:5]) %>% pander()

#################################################################
#################################################################

#2. Estime una matriz de correlación entre las variables de interés
#¿Qué se observa entre las variables independientes?
#¿Qué puede decirse preliminarmente respecto a nuestras hipótesis

cor <- cor(casen2)
cor <- round(cor,2)
#Cambiamos los nombres
colnames(cor) <- c("Ingreso", "Escolaridad", "Edad", "Horas", "N del hogar")
rownames(cor) <- c("Ingreso", "Escolaridad", "Edad", "Horas", "N del hogar")
cor[upper.tri(cor)] <- ""
#Tabla de correlaciones

cor %>% pander()

#################################################################
#################################################################

#3. 3. Estime un modelo de regresión simple prediciendo el
#ingreso declarado a partir de los años de escolaridad de los 
#entrevistados. Luego, incorpore una a una las variables 
#independientes restantes. ¿Qué se puede concluir para las hipótesi

#Despejamos el environment
rm(cor)
#1. Modelo para Ingreso y años de escolaridad
m1 <- lm(ytrabajocor~esc, data=casen2)
#2. Modelo para Ingreso y años de escolaridad más edad
m2 <- lm(ytrabajocor~esc + edad, data=casen2)
#3. Modelo para Ingreso y años de escolaridad más edad y horas trabajadas
m3 <- lm(ytrabajocor~esc + edad + o10, data=casen2)
#4. Modelo para Ingreso y años de escolaridad más edad, horas trabajadas y personas en el hogar
m4 <- lm(ytrabajocor~esc + edad + o10 + numper, data=casen2)

#Tabla de calidad para presentar nuestras regresiones
stargazer(m1,m2,m3,m4, title = "Modelos de regresión para ingreso",
          covariate.labels=c("Años escolaridad", "Edad", "Horas trabajadas", "N del hogar"),
          column.labels=c("Modelo","Modelo","Modelo","Modelo","Modelo"), type ='text',
          dep.var.caption  = "Variable Dependiente",
          dep.var.labels   = "Ingreso declarado",
          font.size = "small", align = TRUE, omit.stat=c("f", "ser"), column.sep.width = "30pt")


#################################################################
#################################################################

#4.  Formule un modelo de regresión múltiple teórico para el 
#modelo 4 de la pregunta anterior. Grafique e interprete los 
#coeficientes de dicha regresión. 
#¿Son estadísticamente significativos? 
#¿Qué se puede decir respecto a cada hipótesi

#Graficamos nuestras estimaciones
#install.packages("sjPlot") #Para graficar nuestros coeficientes
library(sjPlot)
plot_model(m4, sort.est=TRUE, show.values=TRUE, vline.color="black", 
           title = "Figura 1: Modelo 4 para ingreso declarado (Coeficientes NO estandarizados)", 
           axis.labels = c("N hogar", "Horas de trabajo", "Edad", "Años escolaridad"),  
           dot.size = 1, width=0.1, axis.title = "Coeficiente")

#Coeficientes estandarizados
plot_model(m4, sort.est=TRUE, show.values=TRUE, vline.color="black", 
           title = "Figura 2: Modelo 4 para ingreso declarado (Coeficientes estandarizados)", 
           axis.labels = c("N hogar", "Horas de trabajo", "Edad", "Años escolaridad"), 
           dot.size = 1, type="std",width=0.1, axis.title = "Coeficiente")

#################################################################
#################################################################

#5. En base al modelo 4 estime el intervalo de confianza para 
#el coeficiente de años de escolaridad a un 90%, 95% y 99% de 
#nivel de confianza. ¿Qué puede observar según la hipótesis 
#de investigación?

#Limpiamos el enviroment
rm(m1,m2,m3)

#Extraemos los elementos del modelo 4
#Coeficiente de años de escolaridad
beta1 <- m4[["coefficients"]][["esc"]]
beta1
#Error estándar
se <- sqrt(diag(vcov(m4)))
se <- se[2]
se

#IC 90% Niv. Confianza
beta1 - 1.645*se #IC Izquierdo
beta1 + 1.645*se #IC Derecho

#IC 95% Niv. Confianza
beta1 - 1.960*se #IC Izquierdo
beta1 + 1.960*se #IC Derecho

#IC 99% Niv. Confianza
beta1 - 2.576*se #IC Izquierdo
beta1 + 2.576*se #IC Derecho

#################################################################
#################################################################

#6. Los modelos estimados no incluyen la variable género. 
#¿Qué consecuencias puede tener esto para las estimaciones?
#Estime un modelo completo incluyendo dicha variable

#Despejamos el environment
rm(beta1,se,m4,casen2)
#Objeto con las variables incluídas el género
casen2 <- subset(casen, select = c(ytrabajocor, esc, edad, o10, numper, sexo))
#Codificamos el sexo como factor
casen2$genero[casen2$sexo==1] <- 0
casen2$genero[casen2$sexo==2] <- 1
casen2$genero <-factor(casen2$genero, levels = c(0,1), labels = c("Hombre", "Mujer"))
#Estimamos la regresión 
m1 <- lm(ytrabajocor~esc + edad + o10 + numper+sexo, data=casen2)

#Tabla de calidad para presentar nuestras regresiones
stargazer(m1, title = "Modelos de regresión para ingreso",
          covariate.labels=c("Años escolaridad", "Edad", "Horas trabajadas", "N del hogar", "Mujer"),
          column.labels=c("Modelo"), type ='text',dep.var.caption  = "Variable Dependiente",dep.var.labels   = "Ingreso declarado",
          font.size = "small", align = TRUE, omit.stat=c("f", "ser"), column.sep.width = "30pt")


#################################################################
#################################################################

#7. Formule una restricción lineal que permita identificar si
#las variable años de escolaridad tiene una asociación con el ingreso declarado

#Estimamos la regresión
m4 <- lm(ytrabajocor~esc + edad + o10 + numper, data=casen2)

#Para formular restricciones lineales
car::linearHypothesis(m4, c("esc=0"))

#################################################################
#################################################################

#8.  Mediante restricciones lineales, pruebe si la edad y el 
#número de personas del hogar estan asociadas significativamente 
#con el ingreso declarado

#Para formular restricciones lineales
car::linearHypothesis(m4, c("edad=0", "numper=0"))

#################################################################
#Ayudantia 6 - Intervalos de confianza y restricciones múltiples#
#################################################################
