##############################################################
### Ayudantia 2 - Introducción al análisis con R
##Profesor: Luis Maldonado
##Ayudantes: Catalina Rufs - José Daniel Conejeros
##############################################################

##############################################################
#Ejercicio I: Procesamiento en R
##############################################################
rm(list = ls()) #Limpiamos la memoria
install.packages("dplyr")
install.packages("car")
library(dplyr)#Para manipulación de datos
library(car)#Funciones para estimar regresiones
search() #Revisamos los paquetes y herramientas instaladas

#También podemos ver las funciones de cada paquete
library(help = "base") #Funciones base de R
library(help = "dplyr") #Funciones librería dplyr
library(help = "car") #Funciones librería car

options(scipen=999) #Desactivamos la notación científica

## 1. Revise su directorio de trabajo actual y genere un nuevo directorio de trabajo.
#En este directorio debe estar disponible la base de datos y el script de la ayudant??a 2.

getwd() # Se obtiene el directorio de trabajo actual
#Directorio Cata
setwd("ruta")
##############################################################
##############################################################

## 2. A partir de su directorio abra la base de datos `psu_sample.Rdata`
load("Ayudantia2/psu_sample.Rdata")
psu_sample <- as.data.frame(psu_sample)

##############################################################
##############################################################

## 3. Explore la base de datos `psu_sample.Rdata`, indique el número de observaciones, variables
#y realice una exploración inicial de la base.

#Veamos nuestro objeto
View(psu_sample)

#Revisamos las dimensiones de la base de datos
dim(psu_sample) #N y variables

#Revisamos las variables de nuestra base de datos
str(psu_sample)

#Nombres de las variables en la base de datos
colnames(psu_sample)

#Revisamos las 10 primeras observaciones
head(psu_sample, n=10)

#Revisión general de la base de datos
brief(psu_sample)

##############################################################
##############################################################

## 4. En esta oportunidad trabajaremos con las variables `sexo`, `ingresofamiliar`, `leng`, `mate` y `edad`.
#Realice un subset con estas variables y mencione que significa cada variable y sus categor??as.

#Consultamos los nombres de nuestras variables
colnames(psu_sample)

#Seleccionamos variablrs
subset_psu_sample <- subset(psu_sample, select = c(sexo,grupo_depend,leng,mate,edad,ingresofamiliar))

#Revisamos las variables de nuestra nueva base de datos
str(subset_psu_sample)

##############################################################
##############################################################

## 5. A partir de su nueva base de datos genere las siguientes variables con sus respectivos etiquetas y atributos.

#a. Una variable que identifique el género de los/as entrevistados/as con valor **0 para las mujeres**
#y valor **1 para los hombres**

#Obtenemos una tabla resumen de nuestra variable
#5505 hombres (código 1) y 6306 mujeres (código 2)
?recode
table(subset_psu_sample$sexo)

#Generamos una nueva variable a partir de la existente
subset_psu_sample$genero <- subset_psu_sample$sexo

#Recodificamos la variable nueva
subset_psu_sample$genero <-recode(subset_psu_sample$genero, rec="1=1; 2=0")

#Clasificamos esta variable como factor de dos categorias (etiquetas de hombre y mujer)
subset_psu_sample$genero <-factor(subset_psu_sample$genero, levels = c(0,1), labels = c("Mujer", "Hombre"))

#Revisamos nuestra variable final
table(subset_psu_sample$genero)
table(subset_psu_sample$sexo)

##############################################################

#b. Genere una variable *dummy* que identifique con valor **0 los colegios particulares y particular subvencionado**
#y con valor **1 para los colegios municipales **

#Aplicamos la misma lógica del ejercicio anterior
#Obtenemos una tabla resumen de nuestra variable
#1548 part.pagado (código 1), 6755 part.subvencionado (código 2) y 3508 municipal (código 3)
str(subset_psu_sample$grupo_depend)
table(subset_psu_sample$grupo_depend)

#Generamos una nueva variable a partir de la existente
subset_psu_sample$dependencia <- subset_psu_sample$grupo_depend

#Recodificamos la variable nueva
subset_psu_sample$dependencia <-recode(subset_psu_sample$dependencia, rec="1:2=0; 3=1")

#Clasificamos esta variable como factor de dos categorias (etiquetas de hombre y mujer)
subset_psu_sample$dependencia <-factor(subset_psu_sample$dependencia, levels = c(0,1), labels = c("Part/Sub", "Municipal"))

#Revisamos nuestra variable final
table(subset_psu_sample$dependencia)

##############################################################
##############################################################

## 6. Filtre su base de datos solo con las observaciones que provengan de colegios municipales
#y sean menores de 20 años.

#Filtramos la base de datos con las dos condiciones al mismo tiempo.
#Operador lógico "&" y Utilizamos una tubería
subset_psu_sample2 <- subset_psu_sample %>%
  filter(edad < 20 & dependencia == "Municipal")

#Revisamos nuestras variables
summary(subset_psu_sample2)

##############################################################
#Ejercicio II: Estadistica Descriptiva e inferencial en R
##############################################################

## 7. A partir de la base original, estime cuál es la proporción de mujeres y hombres de la muestra

#Removemos los objetos creados
rm(subset_psu_sample, subset_psu_sample2)

#Para una variable categorica:
table(psu_sample$sexo)
prop.table(table(psu_sample$sexo))

##############################################################
##############################################################

## 8. Estime la proporción de mujeres que estudia en un colegio municipal

#Para mas de una variable categorica:
table(psu_sample$grupo_depend, psu_sample$sexo)

install.packages("gmodels")
gmodels::CrossTable(psu_sample$grupo_depend, psu_sample$sexo)
gmodels::CrossTable(psu_sample$sexo, psu_sample$grupo_depend)

##############################################################
##############################################################

## 9. Estime ahora la cantidad de hombres y de mujeres, solteros(as) que pertenecen a un colegio privado

ftable(psu_sample$sexo, psu_sample$estado_civil, psu_sample$grupo_depend)

##############################################################
##############################################################

## 10. Calcule el promedio de edad de quienes rindieron la PSU, y los puntajes medios obtenidos en lenguaje, matematicas y ciencia.

#Para variable numerica
mean(edad) #Es un error t??pico que se comete
mean(psu_sample$edad)
mean(psu_sample$leng)
mean(psu_sample$mate)
mean(psu_sample$cien)

##############################################################
##############################################################

## 11. Calcule el promedio del puntaje obtenido en la prueba de lenguaje segun sexo y según grupo de dependencia.
# Luego repita el procedimiento para el promedio del puntaje obtenido en la prueba de matematicas.

#Para variable numerica y categorica
by(psu_sample$leng, psu_sample$sexo, mean)
by(psu_sample$leng, psu_sample$grupo_depend, mean)

by(psu_sample$mate, psu_sample$sexo, mean)
by(psu_sample$mate, psu_sample$grupo_depend, mean)

##############################################################
##############################################################

## 12. Dado el potencial riesgo de outliers, calcule la media del puntaje en matemáticas habiendo borrado el 5% extremo.

#Medidas de posicion central
mean(psu_sample$leng, tr=.05) # trim o tr borra una fraccion de observaciones al final. Sirve para eliminar outliers.

##############################################################
##############################################################

## 13.a Calcule la mediana, la moda, el rango intercuartil, el minimo y el máximo de la distribución del puntaje en matemáticas.

median(psu_sample$mate) #Mediada
install.packages("DescTools")
DescTools::Mode(psu_sample$mate) #Moda

#Posición no central
quantile(psu_sample$mate)
quantile(psu_sample$mate, c(0.25, 0.75)) #la c es opcional

IQR(psu_sample$mate) #Rango intercuartil
IQR(psu_sample$mate)  #cero porque tiene baja dispersion

summary(psu_sample$mate)
summary(psu_sample)

##############################################################

## 13.b Estime la dispersion de los puntajes en matematicas. Además, construya su intervalo de confianza a un 95%. ¿Existen diferencias
# estadisticamente significativas con la prueba de lenguaje?

#Dispersión
var(psu_sample$mate)
sd(psu_sample$mate)

##Intervalos de confianza
sd(psu_sample$mate)
106.63/sqrt(11811) #Error estándar
(106.63/sqrt(11811))*1.96 #Margen de error o error t?pico
mean(psu_sample$mate)-((106.63/sqrt(11811))*1.96)
mean(psu_sample$mate)+((106.63/sqrt(11811))*1.96)

##############################################################

## 13.c Calcule nuevamente la distribucion y dispersion, pero ahora para la variable edad. ¿Qué diferencia notan?
summary(psu_sample$edad)
var(psu_sample$mate)
sd(psu_sample$mate)

CI(psu_sample$edad, ci=0.95)

##############################################################
#Ejercicio III: Graficos en R
##############################################################

## Histogramas
# 14.a Compare las distribuciones de los puntajes de lenguaje, matematicas y ciencia.

#Para variable numerica.
hist(psu_sample$leng)
hist(psu_sample$mate)
hist(psu_sample$cien)

##############################################################
##############################################################
##Boxplots

# 14.b. Revise los outliers en las pruebas de lenguaje, matematicas y ciencia.
boxplot(psu_sample$leng, psu_sample$mate, psu_sample$cien)

##############################################################
##############################################################

## 14.c. Revise la dispersion de puntajes lenguaje comparando entre mujeres y hombres.
boxplot(leng ~ sexo, data=psu_sample)
