#################################################################
#Ayudantia 7 - Preparación ejercicio 2###########################
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
#install.packages("haven") #lectura/importar de bases de datos en stata
#install.packages("stargazer") #Paquete que nos permite visualizar regresiones
#install.packages("skimr") #Paquete de exploración de datos
#install.packages("texreg") #visualizar regresiones

##Cargar librerías:
library(dplyr)
library(car)          
library(haven)  
library(stargazer)  
library(skimr)      

options(scipen=999) #Desactivamos la notación científica

#################################################################
#################################################################

#1.Realice las siguientes tareas de procesamiento con la base de datos:

#a. Renombre las variables independientes.
#Abrimos nuestra base de datos (dejamos en formato data.frame)
base <- as.data.frame(read_dta("Ejercicio2_2018.dta"))
#Exploramos nuestra base de datos 
#str(base)
head(base)
#Renombramos las variables independientes
names(base)[names(base) == "V9"] <- "igualitarismo"
names(base)[names(base) == "income_dc"] <- "ingreso"
names(base)[names(base) == "AGE"] <- "edad"
names(base)[names(base) == "DEGREE"] <- "educacion"
names(base)[names(base) == "V4"] <- "pais"
head(base)

#################################################################

#b. Invertir los atributos de los indicadores de la variable dependiente 
#y codificar casos pérdidos.

#Observamos los indicadores de la variable dependiente
table(base$V16)
table(base$V17)
table(base$V17)
table(base$V18)

#Recodificamos las categorías como missing: 
#8=NS-NR & 9=Sin datos
base$V16[base$V16 == 8 | base$V16 == 9] <- NA
base$V17[base$V17 == 8 | base$V17 == 9] <- NA
base$V18[base$V18 == 8 | base$V18 == 9] <- NA
base$V19[base$V19 == 8 | base$V19 == 9] <- NA

#Corroboramos los missing
skim(base, 3:6)  %>% pander() # Codificaciones OK. 

#Invertimos las escalas: fuertemente en contra a fuertemente a favor
base$v16 <- 6 - base$V16
base$v17 <- 6 - base$V17
base$v18 <- 6 - base$V18
base$v19 <- 6 - base$V19

#Corroboramos
table(base$V16, base$v16)  %>% pander()
table(base$V17, base$v17)  %>% pander()
table(base$V18, base$v18)  %>% pander()
table(base$V19, base$v19)  %>% pander()

#################################################################

#c. Construir un indicador sumativo de la variable dependiente.

#Construímos nuestro indicador sumativo: 
#Para que la escala parto desde el valor 1
base$fondos_public <- (base$v16 + base$v17 + base$v18 + base$v19) - 3 
#Codificamos como variable numérica
base$fondos_public <- as.numeric(base$fondos_public)
#Miramos nuestro indicador final
skim_with(numeric = list(missing=NULL, complete=NULL),factor = list(missing=NULL, complete=NULL), integer = list(missing=NULL, complete=NULL))
skim(base$fondos_public)  %>% pander()

#################################################################

#d. Limpie la base de datos: considere solo las variables a utilizar 
#y elimine los casos pérdidos

#Seleccionamos las variables del estudio
base <- subset(base, select = c(pais, fondos_public, igualitarismo, ingreso, edad, educacion))
head(base)
#Codificamos missing de las otras variables 
#table(base$igualitarismo) #Escala de 1 al 5, missing: 8 y 9
#table(base$ingreso) #Escala de 1 a 10
#table(base$edad) #Missing 999
#table(base$educacion) #Missing 9
base$edad[base$igualitarismo == 8 | base$igualitarismo == 9] <- NA
base$edad[base$edad == 999] <- NA
base$educacion[base$educacion == 9] <- NA
#Convertimos todas nuestras variables en numéricas
base$edad <- as.numeric(base$edad)
base$educacion <- as.numeric(base$educacion)
base$igualitarismo <- as.numeric(base$igualitarismo)
#Eliminamos los missing
base <- na.omit(base)
#Revisamos la base completa
skim(base)  %>% pander()

#################################################################
#################################################################

#2. Estime el modelo de regresión OLS propuesto para las siguientes 
#muestras y reporte sus resultados en un tabla de calidad

#Modelo para todos los países
m1 <- lm(fondos_public~igualitarismo + ingreso + edad + educacion, data=base)
#Modelo para Chile (152)
m2 <- lm(fondos_public~igualitarismo + ingreso + edad + educacion, data=base, subset=(pais==152))
#Modelo para Suecia (752)
m3 <- lm(fondos_public~igualitarismo + ingreso + edad + educacion, data=base, subset=(pais==752))
#Modelo para USA (840)
m4 <- lm(fondos_public~igualitarismo + ingreso + edad + educacion, data=base, subset=(pais==840))

#Tabla
stargazer(m1,m2,m3,m4, title = "Modelos de regresión para apoyo al gasto público en salud",
          covariate.labels=c("Igualitarismo", "Ingreso", "Edad", "Nivel educacional", "Intercepto"),
          column.labels=c("Todos los paises","Chile","Suecia","USA"), type ='text',
          dep.var.caption  = "Variable Dependiente",
          dep.var.labels   = "Escala de apoyo",
          font.size = "small", align = TRUE, omit.stat=c("f", "ser"), column.sep.width = "30pt")

#Figura 
#Gráfico 
#install.packages("sjPlot")
library(sjPlot)
all <- plot_model(m1, sort.est=F, show.values=TRUE, vline.color="black", title = "Apoyo de gasto en salud (Todos)",  dot.size = 1, width=0.1, axis.title = "Coeficiente")

chile <- plot_model(m2, sort.est=F, show.values=TRUE, vline.color="black", title = "Apoyo de gasto en salud (Chile)",  dot.size = 1, width=0.1, axis.title = "Coeficiente")

suecia <- plot_model(m3, sort.est=F, show.values=TRUE, vline.color="black", title = "Apoyo de gasto en salud (Suecia)",  dot.size = 1, width=0.1, axis.title = "Coeficiente")

usa <- plot_model(m4, sort.est=F, show.values=TRUE, vline.color="black", title = "Apoyo de gasto en salud (USA)",  dot.size = 1, width=0.1, axis.title = "Coeficiente")

#Revisamos los gráficos
#install.packages("egg")
library(egg)
ggarrange(all, chile, suecia, usa, ncol = 2, nrow = 2)

#################################################################
#################################################################

#3. Interprete los coeficientes y sus significaciones estadísticas
#de ingreso para cada uno de los 4 modelos estimados. 
#En base a sus estimaciones, señale el país en donde el ingres
#o tiene el efecto más gran

#################################################################
#################################################################

#4. Estime los coeficientes estandarizados para igualitarismo e 
#ingreso para los cuatro modelos estimados, señalando cuál variable 
#tendría el efecto más grande en cada modelo

#Generamos nuestras variables en puntaje z
base$fondos_public_z <- scale(base$fondos_public)
base$igualitarismo_z <- scale(base$igualitarismo)
base$ingreso_z <- scale(base$ingreso)
base$edad_z <- scale(base$edad)
base$educacion_z <- scale(base$educacion)

#Estimamos los modelos con coeficientes estandarizados 
m1_std <- lm(fondos_public_z~igualitarismo_z + ingreso_z + edad_z + educacion_z, data=base)
m2_std <- lm(fondos_public_z~igualitarismo_z + ingreso_z + edad_z + educacion_z, data=base, subset=(pais==152))
m3_std <- lm(fondos_public_z~igualitarismo_z + ingreso_z + edad_z + educacion_z, data=base, subset=(pais==752))
m4_std <- lm(fondos_public_z~igualitarismo_z + ingreso_z + edad_z + educacion_z, data=base, subset=(pais==840))

#Tabla de calidad con los coeficientes estandarizados
stargazer(m1_std,m2_std,m3_std,m4_std, title = "Modelos de regresión para apoyo al gasto público en salud (betas estandarizados)",
          covariate.labels=c("Igualitarismo", "Ingreso", "Edad", "Nivel educacional", "Intercepto"),
          column.labels=c("Todos los paises","Chile","Suecia","USA"), type ='text',
          dep.var.caption  = "Variable Dependiente",
          dep.var.labels   = "Escala de apoyo",
          font.size = "small", align = TRUE, omit.stat=c("f", "ser"), column.sep.width = "30pt")

#################################################################
#################################################################

#5.Evalue la hipótesis que dice que el efecto del ingreso es igual 
#al efecto del igualitarismo. Para ello, realice los respectivos 
#tests de restricciones múltiples, señalando hipótesis nula e 
#hipótesis alternativa. Realice los test para los datos de Chile, 
#EEUU y Suecia. ¿Observa alguna diferencia?

car::linearHypothesis(m2, c("ingreso=igualitarismo"))
car::linearHypothesis(m3, c("ingreso=igualitarismo"))
car::linearHypothesis(m4, c("ingreso=igualitarismo"))


#################################################################
#################################################################

#6.  Para el coeficiente de regresión de la variable igualitarismo,
#estime e interprete intervalos de confianza al 89%, 56% y 13%. 
#Estime intervalos solo con procedimiento analítico y solo para la 
#base con todos los países. Interprete los intervalos

#Limpiamos el enviroment
rm(m2,m3,m4)

#Extraemos los elementos del modelo 1
#Coeficiente de a?os de igualitarismo
beta1 <- m1[["coefficients"]][["igualitarismo"]]
beta1
#Error est?ndar
se <- sqrt(diag(vcov(m1)))
se <- se[2]
se

#89%
beta1 - 1.600*se #IC Izquierdo
beta1 + 1.600*se #IC Derecho

#56%
beta1 - 0.770*se #IC Izquierdo
beta1 + 0.770*se #IC Derecho

#13%
beta1 - 0.164*se #IC Izquierdo
beta1 + 0.164*se #IC Derecho

#################################################################
#################################################################

#7. Estime la predicción de apoyo a gasto público para una persona 
#que tiene el máximo nivel de igualitarismo y pertenece al grupo 
#de ingreso más bajo. Deje constante el valor de edad y educación 
#en sus respectivos promedios. Utilice solo los datos para Chile.

base_chile <- subset(base,pais==152)
igualitarismo_max152 <- max(base_chile$igualitarismo)
ingreso_min152 <- min(base_chile$ingreso)
edad_prom152 <- mean(base_chile$edad)
educ_prom152 <- mean(base_chile$educacion)

beta0 <- m2[["coefficients"]][["(Intercept)"]]
beta0
beta1 <- m2[["coefficients"]][["igualitarismo"]]
beta1
beta2 <- m2[["coefficients"]][["ingreso"]]
beta2
beta3 <- m2[["coefficients"]][["edad"]]
beta3
beta4 <- m2[["coefficients"]][["educacion"]]
beta4

publicfunding_152= beta0 + beta1*igualitarismo_max152 + beta2*ingreso_min152 + beta3*edad_prom152 + beta4*educ_prom152
publicfunding_152

#Suecia
base_suecia <- subset(base,pais==752)
igualitarismo_max752 <- max(base_suecia$igualitarismo)
ingreso_min752 <- min(base_suecia$ingreso)
edad_prom752 <- mean(base_suecia$edad)
educ_prom752 <- mean(base_suecia$educacion)

beta0 <- m3[["coefficients"]][["(Intercept)"]]
beta0
beta1 <- m3[["coefficients"]][["igualitarismo"]]
beta1
beta2 <- m3[["coefficients"]][["ingreso"]]
beta2
beta3 <- m3[["coefficients"]][["edad"]]
beta3
beta4 <- m3[["coefficients"]][["educacion"]]
beta4

publicfunding_752= beta0 + beta1*igualitarismo_max752 + beta2*ingreso_min752 + beta3*edad_prom752 + beta4*educ_prom752
publicfunding_752

#USA
base_usa <- subset(base,pais==840)
igualitarismo_max840 <- max(base_usa$igualitarismo)
ingreso_min840 <- min(base_usa$ingreso)
edad_prom840 <- mean(base_usa$edad)
educ_prom840 <- mean(base_usa$educacion)

beta0 <- m3[["coefficients"]][["(Intercept)"]]
beta0
beta1 <- m3[["coefficients"]][["igualitarismo"]]
beta1
beta2 <- m3[["coefficients"]][["ingreso"]]
beta2
beta3 <- m3[["coefficients"]][["edad"]]
beta3
beta4 <- m3[["coefficients"]][["educacion"]]
beta4

publicfunding_840= beta0 + beta1*igualitarismo_max840 + beta2*ingreso_min840 + beta3*edad_prom840 + beta4*educ_prom840
publicfunding_840

#################################################################
#################################################################

#8. Solo con la muestra para Chile realice un test de White para 
#evaluar si hay problemas de heterocedasticidad (señale: hipótesis 
#nula y alternativa, estadístico de prueba, valor p estimado, decisión). 
#¿Será necesario utilizar errores estándares robustos?

install.packages("lmtest")
library(lmtest)
bptest(m1) %>% pander()

#################################################################
#########Ayudantia 7 - Preparación ejercicio 2###################
#################################################################
