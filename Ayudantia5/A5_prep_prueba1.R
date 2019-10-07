##############################################################
########### Ayudantia 5 - Preparaci?n Prueba I #################
##Profesor: Luis Maldonado
##Ayudantes: Catalina Rufs - José Daniel Conejeros
##############################################################

##############################################################
#Ejercicio 
##############################################################
##Configuraciones iniciales
rm(list = ls()) #Limpiamos la memoria
options(scipen=999) #Desactivamos la notación científica

##############################################################
##############################################################

#a. Estime el R cuadrado no ajustado y el cuadrado ajustado. Interprete sus resultados:**
  
#R cuadrado no ajustado:
37.11/38.75
#0.9577

#R cuadrado ajustado:
1.64/(4-1-1)
#0.82
38.75/(4-1)
#12.92
1-(0.82/12.92)
#0.9365

##############################################################
##############################################################

#b. Estime la correlaci?n lineal entre edad y altura:**
  
#La correlaci?n se puede estimar como la ra?z cuadrada del R cuadrado no ajustado:
(0.9577)^(1/2)
#0.9786

##############################################################
##############################################################

#c.La desviaci?n est?ndar de edad es 7,89 y la desviaci?n est?ndar de altura es 3,59. En base a esta informaci?n y a sus c?lculos en los puntos a) y b), estime el coeficiente de regresi?n estandarizado, asumiendo que altura es la variable dependiente y edad la variable independiente. Interprete el coeficiente de regresi?n estandarizado estimado.** 

#Coeficiente de regresi?n no estandarizado:
(0.98*3.59/7.89)
#0.4459

#Coeficiente de regresi?n estandarizado:
(7.89*0.45/3.59)
#0.9890

##############################################################
##############################################################

#d. Se sabe que el intercepto de la regresion de la altura con edad como predictor es 66,69. Con esta informaci?n m?s sus estimaciones formule el modelo estimado. Adem?s, utilice el modelo estimado para estimar la altura predicha de Sara a los 40 a?os (480 meses). Coment? su resultado, ?le parece razonable? **

#La altura predicha de Sara a los 40 a?os es:
66.69+0.45*480
#282.69

