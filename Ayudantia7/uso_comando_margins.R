#Ejemplo de estimar marginales


#install.packages("margins")
library(margins)
mg1 <- margins(m2, at = list(igualitarismo = max(base_chile$igualitarismo), ingreso = min(base_chile$ingreso), edad = mean(base_chile$edad), educacion = mean(base_chile$educacion)))
mg2 <- margins(m3, at = list(igualitarismo = max(base_suecia$igualitarismo), ingreso = min(base_suecia$ingreso), edad = mean(base_suecia$edad), educacion = mean(base_suecia$educacion)))
mg3 <- margins(m4, at = list(igualitarismo = max(base_usa$igualitarismo), ingreso = min(base_usa$ingreso), edad = mean(base_usa$edad), educacion = mean(base_usa$educacion)))

summary(mg1)
summary(mg2)
summary(mg3)