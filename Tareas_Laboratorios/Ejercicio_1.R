# Hugo Caralampio Vázquez Hernández
# 2176696
# 06/10/2023

# Importar datos de archivo excel a la consola de R
# Función "read.csv"

setwd("C:/UANL_FCF/REPOSITORIOS/Exp_Met_Est_AD2023/Tareas")

# TAREA_1 -----------------------------------------------------------------

# EJERCICIO_1 -------------------------------------------------------------

Ejercicio_1 <- read.csv("Ejercicio_1.csv", header = TRUE)

## Hipótesis nula
# Ho. la abundancia de efímeras está directamente relacionada a la velocidad de los arroyos.


Ejercicio_1<- read.csv("Ejercicio_1.csv", header = TRUE)
head(Ejercicio_1)
cor.test (Ejercicio_1$Speed, Ejercicio_1$Abundance, var.equal = T)

plot(Ejercicio_1$Speed, Ejercicio_1$Abundance,xlab = "variable independiente", ylab = "Variable dependiente", pch = 19)

## CONCLUSIONES Ejercicio_1 

# El valore de la correlación (r) es de 0.8441, lo que nos indica que es altamente significativa,  por lo que no se requiere aumentar la muestra para obtener más viabilidad 
# Mientras que el valor de P-value es de 0.008393.
# En la gráfica de dispersión nos indica que cuando el valor del eje de las X aumenta, también aumenta el de las ejes Y, el cual se considera una dispersión positiva
# Valores obtenidos(r= 0.8441408, Grados de libertad= 6, Valor de p-value= 0.008393)

# EJERCICIO_2 -------------------------------------------------------------

# Hipótesis 
## Las propiedades del suelo están relacionados de manera positiva con respecto al pH del suelo 

TAREA_1 <- read.csv("TAREA_1.csv", header = TRUE)

# Datos de la estructura del suelo

head(TAREA_1)
tail(TAREA_1)

# Análisis de correlación (pH - n(Estructura del suelo)

Estructura_suelo <- read.csv("Estructura_suelo.csv", header = TRUE)
cor.test(Estructura_suelo$pH, Estructura_suelo$N)
cor.test(Estructura_suelo$pH, Estructura_suelo$Dens)
cor.test(Estructura_suelo$pH, Estructura_suelo$P)
cor.test(Estructura_suelo$pH, Estructura_suelo$Ca)
cor.test(Estructura_suelo$pH, Estructura_suelo$Mg)
cor.test(Estructura_suelo$pH, Estructura_suelo$K)
cor.test(Estructura_suelo$pH, Estructura_suelo$Na)

# Datos con los estadísticos de interés (valores de r y p)

Correlaciones  <- read.csv("Correlaciones.csv", header = TRUE)
head(Correlaciones)

# generación del correlograma de la relación que hay entre los datos de suelo

Estructura_suelo  <- read.csv("Estructura_suelo.csv", header = TRUE)



library("corrplot")

Estructura_suelo.cor <- cor(Estructura_suelo, method = "pearson")
round(Estructura_suelo.cor, digits = 2)
corrplot(Estructura_suelo.cor, type = "upper")

# Conclusiones
# Se rechaza la hipótesis nula, debido a que no todas los componentes del suelo están relacionados de manera positiva con el pH del mismo, principalmente en los componentes "Dens, Mg, K y Na, donde los valores de correlación (r) son relativamente bajo, con una diferencia significativa, a excepción del Ca con un valor de R=0.03089
# En el Correlograma, los colores más tenues son de los valores de correlación más bajas (Dens, k, Mg y Na).



