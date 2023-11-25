# Hugo caralampio Vázquez Hernández
# 2176696
# Maestría en Ciencias Forestales-Primer semestre
# Experimentación y Métodos Estadísticos
# TAREA_2


# Una empresa de alimento para venados esta interesado en determinar si el peso neto medio del contenido de sus costales es de 80 kg como se anuncia en su producto. Digamos que un consumidor al paso del tiempo ha comprado y pesado de forma precisa el contenido neto de 44 costales de 80 kg seleccionados al azar. El consumidor reclama a la empresa que el contenido de sus costales ha sido menor porque sus datos no provienen de una distribución con media de 80 kg. Para investigar esta demanda, la empresa plantea una hipótesis usando el grado de confiabilidad de α = 0.05.

# La hipótesis nula será: No existen diferencias entre la media observada y la declarada en sus costales (80 kg). H0 : µ = 80

# La hipótesis alternativa se declara como: La media observada es menor a la declarada en los costales de 80 kg. H1 : µ < 80

# Ingresar Datos

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)
# Determinación del número de observaciones
n <- length(costal)
length(costal)

# Determinción de la media
costa.media <- mean(costal)
mean(costal)

#Desviación estándar

costa.sd <- sd(costal)
sd(costal)

# Dada la desviación estándar (3.056), ¿Cuál es la probabilidad de observar una media de la muestra (cuando n = 44) de 78.91 Kg o de menor cantidad si la media verdadera es de 80 kg.
# El denominador se le conoce como error estándar de la media (se).

costa.se <- costa.sd/ sqrt(n)
costa.se

# Entonces podemos calcular el valor de T
costa.T <- (costa.media - 80)/ costa.se
costa.T

# Finalmente, el valor de p puede ser calculado.
pt(costa.T, df = n-1) # Arriba se declaro "n" como número de costales observados


# EJERCICIO 1 -------------------------------------------------------------
# El resultado de la prueba de t de una muestra del procedimiento anterior puede ser verificado con la fórmula ya establecida en R.
t.test (costal)

#¿Cuál es el valor de p?
## El valor de es p es de 2.2e-16, indicando un valor altamente significativo.

# ¿Cuántos grados de libertad tiene el experimento?
## los grados de libertad están representados en el resultado de la correlación con las siglas df con un valor de 43 grados de libertad.

#¿Cuál es la hipótesis aceptada?
##  la hipótesis que se acepta es la alterna (H1= < 80), rechazando la Hipótesis nula (Ho = 80 y )

#¿Existe evidencia de que el valor medio promedio de los costales observados es menor (significativamente) a los que anuncia el producto?
## Los resultados obtenidas de la media observada, es evidencia de que si hay diferencia significativa respecto a lo que se anuncia en el producto, quedando por de bajo del peso verdadero.


# EJERCICIO 2 -------------------------------------------------------------


# En el siguiente cuadro se registraron los datos de emisiones de óxido de azufre en toneladas al año de una planta industrial. De acuerdo con la reglamentación de la empresa, la cantidad máxima de toneladas por año emitida no debe superar las 17.5 Ton/año.

# La hipótesis nula será:No existe diferencia significativa en en la media observada y la capacidad máxima establecida de 17.5 Ton/año.
# La hipótesis alternativa: existe diferencia significativa de la media observada en cuanto a la cantidad permitida en toneladas por año, quedando por encima de los 17.5 Ton/año.

Azufre <- c(15.8, 22.7, 26.8, 19.1, 18.5, 14.4, 8.3, 25.9, 26.4, 9.8,
            22.7, 15.2, 23.0, 29.6, 21.9, 10.5, 17.3, 6.2, 18.0, 22.9,
            24.6, 19.4, 12.3, 15.9, 11.2, 14.7, 20.5, 26.6, 20.1, 17.0,
            22.3, 27.5, 23.9, 17.5, 11.0, 20.4, 16.2, 20.8, 13.3, 18.1)

# Correlación
t.test (Azufre)

# ¿Cuál es el valor de p?
## El valor de es p es de 2.2e-16, indicando un valor altamente significativo.

# ¿Cuáles son los intervalos de confianza al 95 % ?
## Los intervalos de confianza del 95% son del 16.87912 y 20.53588

#¿Cuántos grados de libertad tiene el experimento?
## De acuerdo a los resultados de la correlación,  df tiene un valor de 39 grados de libertad.

# ¿Cuál es la hipótesis aceptada?
## Se opta por la hipótesis alterna, ya que de acuerdo a los resultados de la correlación correspondiente, la media estimada fue de 18.708 Ton/año, quedando por en cima de lo permitido.

#¿Existe evidencia de que el valor medio promedio de las emisiones observadas es mayor (significativamente) a la declarada en los procedimientos de seguridad de la empresa?
## El valor promedio de las emisiones observadas indican de que si hay evidencia significativa en que  las emisiones son  mayores a las declaradas por los procedimientos de seguridad de la empresa, debido a que queda por encima de la establecida.


# EJERCICIO 3 -------------------------------------------------------------

# Con la base de datos del Gobierno de México, se presenta la Temperatura actual diaria de las estaciones climatológicas convencionales contenidas en el Sistema de Información Hidrológica (SIH) almacenados en GitHub: https://raw.githubusercontent.com/mgtagle/MCF-202_Agosto_2021/main/TEM PAIRE_DIA.csv
setwd("C:/UANL_FCF/REPOSITORIOS/Exp_Met_Est_AD2023/Tareas")
TAREA_2 <- read.csv("TAREA_2.csv", header = TRUE)
head(TAREA_2)

# Encuentre si el valor promedio de la temperatura (temp_media) registradas es significativamente mayor a la establecida (24 Grados).

# La hipótesis nula será:No hay diferencia significativa de la temperatura media en relación a la establecida.
# La hipótesis alternativa: La temperatura media tiene una diferencia significativa en relación a la establecida, quedando por de bajo.

t.test (TAREA_2$temp_media)


# ¿Cuál es el valor de p?
## El valor de p es de 2.2e-16 siendo un valor significativo, por de bajo del 0.05

#¿Cuáles son los intervalos de confianza al 95 % ?
## Los intervalos de confianza del 98 % son de 23.28216 y 23.97599

# ¿Cuántos grados de libertad tiene el experimento?
## De acuerdo con los resultados de la correlación, los grados de libertad (df) tiene 845 

#¿Cuál es la hipótesis aceptada?
## se acepta la hipótesis alterna ya que el valor obtenido de la correlación fue de 23.63, por lo que es menor a la temperatura media establecida de 24 grados.

#¿Existe evidencia de que el valor medio promedio de las emisiones observadas es mayor (significativamente) a la declarada en los procedimientos de seguridad de la empresa?
## Acorde al valor observado de la temperatura media, no hay evidencia de que la temperatura media sea significativamente mayor a la establecida, ya que la temperatura media obtenida es de 23.62908, por de bajo de la establecida.
