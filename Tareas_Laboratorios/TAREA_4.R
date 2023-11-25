# Hugo Vazquez
# 2176696
# 10/11/2023

# Análisis de varianza

setwd("C:/UANL_FCF/REPOSITORIOS/Exp_Met_Est_AD2023")

# EJERCICIO 1 -------------------------------------------------------------
# Se examinaron cinco tipos diferentes de suelo para detectar la aparición de nematodos. En cada caso se realizaron cinco replicas para su evalu

# Cantidad de nematodos encontrados en cinco tipos de suelo diferente. Cada suelo contiene cinco muestras.

# Variables

Grupo <- gl(5, 5, labels = c("S1","S2", "S3", "S4", "S5"))
Nemátodos <- c(127, 166, 136, 182, 133,
               162, 156, 123, 136, 127,
               155, 140, 125, 115, 117,
               124, 95, 88, 97, 98,
               169, 147, 166, 157, 169)
Variables <- data.frame(Grupo, Nemátodos)
head(Nemátodos)

boxplot(Nemátodos~Variables$Grupo,
  col = "blue",
        ylim=c(90, 185),
        ylab="Número de nemátodos")
mtext("Tipos de suelo", side=1, padj=3)

# Aplique la función tapply y encuentre las varianzas de los cinco tratamientos. ¿Cuántas veces es la diferencia entre la varainza más pequeña y la más grande?

Varianzas_tratamiento <- tapply(Variables$Nemátodos, Variables$Grupo, var)
print(Varianzas_tratamiento)

# Realiza un test F (ANOVA) para comparar las medias de las 5 muestras ¿Cuáles serían las hipótesis nula y alternativa?
    ## Al realizar el ANOVA, para la comaración de medias, las hipótesis serían las siguientes
    ## Ho= Las medias de todos los grupos son iguales
    ## H1= No todas las medias de los grupos son iguales, por lo menos hay una media diferente a resto de los demás.

anova <- aov(Nemátodos ~ Grupo, data = Variables)
anova <- summary(anova)
print(anova)

# Describe los resultados obtenidos indicando cuál es el valor del estadístico de contraste (F), los grados de libertad del factor, los grados de libertad residuales y el valor de P.
    ## Los valores asociados al factor "Grupo" son los siguientes
    ## El estadístico de contraste F tiene un valor de 9.287
    ## Hay 4 grados de libertad para el factor "Grupo" y 20 grados de libertad residuales
    ## el valor de p es de 0.000207, rechazando la Hipótesis nula (Ho), lo que nos indica que al menos una de las medias es diferente al resto de los grupos

# También indica cuál sería el valor crítico de F bajo la hipótesis nula, que nos proporcionará la definición de una región de aceptación y rechazo (consideramos un nivel de significación alfa = 0.05).
    ## si tenemos 4 grados de libertad para Grupo y 20 grados de libertad para Residuales y un nivel de significancia alpha=0.05, aplicamos la siguiente función para determinar el valor crítico de F
df_Grupo <- 4
df_Residuales <- 20
alpha <- 0.05
    ## entonces:
valor_critico_F <- qf(1 - alpha, df_Grupo, df_Residuales)
print(valor_critico_F)
    ## entonces el valor crítico de F con un nivel de significancia alfa del 0.05 es de 2.87 siendo mayor el de contraste F calculado con 9.287, por lo que es evidencia de rechazo a la hipótesis nula.

# Tras evaluar la tabla ANOVA, ¿cuál sería tu conclusión en el contexto del problema?
    ## Se rechaza la hipótesis nula, quedando demostrado que hay un grupo de entre los existentes que es diferente. Por otro lado el análisis de varianza indica que hay una diferencia significativa entre los grupos

# EJERCICIO 2 -------------------------------------------------------------

# Se examino el crecimiento de una especie bajo diferentes regímenes de riego. Cada tratamiento contiene seis observaciones.

# Crecimiento de la especie bajo diferentes regímenes de riego (Bajo, Medio y Alto). Cada tipo de riego contiene seis observaciones.

# Riesgos

Régimen <- gl(3, 6, labels = c("Bajo","Medio", "Alto"))
Rango <- c(9, 11, 6, 7, 6, 5,
               14, 17, 19, 14, 17, 15,
               28, 31, 32, 44, 38, 37)
Riesgos <- data.frame(Régimen, Rango)
head(Rango)

boxplot(Rango~Riesgos$Régimen,
        col = "blue",
        ylim=c (0, 45),
        ylab="crecimiento (mm)")
mtext("Tipo de Riesgo", side=1, padj=3)


# Aplique la función tapply y encuentre las varianzas de los cinco tratamientos. ¿Cuántas veces es la diferencia entre la varianza más pequeña y la más grande?

Varianzas_tratamiento <- tapply(Riesgos$Rango, Riesgos$Régimen, var)
print(Varianzas_tratamiento)

# Realiza un test F (ANOVA) para comparar las medias de las 5 muestras ¿Cuáles serían las hipótesis nula y alternativa?
    ## Al realizar el ANOVA para comparar las medias de los tipos de régimen de tratamientos, las hipótesis quedan de la siguiente manera:
    ## Ho= Las medias de los tratamientos Bajo, Medio y Alto son iguales 
    ## H1= Una de las medias de los tratamientos Bajo, Medio o Alto es diferente de las demás.

anova <- aov(Rango ~ Régimen, data = Riesgos)
anova <- summary(anova)
print(anova)

# Describe los resultados obtenidos indicando cuál es el valor del estadístico de contraste (F), los grados de libertad del factor, los grados de libertad residuales y el valor de P
    ## Los valores asociados al factor "Régimen" son los siguientes
    ## El estadístico de contraste F tiene un valor de 84.48
    ## Hay 2 grados de libertad para el factor "Régimen" y 15 grados de libertad residuales
    ## el valor de p es de 6.84e-9, rechazando la Hipótesis nula (Ho) con un valor relativamente por de bajo del 0.05, lo que nos indica que hay medias es diferente al resto de los grupos.


# También indica cuál sería el valor crítico de F bajo la hipótesis nula, que nos proporcionará la definición de una región de aceptación y rechazo (consideramos un nivel de significación alfa = 0.05).
    ## si tenemos 2 grados de libertad para Grupo y 15 grados de libertad para Residuales y un nivel de significancia alpha=0.05, aplicamos la siguiente función para determinar el valor crítico de F
df_Régimen <- 2
df_Residuales <- 15
alpha <- 0.05
    ## entonces:
valor_critico_F <- qf(1 - alpha, df_Régimen, df_Residuales)
print(valor_critico_F)
    ## entonces el valor crítico de F con un nivel de significancia alfa del 0.05 es de 3.6832 siendo mayor el de contraste F calculado con 84.48, por lo que es evidencia de rechazo a la hipótesis nula.

# Tras evaluar la tabla ANOVA, ¿cuál sería tu conclusión en el contexto del p
    ## Se rechaza la hipótesis nula, ya que existe diferencia entre los grupos de régimen, mientras que en el análisis de varianza indica que hay una diferencia significativa del Régimen Alto, respecto al Medio y al Bajo que son similares


