# Hugo Vazquez
# 2176696

# TAREA_3 - Comparación de medias

setwd("C:/UANL_FCF/REPOSITORIOS/Exp_Met_Est_AD2023/Tareas")


# EJERCICIO 1 -------------------------------------------------------------

# Un laboratorio de Estados Unidos preguntó a 24 personas con fobia a las arañas (Aracnofobia) si podrían participar en un experimento. El laboratorio dividió a los asistentes en 2 grupos. Al Grupo 1 de 12 personas se les pidió que jugaran con una tarántula por un momento y se midieron sus niveles de ansiedad. A las 12 personas restantes (Grupo 2) solo se les mostró una fotografía de una tarántula y sus valores de ansiedad fueron igualmente medidos.

# Datos

Laboratorio_3 <- read.csv("Laboratorio_3.csv",header = TRUE)
head(Laboratorio_3)
Datos <- data.frame(Grupo, Ansiedad)
head(Datos)
mean(Laboratorio_3$Grupo_1)
mean(Laboratorio_3$Grupo_2)
var(Laboratorio_3$Grupo_1)
var(Laboratorio_3$Grupo_2)


boxplot(Ansiedad~Datos$Grupo,
        col = "green",
        ylim=c(30, 60),
        ylab="Nivel de ansiedad")


#Resuelva

# Describa la hipótesis nula y alternativa para este experimento
## Hipótesis nula (Ho): el nivel de ansiedad de ambos grupos no mostraron diferencias significativas, obteniendo valores de significacia similares.
## Hipótesis alterna (H1): el grupo de personas que se le mostró una tarántula real tuvieron una ansiedad más alta en comparación con el grupo que sólo se les mostró una fotografía.

#¿Cúales son las dos procedimientos que se deben cumplir los datos antes de realizar una prueba de t?
## Debe se únicamente de dos variables (muestras) y si las muestras son dependientes o independientes.

#¿Provienen los datos de una distribución Normal?
## Los datos no tienen una distribución normal, ya que están por de bajo del 0.05 de nivel de significacia, además de que se nececita de por lo menos 30 observaciones para determinar el tio de distribución normal de los datos

#¿Poseen los datos varianzas homogéneas?
## No existe homogeneidad en la comparación de varianza de ambos grupos, siendo el de Araña el de mayor valor con una varianza de 121.6364 y Fotografía de 86.3636


#Prueba de t
t.test (Laboratorio_3$Grupo_1)
t.test (Laboratorio_3$Grupo_2)
t.test (Laboratorio_3$Grupo_1, Laboratorio_3$Grupo_2)

#¿Cuál es el valor de p de la prueba de t?
## el valor de p= -1.6813

#¿Cuál es la hipótesis aceptada?
##la hipótesis que se acepta en esta ocación es la hipótesis alterna, ya que el valor de la media del grupo que se le mostró la "tarántula" fue de 47, quedando por encima del grupo de "Fotografía"

#¿El valor medio de ansiedad del grupo Fotografía es mayor estadísticamente comparado con el grupo que sostuvo una tarántula real?
## La media del grupo Fotografía es de 40, por lo que es menor que el valor obtenido del grupo que sostuvo la tarántula real 

#¿Cuántos grados de libertad tiene el experimento?
## Los grados de libertad fueron 23

#¿Cuál es el valor de p?
## El valor de p fue de 0.1072, altamente significativo.

#¿Cuál es la media de ansiedad del grupo Araña?
## La media de la ansiedad del grupo Araña es de 47

# EJERCICIO 2 -------------------------------------------------------------

# Para medir el efecto de pastoreo controlado contra pastoreo continuo 30 novillos fueron asignados a ambos tratamientos (15 y 15 en cada tratamiento). La ganancia en peso (en Kg) para cada animal se registró y se muestran en el siguiente cuadro. Se asume que las varianzas son diferentes y que ambos grupos se distribuyen normalmente.

## Hipótesis nula (Ho): ambos grupos se distribuyen normalmente y las varianzas son similares del uno al otro
## hipótesis alterna (H1): las varianzas son diferentes, siendo el pastoreo controlado con el mayor valor.

control <- c(130, 120, 61, 111, 93, 56, 85, 128, 73, 56, 65, 71, 109, 122, 85)
cont <- c(44, 62, 77, 58, 88, 61, 42, 57, 70, 38, 66, 82, 81, 54, 81)


# 95 % de intérvalo de confianza
t.test (control, cont)

# 99 % de intérvalo de confianza
t.test (control, cont, conf.level = 0.99)

var(control)
var(cont)
median(control)
median(cont)
mean(control)
mean(cont)



# A continuación se presentan los intervalos de confianza para el 95 % y 99 %.

# ¿Cuál es el valor de p de la prueba de t?
## Para el pastoreo controlado el valor de p es de 3.226e-09, mientras que para el pastoreo continuo, el valor de p es de 2.51e-10

# ¿Cuál es la hipótesis aceptada?
## se acepta la hipótesis alterna (H1), en que la varianza son diferentes (731.57 para pastoreo controlado y 246.07 para pastoreo continuo)

# ¿El valor medio de ganancia de peso del grupo Control es mayor estadísticamente comparado con el grupo Continuo?
## En efecto, es mayor el valor medio de la ganancia de peso del grupo control con un valor de 91, mientras que el grupo continuo es de 64.07

# ¿Cuántos grados de libertad tiene el experimento?
## los grados de libertad es de 14 (df) para ambos tipos de pastoreo

# ¿Cuál es el valor de p?
## El valor de p es de 0.002934

#intervalos de confianza del 99%
## 4.2207 y 49.6460

# EJERCICIO 3 -------------------------------------------------------------

# Considere los datos del siguiente cuadro los cuales fueron obtenidos de muestras de suelo de un bosque templado en dos tiempos diferentes. Los datos muestran el contenido de carbono orgánico medidos en las mismas muestras solo en distintas épocas.

TAREA_3<- read.csv("TAREA_3.csv", header = TRUE)
head(TAREA_3)

median(TAREA_3$Time.1)
median(TAREA_3$Time.2)
mean(TAREA_3$Diferencia)

t.test(TAREA_3$Time.1, TAREA_3$Time.2)

# ¿Cuál es el valor de p de la prueba de t?
## el valor de p en la pruea de t= 1.8739

#¿Cuál es el valor de p?
## el valor de p= 0.007889

#¿Cuántos grados de libertad tiene el experimento?
## El experimento tiene 9 grados de libertad (df=9)

#¿Cuál es la hipótesis aceptada?
## la Hipótesis alterna es la que se acepta (H1)

#¿Cuál es la diferencia media entre los dos tiempos?
## la media de la diferencia de ambos tiempos es de 0.181

