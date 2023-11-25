# Hugo Vazquez
# 2176696
# 19/09/2023

# Importar datos de archivo excel a la consola de R
# Funcion "read.csv"
setwd("C:/UANL_FCF/REPOSITORIOS/Exp_Met_Est_AD2023")


# MAGT --------------------------------------------------------------------

#Correlacion

library(repmis)

erupciones <- source_data("https://www.dropbox.com/s/liir6sil7hkqlxs/erupciones.csv?dl=0")

erupciones <- source_data("https://www.dropbox.com/s/liir6sil7hkqlxs/erupciones.csv?dl=1")


plot(erupciones$waiting, erupciones$eruptions, xlab = "Tiempo de espera entre las eru´ciones (min)", ylab = "Duración de las erupciones (min)", pch = 19)

# Estadística descriptiva Erupciones

median(erupciones$eruptions)
sd(erupciones$eruptions)
var(erupciones$eruptions)
mean(erupciones$eruptions)
range(erupciones$eruptions)

boxplot(erupciones$eruptions)

# Estadística descriptiva de waiting
median(erupciones$waiting)
sd(erupciones$waiting)
var(erupciones$waiting)
mean(erupciones$waiting)
range(erupciones$waiting)

boxplot(erupciones$waiting)

# Establecer H0 y H1
# Función de correlación cor.test

cor.test (erupciones$eruptions, erupciones$waiting)

#  Prueba de t dependients ------------------------------------------------


# Datos de airquality de la ciudad de NY, USA
# Comparar las variables en dos periodos de verano (junio)
# Otoño (Septiembre)

aire<- airquality
boxplot(aire$Ozone ~ aire$Month)
aire$centi <- (aire$Temp - 32) / 1.8
aire$centi <- round((aire$Temp - 32)/1.8,1)
boxplot(aire$centi ~ aire$Month)

# Crear un subconjunto solo con los meses de Junio y Sept

aire.junio <- subset(aire, Month == "6")
aire.sep <- subset(aire, Month == "9")


t.test(aire.junio$Wind, aire.sep$Wind, paired = T)
boxplot(aire$Wind ~ aire$Month)


