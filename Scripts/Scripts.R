# Hugo Vazquez
# 2176696
# 29/08/2023

# Importar datos de archivo excel a la consola de R
# Funcion "read.csv"
# setwd("C:/UANL_FCF/REPOSITORIOS/Exp_Met_Est_AD2023")
# Importar ----------------------------------------------------------------

Ocampo <- read.csv("Scripts/EMA_Ocampo.csv", header = TRUE)
head(Ocampo)
tail(Ocampo)

# Descriptivas ------------------------------------------------------------

mean(EMA_Ocampo$DIRS)
mean(EMA_Ocampo$TEMP)
median(EMA_Ocampo$TEMP)
sd(EMA_Ocampo$TEMP)
var(EMA_Ocampo$TEMP)
range(EMA_Ocampo$TEMP)
fivenum(EMA_Ocampo$TEMP)

# Graficas ----------------------------------------------------------------

boxplot(EMA_Ocampo$TEMP, col="green",
        ylim=c(10, 30),
       ylab="Temp ´C",
       main="Ema Ocampo")
mtext("Ema", side=4)
mtext("Datos de agosto 2023", side=1, padj=3, adj=1)



# Datos de vivero ---------------------------------------------------------

Vivero_IE <- read.csv("Scripts/Vivero_IE.csv", header=T)
Vivero_IE$Tratamiento <- as.factor(Vivero_IE$Tratamiento)

mean(Vivero_IE$IE)
tapply(Vivero_IE$IE, Vivero_IE$Tratamiento, mean)
tapply(Vivero_IE$IE, Vivero_IE$Tratamiento, length)


# Normalidad de datos -----------------------------------------------------

# Shapiro wilks
shapiro.test (Vivero_IE$IE)

# Homogeneidad de varianza ------------------------------------------------
bartlett.test(Vivero_IE$IE ~ Vivero_IE$Tratamiento)

# Aplicar la prueba de t para muestras independientes
t.test(Vivero_IE$IE ~ Vivero_IE$Tratamiento, var.equal = T)
# Prueba de t de una muestra
# Subconjunto con los datos de Ctrl y Fert

Ctrl <- subset(Vivero_IE$IE,Vivero_IE$Tratamiento == "Ctrl")
Fert <- subset(Vivero_IE$IE, Vivero_IE$Tratamiento == "FERT")

t.test(Ctrl, mu = 0.95)
t.test(Ctrl, mu = 0.80)
t.test(Ctrl, mu = 0.90)

# Prueva de t dependients
# Datos de airquality de la ciudad de NY, USA
# Comparar las variables en dos periodos de verano (junio)
# Otoño (Septiembre)

aire<- airquality
boxplot(aire$Ozone ~ aire$Month)
boxplot(aire$Ozone ~ aire$month)
aire$centi <- (aire$Temp - 32) / 1.8
aire$centi <- round((aire$Temp - 32)/1.8,1)
boxplot(aire$centi ~ aire$Month)

# Crear un subconjuno solo con los neses de Junio y Sept

aire.junio <- subset(aire, Month == "6")
aire.sep <- subset(aire, Month == "9")


t.test(aire.junio$Wind, aire.sep$Wind, paired = T)
boxplot(aire$Wind ~ aire$Month)

#MAGT
#Corelacion
#
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


# Canopy ------------------------------------------------------------------

copa <- read.csv("Scripts/canopy_short.csv", header = T)
copa$Forest <- as.factor(copa$Forest)

#Identificación de tendencias

plot(copa$Cnpy, copa$LAI4)
plot(copa$Cnpy, copa$GLI)
cor.test (copa$Cnpy, copa$LAI4, method = "spearman")
cor.test(copa$Cnpy, copa$GLI)

