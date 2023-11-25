# Hugo Vazquez
# 2176696
# 12/09/2023

# Importar datos de archivo excel a la consola de R
# Funcion "read.csv"
setwd("C:/UANL_FCF/REPOSITORIOS/Exp_Met_Est_AD2023")

# Datos de Vivero ---------------------------------------------------------
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


