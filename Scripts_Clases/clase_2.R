# Hugo Vazquez
# 2176696
# 29/08/2023

# Importar datos de archivo excel a la consola de R
# Funcion "read.csv"
setwd("C:/UANL_FCF/REPOSITORIOS/Exp_Met_Est_AD2023")
# Importar ----------------------------------------------------------------

Ocampo <- read.csv("Scripts/Ocampo.csv", header = TRUE)
head(Ocampo)
tail(Ocampo)
# Descriptivas ------------------------------------------------------------

mean(Ocampo$DIRS)
mean(Ocampo$TEMP)
median(Ocampo$TEMP)
sd(Ocampo$TEMP)
var(Ocampo$TEMP)
range(Ocampo$TEMP)
fivenum(Ocampo$TEMP)

# Graficas ----------------------------------------------------------------

boxplot(Ocampo$TEMP, col="green",
        ylim=c(10, 30),
        ylab="Temp ´C",
        main=" Ocampo")
mtext("Ema", side=4)
mtext("Datos de agosto 2023", side=1, padj=3, adj=1)

