## 1. Carga los datos y examínalos en R. Emplea las funciones head(), summary(), dim() y str(). 
Datos_de_tt.o<-read.table("datos-trabajoR.txt",header=TRUE)
head(Datos_de_tt.o)
summary(Datos_de_tt.o)
dim(Datos_de_tt.o)
str(Datos_de_tt.o)
#¿Cuántas variables hay? 4 variables 
#¿Cuántos tratamientos? 55 tratamientos


## 2. Haz un boxplot para nuestros datos. Uno para cada condición. Elige un color para cada condición y guárdalo para las siguientes gráficas. 
boxplot(Datos_de_tt.o$Tratamiento, col = "pink",main = "Boxplot de tratamiento", ylab = "Valores")
#Boxplot de los wildtype 
boxplot(Datos_de_tt.o$Wildtype,col ="blue",main="Boxplot del wildtype", ylab = "valores")
#Boxplot de sequía 
boxplot(Datos_de_tt.o$Sequia,col = "orange",main= "Boxplot de Sequía", ylab = "valores")
#Boxplot de Exceso Riesgo 
boxplot(Datos_de_tt.o$ExcesoRiego, col="yellow",main= "Boxplot de Exceso Riesgo", ylab= "valores ")

## 3. Haz dos gráficos de dispersión. El primero debe comparar Sequía con Wildtype, y el segundo ExcesoRiego con Wildtype. 
plot(Datos_de_tt.o$Wildtype, Datos_de_tt.o$Sequia, col= c("cyan", "darkorchid", "deeppink", "pink", "skyblue", "gold"), main = "Gráfico de Dispersión sequía VS Wildtype", xlab = "Wildtype", ylab = "Sequía")

# Ahora el Gráfico de dispersión Exceso de riesgo Vs Wildtype.
plot(Datos_de_tt.o$Wildtype, Datos_de_tt.o$ExcesoRiego, col= c("cyan", "darkorchid", "deeppink", "pink", "skyblue", "gold"), main = "Dispersion Graph sequía VS Exceso riesgo", xlab = "Wildtype", ylab = "Exceso de riesgo")

## 4. Ponle leyenda al gráfico del apartado anterior. En el margen inferior derecho.
legend("bottomright", legend=paste ("Tto", unique(Datos_de_tt.o$Tratamiento)), col= c("cyan", "darkorchid", "deeppink", "pink", "skyblue", "gold"), pch=16, cex=0.8, title="Tratamiento") 

## 5. Haz un histograma para cada variable. 
# Histograma 
#Histograma tratamientos 
hist(Datos_de_tt.o$Tratamiento,col = "pink", main = "Tratamientos", xlab = "Tratamientos", ylab = "Frecuencia")
#Histograma Wildtype 
hist(Datos_de_tt.o$Wildtype,col = "blue", main = "Wildtype", xlab = "Wildtype", ylab = "Frecuencia")
#Histograma de sequía
hist(Datos_de_tt.o$Sequia,col = "orange", main = "Sequia", xlab = "Sequia", ylab = "Frecuencia")
#Histograma de Exceso de Riesgo 
hist(Datos_de_tt.o$ExcesoRiego,col = "yellow",main = "Exceso de Riesgo",xlab = "Exceso Riesgo",ylab = "Frecuencia")


## 6. Haz un factor en la columna tratamiento y guárdalo en una variable. 
factor_tratamiento<-as.factor(Datos_de_tt.o$Tratamiento)

## 7. Calcula la media y la desviación estándar para cada tratamiento.
#Media por tratamiento 
media_porTratamiento<-aggregate(Datos_de_tt.o$Tratamiento~factor_tratamiento,data = Datos_de_tt.o,FUN = mean)
media_porTratamiento
#Cálculo la desviación estandar por tratamiento 
deviacionEst_porTratamiento<-aggregate(Datos_de_tt.o$Tratamiento~factor_tratamiento,data = Datos_de_tt.o,FUN = sd)

## 8. Averigua cuántos elementos tiene cada tratamiento. 
conteo_tratamiento<-table(Datos_de_tt.o$Tratamiento)
print(conteo_tratamiento)

## 9. Extrae los datos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una variable diferente. 
tratamiento_1<-Datos_de_tt.o[Datos_de_tt.o$Tratamiento==1,]
tratamiento_4<-Datos_de_tt.o[Datos_de_tt.o$Tratamiento==4,]

## 10. Queremos comprobar que hay diferencias significativas para el tratamiento 1 y el tratamiento 5 entre Wildtype y Sequia, y entre Wildtype y ExcesoRiego. 
#Primero vamos a extraer los datos del tt.o 5 
tratamiento_5<-Datos_de_tt.o[Datos_de_tt.o$Tratamiento==5,]
#Luego comprobamos la normalidad de el tratamieto 1 
shapiro_wildtype_t1<-shapiro.test(Datos_de_tt.o$Wildtype)
shapiro_sequia_t1<-shapiro.test(tratamiento_1$Sequia)
shapiro_excesoriego_t1 <- shapiro.test(tratamiento_1$ExcesoRiego)

#Después comprobamos la normalidad de el tratamieto 5 
shapiro_wildtype_t5 <-shapiro.test(tratamiento_5$Wildtype)
shapiro_sequia_t5<-shapiro.test(tratamiento_5$Sequia)
shapiro_excesoriego_t5<- shapiro.test(tratamiento_5$ExcesoRiego)

#Para visualizar los datos de tt.o 1 
shapiro_wildtype_t1 # No sigue distrubución normal 
shapiro_sequia_t1 # Sí sigue distribución normal
shapiro_excesoriego_t1 #Sí sigue distribución normal

#Para visualizar los datos de tt.o 5
shapiro_wildtype_t5 #No sigue distrubución normal
shapiro_sequia_t5  #No sigue distrubución normal 
shapiro_excesoriego_t5 #Sí sigue distrubución normal


#Comparamos las variables de tratamiento 1 para ver si son iguales 
#de tt.o 1 Wt vs sequia
var_test_wildtype_sequia_t1<- var.test(tratamiento_1$Wildtype, tratamiento_1$Sequia)
#de tt.o 1 Wt vs exceso de riegso 
var_test_wildtype_excesoriesgo_t1<-var.test(tratamiento_1$Wildtype, tratamiento_1$ExcesoRiego)
#de tt.o 1 sequia  vs exceso de riegso 
var_test_sequia_excesoriego_t1 <- var.test(tratamiento_1$Sequia, tratamiento_1$ExcesoRiego)

#Ver los resultados de tt.o 1 
var_test_wildtype_sequia_t1 #Variancias no iguales 
var_test_wildtype_excesoriesgo_t1  #Variancias no iguales 
var_test_sequia_excesoriego_t1 #Variancias iguales  


#Comparamos las variables de tratamiento 5 (para ver si son iguales)
#de tt.o 5 Wt vs sequia
var_test_wildtype_sequia_t5 <- var.test(tratamiento_5$Wildtype, tratamiento_5$Sequia)
#de tt.o 5 Wt vs exceso de riesgo 
var_test_wildtype_excesoriego_t5 <- var.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego)
#de tt.o 5 sequia vs exceso de riesgo 
var_test_sequia_excesoriego_t5 <- var.test(tratamiento_5$Sequia, tratamiento_5$ExcesoRiego)

#Ver los resultados de tt.o 5
var_test_wildtype_sequia_t5 #Variancias no iguales  
var_test_wildtype_excesoriego_t5  #Variancias no iguales 
var_test_sequia_excesoriego_t5 #Variancias no iguales 

# Elegir y Realizar la Prueba Estadística
#Comparar Wildtype vs Sequia y Wildtype vs 
# Utilizo wilcox.test cuando uno o ambos no siguen la normalidad 
# Utilizo T-test cuando ambos siguen la normalidad 

#Tratamiento 1 
# del tt.o 1 Wt vs sequia
wilcox_wildtype_sequia_t1 <- wilcox.test(tratamiento_1$Wildtype, tratamiento_1$Sequia, exact = F)
# del tt.o 1 Wt vs exceso de riesgo 
wilcox_wildtype_excesoriesgo_t1 <- wilcox.test(tratamiento_1$Wildtype, tratamiento_1$ExcesoRiego, exact =  F)
# del tt.o 1 sequia vs exceso de riesgo 
t_test_sequia_excesoriesgo_t1 <- t.test(tratamiento_1$Sequia, tratamiento_1$ExcesoRiego, var.equal = TRUE)

#Tratamiento 5
# del tt.o 5 Wt vs sequia
wilcox_wildtype_sequia_t5<-wilcox.test(tratamiento_5$Wildtype, tratamiento_5$Sequia, exact = FALSE)
# del tt.o 5 Wt vs exceso de riesgo 
wilcox_wildtype_excesoriesgo_t5 <- wilcox.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego, exact =  F)
# del tt.o 5 exceso de riesgo vs sequia
wilcox_sequia_excesoriesgo_t5 <- wilcox.test(tratamiento_5$Wildtype, tratamiento_5$ExcesoRiego, exact =  F)


#RESULTADOS PRUEBA ESTADÍSTICA
#TT.O 1 
wilcox_wildtype_sequia_t1
wilcox_wildtype_excesoriesgo_t1
t_test_sequia_excesoriesgo_t1

#TT.O 2 
wilcox_wildtype_sequia_t5
wilcox_wildtype_excesoriesgo_t5
wilcox_sequia_excesoriesgo_t5 

#En función de los resultados de la prueba de normalidad, ¿qué test usarías para cada comparativa?
# Si sigue una distrubución normal: Test paramétrico ( como ks.test() o  aov())
# Si no sigue una distribución normal: Test no paramétrico ( u.test(), wilcox.test() o kruskal.test())

## 11. Realiza un ANOVA para comparar el tratamiento 1 en las tres condiciones.

# En primer lugar, separamos las condiciones en variables
wildtype_t1 <- tratamiento_1$Wildtype
sequia_t1 <- tratamiento_1$Sequia
excesoriego_t1 <- tratamiento_1$ExcesoRiego


#Siguiendo, creamos el data frame del anova (una tabla como la anterior)

datos_anova <- data.frame(valor = c(tratamiento_1$Wildtype, tratamiento_1$Sequia, tratamiento_1$ExcesoRiego),
                          condicion = rep(c("Wildtype", "Sequia", "ExcesoRiego"), each = nrow(tratamiento_1))
                          )

# Realizamos el anova: para comprobar si hay diferencias significativas entre las condiciones
anova_resultado <- aov(valor ~ condicion, data = datos_anova)

# Resultados del ANOVA
summary(anova_resultado)
