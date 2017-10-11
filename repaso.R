##Repaso R
## cosas básicas:
ID <- paste("ID", 1:10, sep = "")
Sexo <- c("M", "F", "F", "M", "F", "F", "F", "M", "M", "F")
length(Sexo)
table(Sexo)
prop.table(table(Sexo))
100 * prop.table(table(Sexo))
barplot(prop.table(table(Sexo)))
Hijos <- c(0, 0, 2, NA, 1, 3, 1, 0, 2, 1, NA) 
table(Hijos)
table(Hijos, useNA="always")
cumsum(table(Hijos))
is.na(Hijos)
sum(is.na(Hijos))
#Proporción de faltantes
100 * sum(is.na(Hijos)) / length(Hijos)

## Funciones estadísticas básicas

ingreso <- c(800, 560, 1200, 600, 1800, 1500, 2700, 1900, 5000, 650, 1600)
summary(ingreso)
length(ingreso)
mean(ingreso)
median(ingreso)
sd(ingreso)
100 * sd(ingreso) / mean(ingreso)
ingreso > 1000
table(ingreso > 1000)
skewness(ingreso)
library(moments)
skewness(ingreso)
kurtosis(ingreso)
quantile(ingreso, c(0.25, 0.5, 0.75))
quantile(ingreso, 0.2)
fivenum(ingreso)
mean(ingreso, trim = 0.05) #Promedio recortado al 10% (Cinco y cinco)

m1 <- matrix(c(25, 34, 56, 19, 17, 23, 3, 12, 35, 0, 0, 1), ncol = 2)
rownames(m1) <- paste("Persona", 1:6, sep = "")
colnames(m1) <- c("edad", "exper_laboral")

?apply
apply(m1,1, FUN = min)
apply(m1,2, FUN = min)
##1. importar base de datos:
#en .csv
library(readr)
sisben <- read_csv2("poblacionSisben.csv")
#en .xlsx
library(readxl)
saber2015 <- read_excel("saber2015.xlsx")
# en spss
library(haven)
Ocupados_GEIH <- read_spss("Cabecera - Ocupados.sav")
#en rdata:
load("emb2011.RData")

#2 filtros y agregaciones:
a <- c(11, 12, 13, 14, 15, 16, 17, 18)
a <- 11:18 # hace lo mismo que la linea anterior
# extraer valores 3 y 6 del vector:
a[c(3,6)]
# Con números enteros negativos quito valores
a[c(-3, -6)]
#También funcionan las condiciones lógicas
indices <- 1:8
a[indices == 3  | indices == 6 ]

#seleccionar de la base de datos los colegios que pertenecen a bogota:
bogota <- subset(saber2015, DEPARTAMENTO == "BOGOTA")

#Agregaciones
#obtener promedio de matematicas por departamentos
library(dplyr)
deptos<- group_by(saber2015, DEPARTAMENTO)#Es como la tabla dinámica, permite agrupar por una variable
resumen <- summarise(deptos, mean(PROMMATEMATICA))

#seleccionar de la base las columnas con las que quiero trabajar:
library(sqldf)
saber2015_2 <- select(saber2015, DEPARTAMENTO, CALENDARIO, NATURALEZA, JORNADA, EVALUADOS)
#promedio de matematicas por departamento con sentencas sql
matedptos <- sqldf("select DEPARTAMENTO, avg(PROMMATEMATICA) as 'prom_mat' from saber2015 group by DEPARTAMENTO")
# Estadisticas a nivel departamental cuando el colegio es oficial con sentencias sql:
estad_depto <- sqldf("select DEPARTAMENTO, count(*) as 'conteo_dept', avg(PROMMATEMATICA) as 'prom_mat', median(PROMMATEMATICA) as  'mediana_mat', stdev(PROMMATEMATICA) as 'desv_mat' from saber2015 where NATURALEZA = 'oficial' group by DEPARTAMENTO")


#tablas de frecuencia:
tabla1 <- summarise(deptos, num_col = n())

##recodificacion de variables:
attach(saber2015)
saber2015$clasnumcol[EVALUADOS %in% 1:15] <- "pequeño"
saber2015$clas_num_col[EVALUADOS %in% 16:150] <- "mediano"
#Una pequeña consulta:
summarise(saber2015, max(EVALUADOS)) #para saber el maximo de evaluados
saber2015$clas_num_col[EVALUADOS %in% 151:987] <- "grande"
#ahora una tabla de frecuencias
tabla2 <- table(saber2015$clas_num_col)
#Tabla de frecuencias
t <- (table(saber2015$JORNADA))
barplot(t)

###pegado de bases con el paquete base:
#Para realizar este ejerecicio voy a crear dos tablas:
saber2015_3 <- select(saber2015,CODINST, DEPARTAMENTO, CALENDARIO, NATURALEZA, JORNADA, EVALUADOS)
saber2015_4 <- select(saber2015, CODINST, PROMLECTURACRITICA, PROMMATEMATICA, PROMSOCIALESYCIUDADANAS, PROMCIENCIASNATURALES, PROMINGLES )
#Inner Join: conserva los individuos que estén en ambas variable
saber_inner <- inner_join(saber2015_3,saber2015_4)
saber_left <- left_join(saber2015_3, saber2015_4)
saber_right <- right_join(saber2015_3, saber2015_4)

##extraer un dato de la base
bogota <- subset(saber2015, CODINST == "085761")
