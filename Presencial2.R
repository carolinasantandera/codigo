
setwd("D:/Gdrive/Laboral 2016/DANE_INNOVACION/Curso análisis de datos con R/cursor_2016II/Curso/Bases/datos_curso/Mod4")

rm(list = ls())
load("EMB2011.RData")

library(dplyr)
library(ggplot2)

# 1. Cálcule el gasto promedio y de ingreso de los hogares en Bogotá y por localidad, 
# realice un gráfico con esta información.

resumen1 <- select(a, directorio, localidad, estrato) %>% left_join(localidad) %>% 
         inner_join(select(c, directorio_hog, directorio)) %>% inner_join(ing_gastos) %>%
         group_by( nom_loc) %>% summarise(prom_gasto = mean(gasto), 
         prom_ingreso = mean(ingreso, na.rm = T)) %>%  arrange(-prom_gasto)

resumen1 %>% ggplot(aes(x = nom_loc, y = prom_gasto)) + geom_bar(stat="identity")


resumen1$nom_loc2 <- factor(as.character(resumen1$nom_loc),
                                levels = as.character(resumen1$nom_loc2))
    
resumen1 %>% ggplot(aes(x = nom_loc2, y = prom_gasto)) + geom_bar(stat="identity") +
             coord_flip()
    
resumen1 %>% ggplot(aes(x = nom_loc2, y = prom_gasto)) + geom_bar(stat="identity") +
      coord_flip()
    
resumen1$nom_loc3 <- factor(as.character(resumen1$nom_loc),
                                levels = as.character(resumen1$nom_loc2)[19:1])
    
    
resumen1 %>% ggplot(aes(x = nom_loc3, y = prom_gasto, fill = "red")) + 
                 geom_bar(stat="identity") + xlab("Promedio del gasto") + ylab("Localidad") +
                 coord_flip() +  theme_bw() + theme(legend.position="none") 
    

# Ejercicio: calcular la correlación por estrato entre el gasto y el ingreso
select(a, directorio, localidad, estrato) %>% left_join(localidad) %>% 
inner_join(select(c, directorio_hog, directorio)) %>% inner_join(ing_gastos) %>%
group_by( nom_loc) %>% summarise(prom_gasto = cor(gasto, ingreso, 


# Calcule el ingreso promedio, el ingreso mediana, la desviación estándar y el coeficiente
# de variación por estrato
                                                  
# Cálcular el ingreso per carpita promedio por estrato promedio (usar la pregunta c18)
# http://formularios.dane.gov.co/Anda_4_1/index.php/catalog/189/download/2877

#Calcule el ingreso, el gasto promedio y los deciles de gasto de los Bogotanos en el año 2011. 
# Realice lo mismo por localidad.



# Con las pruebas saber 
library(haven)
saber <- read_spss("saber11_2015II.sav" )      
# Busque como le fue al colegio del cual usted egreso en las pruebas saber en las pruebas saber: Consejo use arrange 

#(ordene por municipio y nombre de la institución) y genere un consecutivo para saber el ranking.
# Calcule el puntaje global de las pruebas Saber y encuentre los 10 mejores colegios de Bogotá 
# (según el puntaje global) de jornada continua, que registraron más de 10 estudiantes 
# y que sean de calendario A.





#1. Calcule el ingreso, el gasto promedio y los deciles de gasto de los Bogotanos en el año 2011. 
#Realice lo mismo por localidad.
prom_gasto <- mean(ing_gastos$gasto, na.rm = T)
prom_ingreso <- mean(ing_gastos$ingreso, na.rm = T)
deciles <- quantile(ing_gastos$gasto, probs = c(seq(0, 1, by=0.1)), na.rm = T)
#2. Calcule cómo es la correlación del gasto y el ingreso en Bogotá
#para eliminar los faltantes, buscar en la ayuda de cor, las opciones disponibles en el argumento use
correlación <- cor(x= ing_gastos$gasto, y= ing_gastos$ingreso, use = "na.or.complete") 
#3.Calcule cuántas encuestas, el número de faltantes y la proporción de los mismos hubo en la encuesta
#por localidad.Haga lo mismo por estrato, ¿Qué concluye?
a1 <- merge(a, localidad, by = "localidad")#pegar a la tabla a la localidad que se encuentra en la tabla localidad
a2 <- merge(a, c[c("directorio_hog", "directorio")])#pegar de la tabla c solo las variable direcotrio_hog, y directorio
table(a2$localidad)#cantidad de encuestas por localidad
sum(is.na(a2$localidad))#número de faltantes por localidad
table(a2$estrato)#cantidad de encuestas por estato
sum(is.na(a2$estrato))#cantidad de faltantes por estrato
#se observa que no hay faltantes ni por localidad ni por estrato, por lo tanto la proporcion es del 100% para las dos variables.
# el codigo 9 por estrato es el faltante por estrato
#4. Corra el código que se presenta a continuación y genere la tabla ing_gastos (revise las  columnas que resultaron)

# Preparación de la base de datos:
rm(list = ls())
setwd("/Users/Carolina/Documents/curso R 2016")
load("emb2011.RData")
# No le preste atención a la función merge (se verá en detalle más adelante):
localidad <- merge(a, localidad)
localidad$nom_loc <- as.character(localidad$nom_loc)

c <- merge(localidad, c)
tabla_inggastos_bogota <- merge(c, ing_gastos)
library(dplyr)# instalar esta libreria para utilizar %>%
tabla_inggastos_bogota <- tabla_inggastos_bogota %>% select(directorio_hog,directorio,nom_loc,
                                                            estrato, ingreso, gasto)

#Calcule lo que se indica posterior al código con la tabla tabla_inggastos_bogota generada en el paso anterior.

#a.Calcule el número de hogares, el número de personas, el ingreso promedio, la mediana y el percentil 25 desagregado por localidad. Ordene la tabla de forma descendente.

por_localidad <- group_by(tabla_inggastos_bogota, nom_loc)
resumenp4 <- summarise(por_localidad, num_hogares = n(), promedio_ingreso = mean(ingreso, na.rm = TRUE), 
                       mediana = median(ingreso,na.rm = TRUE ), p25 = quantile(ingreso, 0.25, na.rm = TRUE))
resumenp4 <- arrange(resumenp4, desc(num_hogares))


#En la localidad de Chapinero calcule el promedio de los diferentes estratos socioecónómicos y la 
#correlación entre el ingreso y el gasto.

chapinero <- filter(tabla_inggastos_bogota, nom_loc == "Chapinero")
por_estrato <- group_by(chapinero, estrato)
resumenp4b <- summarise(por_estrato, promedio_ingreso = mean(ingreso, na.rm = TRUE), 
                        promedio_gasto = mean(gasto, na.rm = TRUE))
resumenp4b$correlacion <- cor(resumenp4b$promedio_ingreso, resumenp4b$promedio_gasto)

#Construya una función para calcular el coeficiente de variación y presente por estrato el promedio,
#la desviación estándar, el coeficiente de variación, la asimetría y la curtosis.
#Excluya de la tabla los que no reportaron el estrato (estrato !=9)

coefvar<- function(x){
  100 * sd(x) / mean(x)
}
library(moments) # libreria utilizada para calcular la curtosis y la asimetria
tabla<- filter(tabla_inggastos_bogota, estrato != 9 & is.na(ingreso) == FALSE) # dejar en la tabla por_estrato3 los valores con estrato diferente de 9 y quitando los valores NA de la variable ingreso
por_estrato2 <- group_by(tabla, estrato)
resumen <- summarize(por_estrato2,  sd_ingreso = sd(ingreso), cv_ingreso = coefvar(ingreso), 
                     asimetria_ingreso = skewness(ingreso),
                     curtosis_ingreso = kurtosis(ingreso))

#Calcule el ahorro por estrato socieconómico para la localidad de Suba y reporte la localidad 
#con mayores ingresos.
resumen1 <- arrange(resumen, desc(promedio_ingreso))
# de acuerdo a la consulta realizada la localidad con un promedio mayor de ingresos es Chapinero

suba <- filter(tabla_inggastos_bogota, nom_loc == "Suba")
por_estratosuba <- group_by(suba, estrato)
por_estratosuba$ahorro <- por_estratosuba$ingreso - por_estratosuba$gasto
resumen2 <- summarise(por_estratosuba, prom_ahorro = mean(ahorro, na.rm = TRUE))
#Cargue las pruebas saber con el siguiente código:
library(haven)
saber_2015II <- read_spss("saber2015IIsav.sav")
#Lleve a cabo las siguientes consultas:
#5.  Busque como le fue a su colegio en las pruebas saber: Consejo use arrange 
#(ordene por municipio y nombre de la institución) y genere un consecutivo para saber el ranking.
#Si no estudio en Colombia o su colegio no aparece en el listado use el Colegio Antonio Nariño H.H Corazonista.
table(saber_2015II$DEPARTAMENTO)
col_bogota <- filter(saber_2015II, DEPARTAMENTO == "BOGOTA" & NOMBREINSTITUCION == "COL DE LA PRESENTACION LUNA PARK")
names(saber_2015II)
saber_2015II$PROMTOTAL <- (saber_2015II$PROMLECTURACRITICA + saber_2015II$PROMMATEMATICA + saber_2015II$PROMSOCIALESYCIUDADANAS +
                             saber_2015II$PROMCIENCIASNATURALES + saber_2015II$PROMINGLES + saber_2015II$PROMRAZONAMIENTOCUANTITA +
                             saber_2015II$PROMCOMPETENCIASCIUDADAN)

consulta1 <- arrange(saber_2015II, desc(PROMTOTAL))
# De acuerdo a la consulta1, el colegio que ocupo el primer lugar de acuerdo al promedio total es ASPAEN LICEO TACURÍ

#6. Calcule cuantos colegios y estudiantes hay según departamento, municipio, naturaleza y jornada, y ordénelos por dichas columnas.
#agrupación por departamentos para calcular cuantos colegios y estudiante hay
saber_dpto <- group_by(saber_2015II, DEPARTAMENTO)
consulta2 <- summarise(saber_dpto, num_col = n(), n_estudiantes = sum(EVALUADOS))
consulta2<- arrange(consulta2, desc(n_estudiantes ))

#Agrupación por municipios para el número de colegios y de estudiantes, utulizando pipelines
consulta3 <- saber_2015II  %>% group_by(NOMBREMUNICIPIO) %>%
  summarize(num_col = n(), n_estudiantes = sum(EVALUADOS)) %>%
  arrange(desc(n_estudiantes))
#Agrupación por naturaleza para el número de colegios y de estudiantes, utulizando pipelines
consulta4 <- saber_2015II  %>% group_by(NATURALEZA) %>%
  summarize(num_col = n(), n_estudiantes = sum(EVALUADOS)) %>%
  arrange(desc(n_estudiantes))
#Agrupación por JORNADA para el número de colegios y de estudiantes, utulizando pipelines
consulta5 <- saber_2015II  %>% group_by(JORNADA) %>%
  summarize(num_col = n(), n_estudiantes = sum(EVALUADOS)) %>%
  arrange(desc(n_estudiantes))

#7. Calcule el puntaje global de las pruebas Saber y encuentre los 10 mejores colegios de Bogotá 
#(según el puntaje global) de jornada continua,
#que registraron más de 10 estudiantes y que sean de calendario A.

consulta6 <- saber_2015II %>% filter(DEPARTAMENTO == "BOGOTA" , JORNADA == "1", EVALUADOS > 10, CALENDARIO == "1") %>% 
  arrange(desc(PROMTOTAL))

#8. Identifique en qué departamento se presentan los mayores coeficientes de variación en el puntaje global. 
#Para responder a esto genere una tabla donde calcule el coeficiente de variación del puntaje global 
#desagregado por departamento, y ordene por el coeficiente de variación.

consulta7 <- summarize(saber_dpto, cv_ingreso = coefvar(PROMTOTAL) )

#9. Calcule la asimetría y curtosis del ingreso.
consulta8 <- summarize(saber_dpto, asimetria_PROM = skewness(PROMTOTAL),
                       curtosis_PROM = kurtosis(PROMTOTAL))

#10. Calcule una tabla de frecuencias absolutas y relativa

table(saber_2015II$JORNADA, saber_2015II$NATURALEZA)
prop.table(table(saber_2015II$JORNADA, saber_2015II$NATURALEZA), 2)