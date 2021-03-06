rm(list = ls())

# Gr�fico ggplot2

# En esta sesi�n se llevar�n a cabo la elaboraci�n de gr�ficos con ggplot2
setwd("D:/Gdrive/Laboral 2016/DANE_INNOVACION/Curso an�lisis de datos con R/cursor_2016II/Curso/Bases/datos_curso/Mod4")
dir()


load("EMB2011.RData")

# M�dulos de viviendas: a, b
# localidad: contiene informaci�n de la localidad
# a: contiene informaci�n de las localidades, el estrato
# b: contiene informaci�n de la vivienda
# c: contiene informaci�n del hogar
# d: contiene informaci�n del hogar
# e: contiene informaci�n de ingresos y gastos del hogar

# Ojetivo calcular el promedio del ingreso por estrato y visualizar
# Calcular la distribuci�n del estrato por localidad


# Se pegar� el nombre de la localidad al m�dulo a:

head(a)
head(localidad)

a <- merge(a, localidad, by = "localidad") # by el nombre de la llave primaria

# Ahora se proceder� pegarle la informaci�n del m�dulo a la tabla de
# ingresos y gastos
intersect(names(localidad), names(ing_gastos))

# Importante: !No hay variables en com�n entre estas dos bases! 

# El identificador de a es directorio, el de  ing_gastos es  directorio_hog, 
# Observe qu el m�dulo c contiene el directorio y el directorio_hog
# S�lo usaremos esas dos variables de la tabla c
datos <- merge(a, c[c("directorio_hog", "directorio")])

# M�dulos de hogares: c, d, ing_gastos
datos <- merge(datos, ing_gastos)


# Conservamos las variables de inter�s:

datos <- datos[c("directorio_hog", "directorio", "localidad", "nom_loc", "estrato",
                  "ingreso", "gasto")]


# El histograma del ingreso en Bogot�
ggplot(datos, aes(x=ingreso)) + geom_histogram()
ggplot(datos, aes(x=ingreso)) + geom_histogram() + xlim(0, 10000000)

ggplot(datos, aes(x=ingreso)) + geom_histogram(aes(y = ..density..)) +
  geom_density(col = "red") +  xlim(0, 10000000)

# El boxplot por estrato

# http://academic.depauw.edu/~hanson/R-Pages/Basic.qplot.pdf
qplot(x = factor(estrato), y = ingreso, data = datos, geom = "boxplot",
      xlab = "Estrato", col = factor(estrato))


 
# Calculemos el ingreso promedio por estrato

aggregate(ingreso ~ estrato, FUN = mean , data = datos)

# Esto se ve mejor en un gr�fico

# el primer argument de ggplot es la base de datos
# aes: se coloca las variables que van en el eje x, y los colores indican que 
# se realizar�n diferentes gr�ficas por estrato.
library(ggplot2)
ggplot(datos, aes(ingreso, colour = factor(estrato))) +   geom_density()

# Cambiar la escala
ggplot(datos, aes(ingreso, colour = factor(estrato))) +   geom_density() +
   xlim(0, 10000000)

# Quitar el estrato 9
datos2 <- subset(datos, estrato!=9)
ggplot(datos2, aes(ingreso, colour = factor(estrato))) +   geom_density() +
  xlim(0, 10000000)

ggplot(datos2, aes(factor(estrato), ingreso)) +  geom_boxplot() 
  
ggplot(datos2, aes(factor(estrato), ingreso, col = factor(estrato))) +  geom_boxplot() 

ggplot(datos2, aes(factor(estrato), ingreso, col = factor(estrato))) +  geom_boxplot() +
geom_jitter()

ggplot(datos2, aes(factor(estrato), ingreso, col = factor(estrato))) +  geom_boxplot() +
  geom_jitter(alpha = 0.1)

# Distribuci�n por estrato

ggplot(datos2, aes(factor(estrato))) + geom_bar() + xlab("Estrato") + ylab("Frec. abs")


ggplot(datos2, aes( factor(estrato), fill = factor(estrato)) ) + geom_bar() + 
  xlab("Estrato") + ylab("Frec. abs")

# Estrato por localidad

ggplot(datos2, aes(factor(localidad), fill=factor(estrato))) + geom_bar()

ggplot(datos2, aes(factor(nom_loc), fill=factor(estrato))) + geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = 1) 

# Reto ordenar por la frecuencia de la localidad (ver m�s adelante pistas)

ggplot(datos2, aes(factor(localidad), fill=factor(estrato))) + geom_bar() +
coord_flip()

ggplot(datos2, aes(factor(localidad), fill=factor(estrato))) + geom_bar() +
scale_fill_brewer(palette = 12) 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

# Ordenar por estrato
datos2$estratoR <- factor(datos2$estrato,
                    levels = 6:1)

ggplot(datos2, aes(factor(localidad), fill=factor(estratoR))) + geom_bar() +
  scale_fill_brewer(palette = 1) 

# Diagrama de barras agrupados

ggplot(datos2, aes(factor(localidad), fill=factor(estrato))) + 
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = 12) + xlab("Estrato") +
  ylab("Frecuencia absoluta")

# C�mo se realiza el diagrama de barras desde una tabla:
cons <- aggregate(ingreso ~ factor(estrato), data = datos2, FUN = length)
names(cons)[1] <- "Estrato" 

names(cons)[2] <- "Conteo" 


# Diagama de barras del ingreso promedio

ggplot(cons, aes(x = factor(Estrato), y = Conteo)) + 
  geom_bar(stat="identity")

tf <- data.frame(table(datos2$estrato))
ggplot(tf, aes(x = factor(Var1), y = Freq)) + 
  geom_bar(stat="identity")

