#Ejercicio Práctico 3 (EP03)
# Grupo 04
# Integrantes:
# - DIEGO ANTONIO ARMIJO PALOMINOS
# - JAIME ALEJANDRO CARRASCO QUINTREQUEO
# - JESÚS ESTEBAN MEDINA GUEVARA
# - CARLOS FRANCISCO RETAMALES APARICIO

#Fijamos la carpeta de trabajo.
setwd("E:\\OneDrive - usach.cl\\2022-2\\8°Semestre\\IME\\IME-EP-2022-2")

#Importar paquetes
library(ggpubr)
library(dplyr)
library(discreteRV)
library(gtools)


# ----------- ACTIVIDAD 2 ----------
# Definan su propia semilla y obtengan 5.000 casos para una distribución de ingresos aproximadamentenormal.

población <- read.csv2("EP03 Datos Casen 2017.csv")
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

set.seed(4567)
grafico.normal <- ggqqplot(ingreso.normal, 
                           title = "Ingreso Normalizado (Distribucion Normal)",
                           color = "red",
                           ylab = "Ingreso")
#Gráfico.
grafico.normal

#ACTIVIDAD 3

#A partir de la distribución conseguida, y sin usar nuevamente la función rnorm(), generen la
#correspondiente distribución Z.

#Apartir del ingreso normal, generamos la distribución z usando la media del ingreso y dividendolo con la desviación estandar.

ingreso.estandarizado <- (ingreso.normal - media.ingreso)/sd.ingreso
grafico.estandarizado <- ggqqplot(ingreso.estandarizado, 
                                  title = "Ingreso Estandarizado",
                                  color = "blue",
                                  ylab = "Ingresos")

#Gráfico.
grafico.estandarizado

#ACTIVIDAD 4
#Con la distribución Z obtenida en el punto anterior, y sin utilizar funciones como rchisq(), construyan
#dos distribuciones χ2, cada una con más de 3 y menos de 15 grados de libertad.

#Grados de libertad.
gl1 = 7
gl2 = 11

#Realizamos la distribuciones chi cuadrado a partir de la suma del ingreso estandarizado, el grado de libertad y elevarlo al cuadrado.

distribucion.chi1 <- function(x) { resultado <- sum(sample(ingreso.estandarizado, gl1)^2)
return(resultado) }
distribucion.chi2 <- function(x) { resultado <- sum(sample(ingreso.estandarizado, gl2)^2) 
return(resultado)}

chi1 <- sapply(1:1000, distribucion.chi1)
chi2 <- sapply(1:1000, distribucion.chi2)

#Para graficar con ggqqplot se deben transformar los vectores en dataframe.
df1 <- data.frame(chi1)
df2 <- data.frame(chi2)


grafico.chi1 <- ggplot(df1, aes(x = chi1)) +
  geom_density() +
  ggtitle("Distribución Chi 1") +
  xlab("Chi 1") + ylab("Densidad")

grafico.chi2 <- ggplot(df2, aes(x = chi2)) +
  geom_density() +
  ggtitle("Distribucion Chi 2") +
  xlab("Chi 2") + ylab("Densidad")

#Gráficos.
grafico.chi1
grafico.chi2

#ACTIVIDAD 5 (Distribucion F)

#Usando las dos distribuciones χ2 generadas en el punto anterior, construyan una distribución F.

ingreso.f <- (chi1/gl1)/(chi2/gl2)

df3 <- data.frame(ingreso.f)

grafico.f <- ggplot(df3, aes(x = ingreso.f)) +
  geom_density() + 
  ggtitle("Distribucion F") +
  xlab("Valores F") + ylab("Densidad")

#Gráfico.
grafico.f

#ACTIVIDAD 6
#Se encuentra bajo cada comentario de "Gráfico" para ver donde se hace un llamado a cada gráfico.

#ACTIVIDAD 7

#El siguiente código R muestra la obtención de 20 repeticiones de un ensayo de Bernoulli con éxito si se elige la
#encuesta de una mujer de entre los datos seleccionados desde la encuesta Casen 2017.
#Definan su propia semilla y número de repeticiones para el ensayo.


set.seed(1322)
n.repeticiones <- 20

ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)

veinte.repeticiones <- sapply(1:n.repeticiones, ensayo)


#Generamos la probabilidad de que una mujer sea escogida de la muestra.

cantidad_m <- sum(veinte.repeticiones)
p <- sum(cantidad_m)/n.repeticiones

# ACTIVIDAD 8 (DISTRIBUCIÓN BINOMIAL)

#Generen, sin utilizar funciones como rbinom(), una distribución binomial.

#Se genera la distribucion binomial, usando la formula correspondiente.

d_binomial<-list()
  for(i in 1:n.repeticiones){
  d_binomial<-c(d_binomial,choose(n.repeticiones,i)*p^i*(1-p)^(n.repeticiones-i))
}

df_binomial <- data.frame("K"=c(1:n.repeticiones),"Probabilidad"=unlist(d_binomial))
grafico.d_binomial<-ggbarplot(df_binomial,x="K",y="Probabilidad")

#Descomentar para ver el grafico.
grafico.d_binomial

#ACTIVIDAD 9

#Similarmente, construyan una distribución geométrica.

# Se Genera una distribuciÓn geometrica.
d_geometrica<-list()
for (i in 1:n.repeticiones){
  d_geometrica<-c(d_geometrica, (1-p)^(i-1)*p)
}

df_geometrica <- data.frame("K"=c(1:n.repeticiones),"Probabilidad"=unlist(d_geometrica))
grafico.geometrica<-ggbarplot(df_geometrica,x="K",y="Probabilidad")

#Gráfico.
grafico.geometrica

#ACTIVIDAD 10

#Análogamente, generen una distribución binomial negativa.

#se genera un distribucion binombial negativa.
d_binomial.negativa<-list()
for (i in 1:n.repeticiones){
  pr<-choose(n.repeticiones-1,i-1)*p^(i)*(1-p)^(n.repeticiones-i)
  d_binomial.negativa<-c(d_binomial.negativa,pr)
}

df_binomial.negativa <- data.frame("K"=c(1:n.repeticiones),"Probabilidad"=unlist(d_binomial.negativa))
grafico.d_binomial.negativa<-ggbarplot(df_binomial.negativa,x="K",y="Probabilidad")

#Gráfico.
grafico.d_binomial.negativa


