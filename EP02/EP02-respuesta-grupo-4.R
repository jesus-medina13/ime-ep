#Ejercicio Práctico (EP02)
# Grupo 04
# Integrantes:
# - DIEGO ANTONIO ARMIJO PALOMINOS
# - JAIME ALEJANDRO CARRASCO QUINTREQUEO
# - JESÚS ESTEBAN MEDINA GUEVARA
# - CARLOS FRANCISCO RETAMALES APARICIO

#Fijamos la carpeta de trabajo.
setwd("C:\\Users\\jechu\\OneDrive\\Escritorio\\USACH\\IME\\IME-EP-2022-2\\EP02")

#Importar paquetes.
library(ggpubr)
library(dplyr)
library(patchwork)
library(ggplot2)

#Información Previa

#Los datos que se han cargado son 28063 variables, donde la primera fila es 
#información del Cadem, siendo folio, edad, sexo, zona, etc.
#Siendo estos datos extraidos del Casen de 2017.

#La primera fila tiene variables categoricas, mientras que las restantes
#son variables que van ir variando dependiendo de la variable categorica
#(enteras no negativas, carácter, entre otros).

#Para mostrar ambos graficos.
mat <- matrix(c(1, 2),
              nrow = 1, ncol = 2,
              byrow = TRUE)

layout(mat = mat)

#Cargar datos.

datos <- read.csv2("EP02 Datos Casen 2017.csv")

#Se selecciona los datos a usar.

hombres <- datos[datos$sexo == 'Hombre',]
rural <- hombres[hombres$zona == 'Rural',]
urbana <- hombres[hombres$zona == 'Urbano',]


# --- Pregunta ---
# ¿Se distribuye de igual manera la situación ocupacional de los hombres 
# que viven en áreas rurales y quienes viven en áreas urbanas de la RM?

# Se agrupan los datos para poder obtener sus frecuencias.

situacion_rural <- group_by(rural,ch1) %>% summarise(count = n())
situacion_urbana <- group_by(urbana,ch1) %>% summarise(count = n())



#Graficos : Permiten visualizar de mejor manera las distribuciones.

grafico_rural <- ggbarplot(situacion_rural, x = 'ch1', y = 'count', 
                           title = "Situación de hombres en zona rural",
                           xlab = "Situación ocupacional",
                           ylab = "Cantidad") + scale_x_discrete(labels = abbreviate)


grafico_urbana <- ggbarplot(situacion_urbana, x = 'ch1', y = 'count', 
                            title = "Situación de hombres en zona urbana",
                            xlab = "Situacion ocupacional",
                            ylab = "Cantidad") + scale_x_discrete(labels = abbreviate)

#Se muestran los graficos.

grafico_urbana + grafico_rural

#Se adjunta tambien graficos de densidad para ver el contorno de esta distribución

grafico_d_rural <- ggplot(situacion_rural, aes(x=count)) + geom_density() + scale_y_continuous("Densidad") + scale_x_continuous("Cantidad")
grafico_d_urbana <- ggplot(situacion_urbana, aes(x=count)) + geom_density() + scale_y_continuous("Densidad") + scale_x_continuous("Cantidad")

#Se muestran los graficos.

#grafico_d_rural + grafico_d_urbana


# ------  Aclaracion --------
# Los nombres de las etiquetas del eje x son abreviaturas y aunque no se entienda el nombre exacto
# estos no son relevantes para lo que se solicita responder.


cat("Respuesta: Al tratarse de una variable categórica no se utiliza la medida de desviación estandar, 
    por lo que se procede a realizar gráficas para visualizar la distribución de los datos. 
    
    En vista de los graficos obtenidos se ve una distribución muy similar en ambas zonas, solo varía la magnitud
    de personas en cada tipo de situación ocupacional. Por lo tanto la zona rural y urbana de la región metropolitana
    posee unas situaciones ocupacionales que se distribuyen de manera similar para el caso de los hombres.
    
    Por la tanto, se concluye que las ocupaciones de los hombres de la región metropolitana no depende del sector al que pertenezca,
    ya que se observa muy claramente que las tendencias son exactamente igual.")

