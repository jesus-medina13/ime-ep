#EP02

#Librerias
library(ggpubr)
library(dplyr)
library(patchwork)

#Para mostrar ambos graficos
mat <- matrix(c(1, 2),
              nrow = 1, ncol = 2,
              byrow = TRUE)

layout(mat = mat)

#Datos
datos <- read.csv2("C:\\Users\\dvvp\\Desktop\\EP02\\EP02 Datos Casen 2017.csv")

hombres <- datos[datos$sexo == 'Hombre',]
rural <- hombres[hombres$zona == 'Rural',]
urbana <- hombres[hombres$zona == 'Urbano',]


# --- Pregunta ---
# ¿Se distribuye de igual manera la situación ocupacional de los hombres 
# que viven en áreas rurales y quienes viven en áreas urbanas de la RM?

# Se agrupan los datos para poder obtener sus frecuencias
situacion_rural <- group_by(rural,ch1) %>% summarise(count = n())
situacion_urbana <- group_by(urbana,ch1) %>% summarise(count = n())



#Graficos : Permiten visualizar de mejor manera las distribuciones

grafico_rural <- ggbarplot(situacion_rural, x = 'ch1', y = 'count', 
                           fill = c("red", "green", "yellow", "blue"), 
                           title = "Situacion de hombres en zona rural",
                           xlab = "Situacion ocupacional",
                           ylab = "Cantidad") + scale_x_discrete(labels = abbreviate)


grafico_urbana <- ggbarplot(situacion_urbana, x = 'ch1', y = 'count', 
                            fill = c("red", "green", "yellow", "blue"), 
                            title = "Situacion de hombres en zona urbana",
                            xlab = "Situacion ocupacional",
                            ylab = "Cantidad") + scale_x_discrete(labels = abbreviate)


grafico_urbana + grafico_rural

# ------  Aclaracion --------
# Los nombres de las etiquetas del eje x son abreviaturas y aunque no se entienda el nombre exacto
# estos no son relevantes para lo que se solicita responder.


cat("Respuesta: Al tratarse de una variable categórica no se utiliza la medida de desviación estandar, 
    por lo que se procede a realizar gráficas para visualizar la distribución de los datos. 
    
    En vista de los gráficos obtenidos se ve una distribucion muy similar en ambas zonas, solo varía la magnitud
    de personas en cada tipo de situación ocupacional. Por lo tanto la zona rural y urbana de la región metropolitana
    posee unas situaciones ocupacionales que se distribuyen de manera similar para el caso de los hombres.
    
    Por la tanto, se concluye que las ocupaciones de los hombres de la region metropolitana no depende del sector al que pertenezca,
    ya que se observa muy claramente que las tendencias son exactamente igual.")

