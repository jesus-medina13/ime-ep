# Ejercicio Práctico 01 (EP01)
# Grupo 04
# Integrantes:
# - DIEGO ANTONIO ARMIJO PALOMINOS
# - JAIME ALEJANDRO CARRASCO QUINTREQUEO
# - JESÚS ESTEBAN MEDINA GUEVARA
# - CARLOS FRANCISCO RETAMALES APARICIO

# Preguntas:
# ¿Qué variables se han cargado?
# La variable que se ha cargado son datos sobre los nuevos casos diarios
# de pacientes sintomáticos diagnosticados con Covid-19 de cada 
# fecha desde 2020 (03-03-2020) hasta 2022 (06-03-2022), indicando cada región de Chile
# con el nombre de columna "Region".
# ¿Qué tipo tiene cada una de estas variables?
# El tipo de variable es de tipo númericas discretas siendo este 
# el número de pacientes sintomaticos diagnoticados con Covid-19.
# ¿Qué escala parecen tener estas variables?
#  La escala de esta variable es escala de razón.

library(dplyr) #Libreria para lectura de csv
library(tidyr) #Libreria para para pasar la informacion que esta en filas a columnas y trabajar los datos

# Leemos los datos
datos <- read.csv2("C:\\Users\\jechu\\OneDrive\\Escritorio\\USACH\\IME\\IME-EP-2022-2\\EP01\\EP01 Datos Covid (1).csv")

# P1
# Filtramos por la region solicitada y el rango de fecha
# Siendo coquimbo y desde 1 de mayo de 2020 hasta el 31 de octubre de 2020
coquimbo <- datos %>% filter(Region == "Coquimbo" & date >= as.Date("2020-05-01") & date <= as.Date("2020-10-31"))

# Buscamos el dia con mayor contagio
resultadoP1 <- coquimbo %>% filter(value == max(value))
resultadoP1 <- resultadoP1[["date"]]
# Se muestra por pantalla el resultado 
# Para ver la respuesta siendo 2020-07-25 descomentar el siguiente comentario
#cat("El dia que se produjo el mayor número de casos con síntomas en la región de Coquimbo entre el 01-may-2020 y el 31-oct-2020 fue el: ",format(resultadoP1,"%m/%d/%Y"))

#P2
# Asumiendo que se refiere a la region de Coquimbo 

# Agrupamos por mes y sumamos a través del valor de contagiados por mes
resultadoP2 <- coquimbo %>% group_by(month) %>% summarise(total = sum(value))

# Para ver el resultado, descomentar la siguiente linea
#print(resultadoP2)