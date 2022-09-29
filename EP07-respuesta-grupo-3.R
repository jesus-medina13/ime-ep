#Grupo 3
#Diego Armijo
#Jaime Carrasco
#Jesús Medina
#Benjamín Sagredo

library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

#1.-Una plataforma de streaming desea estudiar si hay diferencia en la popularidad de que gozan las series de
# superhéroes entre adolescentes y entre adultos jóvenes. Tras analizar los datos de un grupo de usuarios
# conformado por 12 adolescentes y 15 adultos jóvenes, ha comprobado que 8 de los primeros y 5 de los
# segundos son seguidores de este tipo de series. ¿Cuál debería ser la conclusión de este estudio?

#H0: adolescentes y adultos jóvenes comparten de igual manera el gusto por las películas de superhéroes
#H1: adolescentes y adultos jóvenes no comparten de igual manera el gusto por las películas de superhéroes

adolescentes<-c(8,4)
adulto_joven<-c(5,10)

tabla<-as.table(rbind(adolescentes,adulto_joven))

dimnames(tabla)<-list(etario=c("adolescentes","adulto joven"),
                      preferencia=c("Seguidor","No seguidor"))
print(tabla)

prueba1<-fisher.test(tabla)
print(prueba1)

#Con un nivel de significancia alfa=0.05 existe suficiente evidencia para 
#aceptar la hipótesis nula. Es decir los adolescentes y adultos jóvenes 
#comparten gustos por las películas de superheroes

#2.-En un estudio acerca del efecto de la deficiencia de vitamina B1 durante la infancia temprana Katz, Haltus &
# Friedmann (2022) (Journal of Neurolinguistics, 62(5), 101042), se ha concluido que tal carencia se traduce en
# severos problemas de lenguaje. Un nuevo estudio busca verificar este hallazgo, para lo cual ha evaluado el
# desarrollo del lenguaje a los 5 años de vida de 35 parejas de gemelos idénticos donde, por razones médicas,
# los bebés son alimentados con diferentes fórmulas (una carente de vitamina B1 y la otra, no). Los datos
# registrados muestran que:
#   -En 10 parejas de gemelos, ninguno presenta trastornos del lenguaje.
#   -En 2 parejas de gemelos, ambos presentan trastornos del lenguaje.
#   -En 6 parejas de gemelos, solo aquel que fue alimentado con la fórmula que sí contiene vitamina B1
#     desarrolla trastornos del lenguaje.
#   -En las 17 parejas de gemelos restantes, solo el gemelo con carencia de vitamina B1 presenta trastornos
#     del lenguaje.
# ¿Soportan los nuevos datos la conclusión del estudio original?


#Establecer hipótesis
#H0: No existen cambios en los trastornos del lenguaje al alimentar a uno de los gemelos con proteína B1
#H1: Existen cambios en los trastornos del lenguaje al alimentar a uno de los gemelos con proteína B1
gemelos<-seq(1:35)
alimentadoB1<-c(rep("Sin problema",10),rep("Problema",2),rep("Problema",6),rep("Sin problema",17))
sinB1<-c(rep("Sin problema",10),rep("Problema",2),rep("Sin problema",6),rep("Problema",17))

datos2<-data.frame(gemelos,alimentadoB1,sinB1)

tabla2<-table(sinB1,alimentadoB1)

prueba2<-mcnemar.test(tabla2)

print(prueba2)

#Con un nivel de significancia de alfa =0.05 existe suficiente evidencia para 
#rechazar la hipótesis nula.Por ende se puede establecer que los investigadores 
#acertaron en el estudio realizado

#3.-En su permanente afán de comprender mejor a sus enemigos eternos, los vampiros, Van Helsing desea
# saber si existe relación entre el continente de procedencia de estos seres y su tipo de sangre predilecto.
# ¿Qué puede concluir a partir de los datos recolectados?

#Establecer hipótesis 
#H0: Las variables lugar de origen y tipo de sangre predilecto son independientes
#H1: Las variables lugar de origen y tipo de sangre predilecto están relacionadas

europa<-c(14,25,10,3)
america<-c(8,13,11,16)

tabla3<-as.table(rbind(europa,america))
dimnames(tabla3)<-list(lugar=c("Europa","America"),
                       tipo=c("A","B","AB","O"))
prueba3<-fisher.test(tabla3,0.05)

print(prueba3)

tabla3_1 <- as.data.frame(tabla3) # Para dejarlo como dataframe

#Con un nivel de significancia de alfa 0.05 existe suficiente evidencia para
#rechazar la hipótesis nula, por ende se puede establecer una relacion entre
#el tipo de sangre que prefieren los vampiros con el lugar de origen del vampiro

#4.-La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes en
# asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3
# asignaturas, indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad?


#Establecer hipótesis
#H0: La proporción de estudiantes aprobados es igual en todos los ramos
#H1: La proporción de estudiantes aprobados es distinta en al menos un ramo

archivo<-read.csv2("C:\\Users\\dvvp\\Desktop\\EP07-Grupo3\\EP07 Datos.csv")
set.seed(278)
datos3<-sample_n(archivo,50)
datos3<-datos3%>%pivot_longer(c("Calculo","Algebra","Fisica"),
                             names_to="Asignatura",
                             values_to="Estado")
datos3[["Asignatura"]]<-factor(datos3[["Asignatura"]])
datos3[["Estado"]]<-factor(datos3[["Estado"]])

prueba4<-cochran.qtest(Estado~Asignatura|Id,data=datos3,alpha=0.05)
print(prueba4)

#Con un nivel de significancia de alfa=0.05 existe suficiente evidencia para 
#rechazar la hipótesis alternativa. Por ende se puede concluir que la proporción
#de aprobación de los 3 ramos es similar.