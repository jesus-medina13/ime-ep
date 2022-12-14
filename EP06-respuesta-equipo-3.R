library(dplyr)
library(pwr)
library(Hmisc)

#Crear dataframe
data<-read.csv2("datos.csv")

#1.-Estudios previos hab?an determinado que la proporci?n de autoras en la 
# especialidad de psiquiatr?a era de 48%. ?Respaldan estos datos tal estimaci?n?

#Determinar hipotesis 
#H0: p =  0.48
#H1: p != 0.48

#Valor nulo
p_nulo<-0.48

#Definir un nivel de confianza alfa
alfa<-0.05
#Calcular la probabilidad de exito
siquiatras_mujeres<-as.numeric(data%>%filter(Especialidad=="Psiquiatr?a")%>%select(Mujer))
siquiatras_hombres<-as.numeric(data%>%filter(Especialidad=="Psiquiatr?a")%>%select(Hombre))
cantidad_mujeres<-as.numeric(sum(data$Mujer))

cantidad_siquiatras<-siquiatras_mujeres+siquiatras_hombres


#Comprobar prueba
prueba1<-prop.test(siquiatras_mujeres,
                  n=cantidad_siquiatras,
                  alternative = "two.sided",
                  conf.level = 1-alfa)

print(prueba1)

#Existe suficiente evidencia para rechazar la hipotesis alternativa en favor a 
# la hipotesis nula con un nivel de confianza de alfa=0.05

#2.-Seg?n estos datos, ?es igual la proporci?n de autoras en las ?reas de 
# pediatr?a y radiolog?a?

#Se plantean las hipotesis
#H0: p1-p2  = 0  
#H1: p1-p2 != 0

cantidad_autoras_pediatria<- as.numeric(data %>% filter(Especialidad == "Pediatr?a")%>%select(Mujer))
cantidad_autores_pediatria<- as.numeric(data %>% filter(Especialidad == "Pediatr?a")%>%select(Hombre))

cantidad_autoras_radiologia<-as.numeric(data %>% filter(Especialidad == "Radiolog?a") %>%select(Mujer))
cantidad_autores_radiologia<-as.numeric(data %>% filter(Especialidad == "Radiolog?a") %>%select(Hombre))

cantidad_pediatria<-cantidad_autoras_pediatria+cantidad_autores_pediatria

cantidad_radiologia<-cantidad_autoras_radiologia+cantidad_autores_radiologia
n<-c(c(cantidad_pediatria,cantidad_radiologia ))

exitos<-c(cantidad_autoras_pediatria,cantidad_autoras_radiologia)
valor_nulo<-0

prueba2<-prop.test(exitos,n=n,alternative = "two.sided",conf.level=1-alfa)

print(prueba2)

#Existe suficente evidencia para rechazar la hipotesis nula, en favor de la 
#hipotesis nula con un nivel de significancia del 0.05

#3.-Suponiendo que la diferencia en la proporci?n de autoras en la especialidad  
# de radiolog?a y la de anestesiolog?a es de 0,32. ?A cu?ntos autores deber?amos 
# monitorear para obtener un intervalo de confianza del 99% y poder estad?stico 
# de 80%, si se intenta mantener aproximadamente la misma proporci?n de gente 
# estudiada en cada caso?

cantidad_autoras_anestesiologia<- as.numeric(data %>% filter(Especialidad == "Anestesiolog?a")%>%select(Mujer))
cantidad_autores_anestesiologia<- as.numeric(data %>% filter(Especialidad == "Anestesiolog?a")%>%select(Hombre))

cantidad_anestesiologos<-cantidad_autores_anestesiologia+cantidad_autoras_anestesiologia

p1<-cantidad_autoras_anestesiologia/cantidad_anestesiologos
p2<-cantidad_autoras_radiologia/cantidad_radiologia


# En la p?gina 9 del cap?tulo 7 se sugiere usar p2 = 0.5 cuando
# tenemos una proporci?n (en este caso p1 ser?a la diferencia en la proporci?n)

h <- ES.h(0.32, 0.5)

pwr <- pwr.p.test(h = h,
           n=NULL,
           sig.level = 0.01,
           power=0.8,
           alternative = "two.sided")

# Redondeamos el tama?o obtenido 
n <- ceil(pwr$n)

# Asignar las cantidades que corresponden a cada especialidad, seg?n el n obtenido
n1 <- 40
n2 <- n - n1

# Calcular la cantidad de especialistas mujeres para que sigan la proporci?n
nueva_cantidad_anestesiologas <- p1*n1
nueva_cantidad_radiologas <- p2*n2
