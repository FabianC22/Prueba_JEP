

### Librerias 
library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)
library(scales)

### Carga de bases

A <- read_excel("C:/Users/fabia/OneDrive - Universidad Nacional de Colombia/UNAL/JEP_Pureba/Prueba_Tecnica/Prueba_Tecnica/input/A.xlsx")
B <- read_excel("C:/Users/fabia/OneDrive - Universidad Nacional de Colombia/UNAL/JEP_Pureba/Prueba_Tecnica/Prueba_Tecnica/input/B.xlsx")

# 1.
# Agregar Columna "fuente"

A$fuente<- "A"
B$fuente<- "B"

# Unir los Data frame
General<-rbind(A,B)


# Crear una columna que indique si el Número de documeto  está repetido
General$Identificador<- duplicated(General$NUMERO_DOCUMENTO) | duplicated(General$NUMERO_DOCUMENTO, fromLast = TRUE)

#2. Analisis

# Departamento 


A_sin_duplicados <- A[!duplicated(A$NUMERO_DOCUMENTO), ]
B_sin_duplicados <- B[!duplicated(B$NUMERO_DOCUMENTO), ]
General_dep<-rbind(A_sin_duplicados ,B_sin_duplicados)


g = ggplot(General_dep, aes(DEPARTAMENTO, fill=fuente) ) +
  labs(title = "Reportes por Departamento")+ylab("Número de reportes") +
  theme(plot.title = element_text(size = rel(2), colour = "blue"))+ coord_flip()

g+geom_bar(position="dodge") + scale_fill_manual(values = alpha(c("orange", "blue"), 1)) +
  theme(axis.title.x = element_text(face="bold", size=10))   

# Municipio

g1 = ggplot(General, aes(MUNICIPIO, fill=fuente) ) +
  labs(title = "Reportes por Municipio")+ylab("Número de reportes") +
  theme(plot.title = element_text(size = rel(2), colour = "blue"))+ coord_flip()

g1+geom_bar(position="dodge") + scale_fill_manual(values = alpha(c("orange", "blue"), 1)) +
  theme(axis.title.x = element_text(face="bold", size=10))   

# Sexo

g2 = ggplot(General_dep, aes(SEXO, fill=fuente) ) +
  labs(title = "Reportes por Sexo")+ylab("Número de reportes") +
  theme(plot.title = element_text(size = rel(2), colour = "blue"))

g2+geom_bar(position="dodge") + scale_fill_manual(values = alpha(c("orange", "blue"), 1)) +
  theme(axis.title.x = element_text(face="bold", size=10))   

### Tabla de contingencia 

kable(table(General$NUMERO_DOCUMENTO,General$fuente))

g3 = ggplot(General, aes(as.character(NUMERO_DOCUMENTO), fill=fuente) ) +
  labs(title = "Reportes por Municipio")+ylab("Número de reportes") +
  theme(plot.title = element_text(size = rel(2), colour = "blue"))+ coord_flip()

g3+geom_bar(position="dodge") + scale_fill_manual(values = alpha(c("orange", "blue"), 1)) +
  theme(axis.title.x = element_text(face="bold", size=10))   

