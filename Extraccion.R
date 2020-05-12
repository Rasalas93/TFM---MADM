# // --- Asignacion de Directorio --- //
cat("\014")
getwd()
#setwd("/home/brayan/Documents/TFM/R-Code")  

# // --- Librerias --- //

library(tidyverse)
library(lubridate)
library(dummy)
library(dummies)
library(plm)
library(quantmod)

#  fucion de extraccion
extraction <- function(Emp) {
  x <- getSymbols(as.character(Emp), src="yahoo", from = "2000-01-01", 
                     to = "2013-01-01", auto.assign = FALSE)
  return(x)
}

# Carga de lista de empresas...
setwd("C:/Users/rasal/Documents/MADM/TFM")
Empresas <- read.csv("Listado_Empresas_V2.csv")

# Loop de extraccion
setwd("C:/Users/rasal/Documents/MADM/TFM/Prueba")
for (i in 1:5) { ##--- todas las empresas con: 1:length(Empresas)
  x <- extraction(as.character(Empresas[i,1]))
  y <- as.data.frame(x)
  y$Date <- index(x)[1:nrow(x)]
  write.csv(y,paste(as.character(Empresas[i,1]),".csv",sep=""))
}
