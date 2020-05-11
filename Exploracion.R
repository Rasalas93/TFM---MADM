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
# // --- Carga de tablas --- //

# --- Base de politicos y sus cargos en las Empresas --- 
Politcs <- read.csv("Polit_Empresas_v2.csv")

# --- Base de Encuestas del CIS --- 
Intention <- read.csv("Intencion_voto_v2.csv")
# Intention2 <- read.csv("Intencion_voto_v2.csv")[,1:3]

# --- Modelo de mercado: IBEX35 --- 
IBEX <- read.csv("^IBEX.csv")

# --- Bases de empresas --- 
setwd("/home/brayan/Documents/TFM/Bases_Empresas")  
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)
names(myfiles) <- temp
temp <- as.data.frame(temp)

# write_csv(temp,'emps1.csv')

# for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) # --- Lectura Independiente de cada dataframe

# // --- Analisis de Datos --- //
# --- Ajustes de Variables ---
str(Politcs$Partido)
unique(Politcs$Partido)
names(Intention)
summary(Politcs$Partido)

Intention$Resultados <- as.Date(Intention$Resultados,"%d/%m/%Y")
Intention$Nota.Presa <- as.Date(Intention$Nota.Presa,"%d/%m/%Y")
names(Intention)[c(2,8)] <- c("Date","CIU")
Intention <- Intention[order(Intention$Date),]

# --- Creacion de variables de diferencias del CIS ---
Delta.CIS <- as.data.frame(apply(Intention[3:ncol(Intention)], 2, diff))
Delta.CIS <- rbind(rep(0, ncol(Delta.CIS)),Delta.CIS)
Delta.CIS <- cbind(Intention$Nota.Presa,Intention$Date, Delta.CIS)
names(Delta.CIS)[1:2] <- c("Nota.Presa","Date")
nrow(Delta.CIS)==nrow(Intention)

# --- Bases de empresa Test ---

Data <- myfiles$`0NPV.IL.csv`
Data$EMP <- temp[1,]
Data$Delta.Price.day <- c()
# Data$Close <- Data$Adj.Close #-------------- Si quiero considerar el valor ajustado
Data[Data$Close == 0,]$Close <- (Data[Data$Close == 0,]$High + Data[Data$Close == 0,]$Low)/2

Data$Delta.Price.day[1] <- 0
Data$Delta.Price.day[2:nrow(Data)] <- diff(Data$Close)
# Data[is.na(Data$Close),] %>% view()
is.na(Data$Close) %>% sum

for (i in 2:length(myfiles)) {
  EMP <- as.data.frame(myfiles[temp[i,]])
  EMP$EMP <- temp[i,]
  # EMP[,5] <- EMP[,6] #-------------- Si quiero considerar el valor ajustado
  EMP$Delta.Price.day <- c()
  EMP$Delta.Price.day[1] <- 0
  names(EMP) <- names(Data)
  EMP[,c(2:7)] <- apply(EMP[,c(2:7)], 2, as.double)
  #
  if(sum(is.na(EMP$Close)) > 0) {
    EMP <- EMP[!is.na(EMP$Close),]
  }
  # EMP[EMP[,5] == 0,][,5]
  if (sum(EMP[,5] == 0) > 0) {
    EMP[EMP[,5] == 0,][,5] <- (EMP[EMP[,5] == 0,][,4] + EMP[EMP[,5] == 0,][,3])/2 # calcular algunos valores faltantes
  }
  EMP$Delta.Price.day[2:nrow(EMP)] <- diff(EMP$Close)
  Data <- rbind(Data,EMP)
}
Data

is.na(Data$Close) %>% sum
sum(Data$Close == 0)
Data$Date <- as.Date(Data$Date,"%Y-%m-%d")
Data$Delta.Price <- Data$Close - Data$Open

EMP <- within(data=as.data.frame(Data$EMP), EMP<-data.frame(do.call('rbind',strsplit(as.character(Data$EMP),".",fixed=TRUE))))
Data$EMP <- as.character(EMP$EMP[,1])
Data$EMP <- as.factor(Data$EMP)
str(Data)
Data <- left_join(Data, Delta.CIS, by = 'Date')
Data$Enquesta <- if_else(!is.na(Data$Nota.Presa)==T, 1,0)
Data$VAR1 <- c()
Data$VAR1[1] <- 0
for(i in 2:(nrow(Data)-1)){
  if ((Data$Enquesta[i]||Data$Enquesta[i-1])==TRUE) {
    Data$VAR1[i] <- 1
  }else{
    Data$VAR1[i] <- 0
  }
}
Politcs1 <- Politcs %>% filter(Acronimo != "#N/A" & Acronimo != "0") %>% 
  select(Ejercicio,Partido,Acronimo)
Politcs1 <- Politcs1[!duplicated(Politcs1),]
Politcs1<-cbind(Politcs1,dummies::dummy(Politcs1$Partido))
unique(Politcs1$Partido)
Pol <- Politcs1 %>% group_by(Ejercicio,Acronimo) %>% summarise(PSOE = max(Politcs1PSOE),
                                                               PP = max(Politcs1PP),
                                                               UCD = max(Politcs1UCD),
                                                               PNV = max(Politcs1PNV),
                                                               CIU = max(Politcs1CIU),
                                                               `DESIGNACION REAL` = max(`Politcs1DESIGNACION REAL`),
                                                               ERC = max(Politcs1ERC),
                                                               UPN = max(Politcs1UPN),
                                                               UI = max(Politcs1UI))
Data$year <- year(Data$Date)
Data <- left_join(Data,Pol, by= c("year"="Ejercicio","EMP"="Acronimo"))

# // --- Calculo de variables dependientes (2) --- //
Data$Delta.Price.part <- Data$Delta.Price/Data$Open
Data$Delta.Price.day.part <- Data$Delta.Price.day/(Data$Close-Data$Delta.Price.day)
Data$Delta.Price.T <- log(Data$Adj.Close/Data$Open)
Data$Delta.Price.day.T <- 0
Data$Delta.Price.day.T[2:nrow(Data)] <- log(Data$Close[2:nrow(Data)])-log(Data$Close[2:nrow(Data)]-Data$Delta.Price.day[2:nrow(Data)])

Data1 <- Data[,-c(1,11)]
Data1[is.na(Data1)] <- 0
Data[,-c(1,11)] <- Data1

# // --- Inclusion del market model, IBEX35 --- //
IBEX %>% str
IBEX$Date <- as.Date(IBEX$Date,"%Y-%m-%d")
IBEX[,c(2:ncol(IBEX))]<- apply(IBEX[,c(2:ncol(IBEX))],2,as.double)
IBEX$Diff <- 0
IBEX$Diff[2:nrow(IBEX)] <- diff(log(IBEX$Adj.Close))
Data <- left_join(Data, IBEX[,c(1,8)], by = 'Date') #IBEX[,c(1,5)] Ibex normal and IBEX[,c(1,8)] to log

names(Data)[ncol(Data)] <- "IBEX"

# (Data %>% filter(EMP == 'SAN' & Date == '2007-02-22') %>% select(12:36))==0

# Data1 <-Data %>% filter(Enquesta==1)
setwd("/home/brayan/Documents/TFM/R-Code")
write_csv(Data,'Panel2.csv')

