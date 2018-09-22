#----------------------------------------------------------------------------------------
# Carga de librerias
#----------------------------------------------------------------------------------------
library(bsts)
library(lubridate)
library(MLmetrics)
library(ggplot2)
library(scales)
library(dplyr)
#----------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
mesesParaProcesar <- c(2,3,4,5,6,7,8,9,10,11)
#----------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
# Aqui guardaremos las metricas
#----------------------------------------------------------------------------------------
dfMetrics <- data.frame(Modelo=character(),
                        Mes=integer(),
                        MAE=double(), 
                        MAPE=double(),
                        MSE=double(), 
                        RMSE=double(), 
                        stringsAsFactors=FALSE) 
#----------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------
# Carga del dataset y creación de variables
#----------------------------------------------------------------------------------------
setwd("C:\\Users\\Admin\\Documents\\!!!!! UAH\\UAH-TFM-master_V2\\UAH-TFM-master\\data")
data0 <- read.table("SerieTotal2016_ext_top100.csv", sep=",", header=T)

data0$timestamp <- ymd_hms(data0$timestamp)
data0$Year      <- year(data0$timestamp)
data0$Month     <- month(data0$timestamp)
data0$Day       <- day(data0$timestamp)
data0$WeekDay   <- weekdays(data0$timestamp)

str(data0)
#----------------------------------------------------------------------------------------





#----------------------------------------------------------------------------------------
# proceso_sin_regresores
#----------------------------------------------------------------------------------------
proceso_sin_regresores <- function(element_key, mesHasta) {
  
  ek <- element_key
  data     <- subset(data0, with(data0, as.numeric(data0$element_key) == ek & data0$Month <= mesHasta - 1))
  dataTest <- subset(data0, with(data0, as.numeric(data0$element_key) == ek & data0$Month == mesHasta))
  newdata  <- subset(data0, with(data0, as.numeric(data0$element_key) == ek & data0$Month == mesHasta))
  
  y = data$occupation_perc
  
  #ss <- AddLocalLinearTrend(list(), y) # Añado un estado trend y estados de 7 dias y 24 horas para que entienda que se trata de semanas y dias
  ss <- AddAr(list(), y)
  ss <- AddSeasonal(ss, y, nseasons = 24)
  ss <- AddSeasonal(ss, data$occupation_perc, nseasons = 7, season.duration = 24)
  modelo <- bsts(data$occupation_perc,
                 state.specification = ss,
                 timestamps = data$timestamp,
                 na.action = na.exclude(),
                 niter = 500)
  
  burn <- SuggestBurn(0.1, modelo)
  horizon = 12
  pred <- predict.bsts(modelo, horizon = horizon, newdata = newData, burn = burn, quantiles = c(.025, .975))
  
  fitted = pred$mean
  actual = dataTest$occupation_perc
  
  print(paste("mes=", as.character(mesHasta)))
  
  de <- data.frame("SR", mesHasta, MAE(fitted, actual), MAPE(fitted, actual), MSE(fitted, actual), RMSE(fitted, actual))
  names(de) <- c("Modelo","Mes", "MAE", "MAPE","MSE","RMSE")
  dfMetrics <<- rbind(dfMetrics, de)
  

  return(0)
  
}
#----------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
# Bucle de entrenamiento sin regresores
#----------------------------------------------------------------------------------------
dfMetrics <- dfMetrics[0,]
for (mesHasta in mesesParaProcesar){
  proceso_sin_regresores("1234", mesHasta)
}
#----------------------------------------------------------------------------------------
names(dfMetrics) <- c("Modelo","Mes", "MAE", "MAPE","MSE","RMSE")
ggplot(data=dfMetrics, aes(x=Mes)) +
 geom_line(aes(y=RMSE, colour = "RMSE"), size=1.2)
#----------------------------------------------------------------------------------------









#----------------------------------------------------------------------------------------
# proceso_con_regresores
#----------------------------------------------------------------------------------------
proceso_con_regresores <- function(element_key, mesHasta) {
  
  ek <- element_key
  data     <- subset(data0, with(data0, as.numeric(data0$element_key) == ek & data0$Month <= mesHasta - 1))
  dataTest <- subset(data0, with(data0, as.numeric(data0$element_key) == ek & data0$Month == mesHasta))
  newdata  <- subset(data0, with(data0, as.numeric(data0$element_key) == ek & data0$Month == mesHasta))
  
  y = data$occupation_perc
  
  #ss <- AddLocalLinearTrend(list(), y) # Añado un estado trend y estados de 7 dias y 24 horas para que entienda que se trata de semanas y dias
  ss <- AddAr(list(), y)
  ss <- AddSeasonal(ss, y, nseasons = 24)
  ss <- AddSeasonal(ss, data$occupation_perc, nseasons = 7, season.duration = 24)
  modelo <- bsts(occupation_perc  ~ o3 + pm2_5 + co + no2 + road_temp + air_temp + tmin + tmax + prcp,
                 state.specification = ss,
                 timestamps = data$timestamp,
                 na.action = na.omit,
                 data = data,
                 niter = 500,
                 expected.model.size = 20)
  
  burn <- SuggestBurn(0.1, modelo)
  horizon = 12
  pred <- predict.bsts(modelo, horizon = horizon, newdata = newdata, burn = burn, quantiles = c(.025, .975))
  
  fitted = pred$mean
  actual = dataTest$occupation_perc
  
  print(paste("mes=", as.character(mesHasta)))
  
  de <- data.frame("CR", mesHasta, MAE(fitted, actual), MAPE(fitted, actual), MSE(fitted, actual), RMSE(fitted, actual))
  names(de) <- c("Modelo","Mes", "MAE", "MAPE","MSE","RMSE")
  dfMetrics <<- rbind(dfMetrics, de)
  
  
  return(0)
  
}
#----------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------
# Bucle de entrenamiento con regresores
#----------------------------------------------------------------------------------------
dfMetrics <- dfMetrics[0,]
for (mesHasta in mesesParaProcesar){
  proceso_con_regresores("1234", mesHasta)
}
#----------------------------------------------------------------------------------------
names(dfMetrics) <- c("Modelo","Mes", "MAE", "MAPE","MSE","RMSE")
ggplot(data=dfMetrics, aes(x=Mes)) +
 geom_line(aes(y=RMSE, colour = "RMSE"), size=1.2)
#----------------------------------------------------------------------------------------










#----------------------------------------------------------------------------------------
# OTRO ESCENARIO
#----------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------------
setwd("C:\\Users\\Admin\\Documents\\!!!!! UAH\\UAH-TFM-master_V2\\UAH-TFM-master\\data")
data0 <- read.table("SerieTotal2016_ext_top100.csv", sep=",", header=T)
#--------------------------------------------------------------------------------------------------
data0$Fecha     <- ymd(substr(data0$timestamp,1,10))
data0$timestamp <- ymd_hms(data0$timestamp)
data0$Hora      <- hour(data0$timestamp)
data0$Mes       <- month(data0$timestamp)
data0$Dia       <- day(data0$timestamp)
data0$WeekDay   <- weekdays(data0$timestamp)
#--------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
# proceso_con_regresores_YMD
#----------------------------------------------------------------------------------------
proceso_con_regresores_YMD <- function(element_key, mesHasta) {
  
  ek <- element_key
  data     <- subset(data0, with(data0, as.numeric(data0$element_key) == ek & data0$Mes <= mesHasta - 1))
  dataTest <- subset(data0, with(data0, as.numeric(data0$element_key) == ek & data0$Mes == mesHasta))
  newdata  <- subset(data0, with(data0, as.numeric(data0$element_key) == ek & data0$Mes == mesHasta))
  
  y = data$occupation_perc
  
  ss <- AddAr(list(), y)
  ss <- AddSeasonal(ss, y, nseasons = 52)
  ss <- AddSeasonal(ss, y, nseasons = 6, season.duration = 11)
  modelo <- bsts(occupation_perc  ~ Hora + o3 + pm2_5 + co + no2 + road_temp + air_temp + tmin + tmax + prcp,
                 state.specification = ss,
                 timestamps = data$Fecha,
                 na.action = na.omit,
                 data = data,
                 niter = 1000,
                 expected.model.size = 20)
  
  burn <- SuggestBurn(0.1, modelo)
  horizon = 12
  pred <- predict.bsts(modelo, horizon = horizon, newdata = newdata, burn = burn, quantiles = c(.025, .975))
  
  fitted = pred$mean
  actual = dataTest$occupation_perc
  
  print(paste("mes=", as.character(mesHasta)))
  
  de <- data.frame("CR YMD", mesHasta, MAE(fitted, actual), MAPE(fitted, actual), MSE(fitted, actual), RMSE(fitted, actual))
  names(de) <- c("Modelo","Mes", "MAE", "MAPE","MSE","RMSE")
  dfMetrics <<- rbind(dfMetrics, de)
  
  
  return(0)
  
}
#----------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------
# Bucle de entrenamiento con regresores
#----------------------------------------------------------------------------------------
dfMetrics <- dfMetrics[0,]
for (mesHasta in mesesParaProcesar){
  proceso_con_regresores_YMD("1234", mesHasta)
}
#----------------------------------------------------------------------------------------
names(dfMetrics) <- c("Modelo","Mes", "MAE", "MAPE","MSE","RMSE")
ggplot(data=dfMetrics, aes(x=Mes)) +
   geom_line(aes(y=RMSE, colour = "RMSE"), size=1.2)
#----------------------------------------------------------------------------------------





