#------------------------------------------------------------------------------
library(bsts)
library(lubridate)
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
setwd("C:\\Users\\Admin\\Documents\\!!!!! UAH\\UAH-TFM-master_V2\\UAH-TFM-master\\data")
data0 <- read.table("SerieTotal2016_ext_top100.csv", sep=",", header=T)
data0$timestamp <- ymd_hms(data0$timestamp)
data0$Year      <- year(data0$timestamp)
data0$Month     <- month(data0$timestamp)
data0$Day       <- day(data0$timestamp)
data0$WeekDay   <- weekdays(data0$timestamp)
str(data0)
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# ek = 1234 & month = 1
#------------------------------------------------------------------------------
data <- subset(data0, with(data0, as.numeric(data0$element_key) == 1234 & data0$Month == 1))
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# sin regresores
#------------------------------------------------------------------------------
ss <- AddLocalLinearTrend(list(), data$occupation_perc)
ss <- AddSeasonal(ss, data$occupation_perc, nseasons = 7)
model1 <- bsts(data$occupation_perc,
               state.specification = ss,
               timestamps = data$timestamp,
               niter = 1000)

plot(model1)
plot(model1, "components")  

pred <- predict(model1, horizon = 12, burn = 100)
plot(pred)

#------------------------------------------------------------------------------
# con regresores
#------------------------------------------------------------------------------
ss <- AddLocalLinearTrend(list(), data$occupation_perc)
ss <- AddSeasonal(ss, data$occupation_perc, nseasons = 7)
model1 <- bsts(data$occupation_perc ~ prcp + tmax + tmin + air_temp + road_temp + poi + baseball + tennis + basket + soccer + event + no2 + co + pm2_5 + o3,
               state.specification = ss,
               timestamps = data$timestamp,
               na.action = na.omit,
               data = data0,
               niter = 1000)

plot(model1)
plot(model1, "components") 

pred <- predict(model1, horizon = 12, burn = 100)
plot(pred)






