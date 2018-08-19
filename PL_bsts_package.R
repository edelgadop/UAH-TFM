#-----------------------------------------------------------------
library(bsts)     # load the bsts package
#-----------------------------------------------------------------
setwd("~/!!!!! UAH/TFM-R")
#-----------------------------------------------------------------
# se carga el fichero
data0 <- read.table("Serie_Total2016.csv", sep=",", header=T)
#-----------------------------------------------------------------




#-----------------------------------------------------------------
# Seleccionando un parquimetro
#-----------------------------------------------------------------
data <- subset(
  data0, 
  with
  (
    data0, 
    as.numeric(data0$element_key) == 1001
  )
)


# se cambia el tipo de datos de la varable timestamp
str(data)
data$timestamp = as.Date(data$timestamp)




ss <- AddLocalLinearTrend(list(), data$occupation_perc)
ss <- AddSeasonal(ss, data$occupation_perc, nseasons = 52)
model1 <- bsts(data$occupation_perc,
               state.specification = ss,
               niter = 1000)


plot(model1)
plot(model1, "components")  # plot(model1, "comp") works too!
plot(model1, "help")





#-----------------------------------------------------------------
# Seleccionando varios parquimetros
#-----------------------------------------------------------------
data <- subset(
  data0, 
  with
  (
    data0, 
    as.numeric(data0$element_key) >= 1000
    & as.numeric(data0$element_key) <= 1005
  )
)

# se cambia el tipo de datos de la varable timestamp
str(data)
data$timestamp = as.Date(data$timestamp)

print(data$occupation_perc)


ss <- AddLocalLinearTrend(list(), data$occupation_perc)
#ss <- AddSeasonal(ss, data$occupation_perc, nseasons = 52)


#-----------------------------------------------------------------
model1 <- bsts(occupation_perc ~ .,
               state.specification = ss,
               niter = 500,
               data = data)

plot(model1, "coef")
#-----------------------------------------------------------------
model2 <- bsts(occupation_perc ~ .,
               state.specification = ss,
               niter = 1000,
               data = data,
               expected.model.size = 5)

plot(model2, "coef")
#-----------------------------------------------------------------
CompareBstsModels(list("Model 1" = model1,
                       "Model 2" = model2),
                  colors = c("red", "blue"))
#-----------------------------------------------------------------








