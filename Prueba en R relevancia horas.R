library(dplyr)
library(tidyr)

#Me importo el df que hemos sacado tras la ejecución del notebook Consolidación notebooks EC_Seattle_data_sources
df=read.csv2("C:/Users/CINTIA/Desktop/ProyectoFindeMaster/df_r.csv", sep = ",")


#revisamos el df
str(df)
head(df)
summary(df)

#Vamos a generar nuevas columnas a partir del timestamp
#Yo me suelo guiar para los formatos por esta página https://www.r-bloggers.com/date-formats-in-r/
#Esta página para sacar las horas https://www.stat.berkeley.edu/~s133/dates.html
df_1 <- df %>%
  mutate(
    week = format(as.POSIXlt(timestamp), '%a'),
    mes =  format(as.POSIXlt(timestamp), '%b'),
    hour = format(as.POSIXlt(timestamp),'%H')
  )

str(df_1)
head(df_1)
summary(df_1)

df_1$hour=as.numeric(df_1$hour)
df_1$week=as.factor(df_1$week)
df_1$mes=as.factor(df_1$mes)

#Voy a generar dos nuevos campos por franja horaria:  https://www.seattle.gov/transportation/projects-and-programs/programs/parking-program/paid-parking-information/street-parking-rates
#También he creado un campo time_zone_2 con un reparto equitativo de las horas (4h cada franja)
#Este campo nos puede resultar útil para reducir el número de registros en los modelos y que no pesen tanto

df_1$time_zone <- as.factor(ifelse(df_1$hour < 11, 'morning', 
                                ifelse(df_1$hour > 17, 'Evening', 'Afternoon')))

df_1$time_zone_2 <- as.factor(ifelse(df_1$hour < 12, 'morning', 
                                   ifelse(df_1$hour > 16, 'Evening', 'Afternoon')))

#Analisis poder predictivo

relevancia=function(Target,VariableCategorica){
  levels=levels(VariableCategorica)
  colors=c()
  for (i in 1:length(levels)){
    TABLA=table(Target,VariableCategorica==levels[i])
    chi=chisq.test(TABLA)
    if (chi$p.value<0.05){
      colors=c(colors,"green")
    }else{
      colors=c(colors,"gray")
    }
  }
  TABLA=table(Target,VariableCategorica)
  plot=barplot(100*TABLA[2,]/(TABLA[1,]+TABLA[2,]),ylim=c(0,100),col=colors,cex.names=0.6)
  text(x=plot, y=5+100*TABLA[2,]/(TABLA[1,]+TABLA[2,]),labels=paste(round(100*TABLA[2,]/(TABLA[1,]+TABLA[2,]),2),"%",sep=""))
}

df_2 <- filter(df_1, element_key == '1001')
df_2$occupation_perc=as.numeric(df_2$occupation_perc)
# Campo ocu donde 1 es alta ocupacion y 0 baja ocupacion
df_2$ocu <- as.factor(ifelse(df_2$occupation_perc < 50 , 1, 0))
df_2$hour=as.factor(df_2$hour)

table(df_2$ocu)
table(df_2$time_zone)
table(df_2$time_zone_2)

relevancia(df_2$ocu, df_2$time_zone)

relevancia(df_2$ocu, df_2$time_zone_2)

relevancia(df_2$ocu, df_2$hour)

#Necesitamos encontrar un termino medio entre los campos time_zone y hour





                      