library(PxWebApiData)
library(ggplot2)
install.packages("gglorenz")
library(gglorenz)
#Hvilke variabler som finnes i tabellen
variables <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                     returnMetaFrames = TRUE)
names(variables)

#hvilke verdier har ulike variablene
values <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                  returnMetaData = TRUE)
#Kommunekoder
values[[1]]$values
#Inntekt før/etter skatt
values[[2]]$values # 00 = Samlet inntekt, 00S=Inntekt etter skatt
#Desiler
values[[3]]$values
#Statistikkvariabel
values[[4]]$values
#År
values[[5]]$values
data <- ApiData("https://data.ssb.no/api/v0/en/table/12558/",
                Tid =c("2005","2020"), # Velg årene 2005 og 2020
                Desiler=c("01", "02", "03" ,"04", "05", "06" ,"07", "08" ,"09", "10"), #Vi velger alle desiler
                InntektSkatt="00", #Vi velger samlet inntekt
                ContentsCode="VerdiDesil", #Velger den høyeste verdien i desilen
                Region=c("5401","1902")) #Tromsø endret kommunenummer i 2020

dataframe <- data$dataset
dataframe <- dataframe[complete.cases(dataframe[,c("value")]),]

ggplot(dataframe, aes(x = value, color = Tid))+
  stat_lorenz()+
  geom_abline(color = "grey")+
  ggtitle("Inntektsfordeling i Tromsø kommune 2005-2020")
  

