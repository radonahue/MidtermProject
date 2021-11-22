library(tidyverse)
library(openxlsx)
library(dplyr)

unzip("2013-zip.zip")
thirteen<-read.csv("2013.csv")
unzip("2014-zip.zip")
fourteen<-read.csv("2014.csv")
unzip("2015-zip.zip")
fifteen<-read.csv("2015.csv")
unzip("2016-zip.zip")
sixteen<-read.csv("2016.csv")
unzip("2017-zip.zip")
seventeen<-read.csv("2017.csv")
unzip("2018-zip.zip")
eighteen<-read.csv("2018.csv")
unzip("2019-zip.zip")
nineteen<-read.csv("2019.csv")

nineteen$year<-"2019"
eighteen$year<-"2018"
seventeen$year<-"2017"
sixteen$year<-"2016"
fifteen$year<-"2015"
fourteen$year<-"2014"
thirteen$year<-"2013"

data<-rbind(nineteen, eighteen, seventeen, sixteen, fifteen, fourteen, thirteen)


dataframe<-data[, c(1:2, 5:6, 7, 11, 17, 22:23, 26, 29:30)]
names(dataframe)<- c("NPI", "Name", "Crdntls", "Gender", "EntityFlag", "State", "PrvdrType", "InOfficeFlag", "Benes", "TotalCharges", "MedicarePaymentAmount", "Year")

dataframe$ThirdPartypaymentamount<-dataframe$TotalCharges-dataframe$MedicarePaymentAmount


#flushots<-dataframe %>% filter(State==state.abb|State=="DC")
flushots<-dataframe %>% filter(State!="AA"&State!="AE"&State!="XX"&State!="AP"&State!="MP"&State!="VI"&State!="GU"&State!="PR")


#og<-dataframe %>% group_by(State) %>% summarise(n())



