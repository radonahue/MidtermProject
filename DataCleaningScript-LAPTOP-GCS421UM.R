library(tidyverse)
library(openxlsx)
library(dplyr)
library(stringr)

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

nineteen$Year<-"2019"
eighteen$Year<-"2018"
seventeen$Year<-"2017"
sixteen$Year<-"2016"
fifteen$Year<-"2015"
fourteen$Year<-"2014"
thirteen$Year<-"2013"


nineteen$yearnum<-7
eighteen$yearnum<-6
seventeen$yearnum<-5
sixteen$yearnum<-4
fifteen$yearnum<-3
fourteen$yearnum<-2
thirteen$yearnum<-1

data<-rbind(nineteen, eighteen, seventeen, sixteen, fifteen, fourteen, thirteen)

data<-data%>%filter(HCPCS_Cd=="G0008")

dataframe<-data[, c(1:2, 7, 11, 17, 23,26, 29:30)]
names(dataframe)<- c("NPI", "Name",  "EntityFlag", "State", "PrcType", "Benes", "TotalCharges", "MedicarePaymentAmount", "Year")

flushots<-dataframe %>% filter(State!="AA"&State!="AE"&State!="XX"&State!="AP"&State!="MP"&State!="VI"&State!="GU"&State!="PR")


ACS19<-read.xlsx("ACS2019.xlsx")
ACSdf19<-ACS19[9:59, 1:2]
names(ACSdf19)<-c("state", "estimate")
ACSdf19$Year<-"2019"
ACSdf19$yearnum<-7
ACSdf19$estimate<-as.numeric(ACSdf19$estimate)
ACSdf19$estimate<-ACSdf19$estimate*1000

ACS18<-read.xlsx("ACS2018.xlsx")
ACSdf18<-ACS18[7:59, 1:2]
names(ACSdf18)<-c("state", "estimate")
ACSdf18$Year<-"2018"
ACSdf18$yearnum<-6
ACSdf18$state<-str_to_title(tolower(ACSdf18$state))
ACSdf18$estimate<-as.numeric(ACSdf18$estimate)
ACSdf18$estimate<-ACSdf18$estimate*1000

ACS17<-read.xlsx("ACS2017.xlsx")
ACSdf17<-ACS17[7:59, 1:2]
names(ACSdf17)<-c("state", "estimate")
ACSdf17$Year<-"2017"
ACSdf17$yearnum<-5
ACSdf17$state<-str_to_title(tolower(ACSdf17$state))
ACSdf17$estimate<-as.numeric(ACSdf17$estimate)
ACSdf17$estimate<-ACSdf17$estimate*1000

ACS16<-read.xlsx("ACS2016.xlsx")
ACSdf16<-ACS16[8:59, 1:2]
names(ACSdf16)<-c("state", "estimate")
ACSdf16$Year<-"2016"
ACSdf16$yearnum<-4
ACSdf16$state<-str_to_title(tolower(ACSdf16$state))
ACSdf16$estimate<-as.numeric(ACSdf16$estimate)
ACSdf16$estimate<-ACSdf16$estimate*1000

ACS15<-read.xlsx("ACS2015.xlsx")
ACSdf15<-ACS15[8:59, 1:2]
names(ACSdf15)<-c("state", "estimate")
ACSdf15$Year<-"2015"
ACSdf15$yearnum<-3
ACSdf15$state<-str_to_title(tolower(ACSdf15$state))
ACSdf15$estimate<-as.numeric(ACSdf15$estimate)
ACSdf15$estimate<-ACSdf15$estimate*1000

ACS14<-read.xlsx("ACS2014.xlsx")
ACSdf14<-ACS14[8:59, 1:2]
names(ACSdf14)<-c("state", "estimate")
ACSdf14$Year<-"2014"
ACSdf14$yearnum<-2
ACSdf14$state<-str_to_title(tolower(ACSdf14$state))
ACSdf14$estimate<-as.numeric(ACSdf14$estimate)
ACSdf14$estimate<-ACSdf14$estimate*1000

#Thirteen's different

ACS13<-read.xlsx("ACS2013.xlsx")
ACSdf13<-ACS13[51:54,]
z_<-as.data.frame(t(ACSdf13[2:4, 2:206]))
names(z_)<-c("state", "Group","estimate")
z_<-subset(z_, z_$Group=="All persons")
z_<-z_[2:nrow(z_),c(1,3)]
ACSdf13<-z_
ACSdf13$Year<-"2013"
ACSdf13$yearnum<-1
ACSdf13$state<-str_to_title(tolower(ACSdf13$state))
ACSdf13$estimate<-as.numeric(ACSdf13$estimate)
ACSdf13$estimate<-ACSdf13$estimate*1000


ACS<-rbind(ACSdf19, ACSdf18, ACSdf17, ACSdf16, ACSdf15, ACSdf14, ACSdf13)

ACS$State<-state.abb[match(ACS$state,state.name)]

ACS$State<-ifelse(ACS$state=="District Of Columbia"|ACS$state=="District of Columbia", "DC", ACS$State)

ACS<-ACS%>%filter(estimate!="NA")

ACS<-ACS%>%dplyr::select(State,Year,estimate)

flushot_s<-aggregate(Benes~State+Year+EntityFlag, data=flushots, sum)


flushot_s<-left_join(flushot_s, ACS, by=c("State", "Year", "yearnum"))





