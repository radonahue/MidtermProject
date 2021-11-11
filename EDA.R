library(tidyverse)
library(openxlsx)
library(dplyr)
library(usmap)
library(lme4)

#data dictionary for reference: https://data.cms.gov/resources/medicare-physician-other-practitioners-by-provider-and-service-data-dictionary

#for population stats: https://www.kff.org/medicare/state-indicator/total-medicare-beneficiaries/?currentTimeframe=7&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D


nineteen<-read.csv("2019.csv")
eighteen<-read.csv("2018.csv")
seventeen<-read.csv("2017.csv")
sixteen<-read.csv("2016.csv")
fifteen<-read.csv("2015.csv")
fourteen<-read.csv("2014.csv")
thirteen<-read.csv("2013.csv")

nineteen$year<-"2019"
eighteen$year<-"2018"
seventeen$year<-"2017"
sixteen$year<-"2016"
fifteen$year<-"2015"
fourteen$year<-"2014"
thirteen$year<-"2013"

data<-rbind(nineteen, eighteen, seventeen, sixteen, fifteen, fourteen, thirteen)

data$state<-data$Rndrng_Prvdr_State_Abrvtn

#states

thirteenpop<-read.csv("2013pop.csv")
fourteenpop<-read.csv("2014pop.csv")
fifteenpop<-read.csv("2015pop.csv")
sixteenpop<-read.csv("2016pop.csv")
seventeenpop<-read.csv("2017pop.csv")
eighteenpop<-read.csv("2018pop.csv")
nineteenpop<-read.csv("2019pop.csv")

nineteenpop$year<-"2019"
eighteenpop$year<-"2018"
seventeenpop$year<-"2017"
sixteenpop$year<-"2016"
fifteenpop$year<-"2015"
fourteenpop$year<-"2014"
thirteenpop$year<-"2013"

statepop<-rbind(nineteenpop, eighteenpop, seventeenpop, sixteenpop, fifteenpop, fourteenpop, thirteenpop)

rename(statepop$ï..Location, Location)

statepop$state<-state.abb[match(statepop$ï..Location,state.name)]

datamain<-left_join(data, statepop, by=c("year", "state"))

#column cleanup needed but Medicare populations are here

#There are some "states" that we might want to consider excluding, AA, XX, AP, MP, AE, VI, GU, PR as they rep military sites and territories

d<-table(data$HCPCS_Cd, data$HCPCS_Desc)
View(d)
library(openxlsx)

write.xlsx(d, "codesdesc.xlsx")

#t<-subset(data, data$HCPCS_Desc=="Vaccine for influenza administered into muscle to individuals 3 years of age and older")

#Total Beneficiaries Receiving flu shot services per year

t<-aggregate(data$Tot_Benes, by=list(Year=data$year), FUN=sum)

ggplot(t, aes(x=Year, y=x, fill=Year))+geom_col()+geom_text(aes(label = x), vjust = -0.5)+scale_y_continuous(labels = scales::comma) +ggtitle("Unique Beneficiaries Receiving Flu Vaccines per Year")

#Unique Number of Providers per year

z<-data %>% unique %>% group_by(year) %>% summarise(Rndrng_NPI=n())

ggplot(z, aes(x=year, y=Rndrng_NPI, fill=year))+geom_col()+geom_text(aes(label = Rndrng_NPI), vjust = -0.5)+scale_y_continuous(labels = scales::comma) +ggtitle("Unique Providers per year")

#Total Beneficiaries Receiving flu shot services per state

t2<-aggregate(data$Tot_Benes, by=list(state=data$Rndrng_Prvdr_State_Abrvtn), FUN=sum)

#Couldn't get a map to work

# FluShots <- plot_usmap(data = t2, values = t2$x, color = "black") + 
#   scale_fill_continuous(low="white", high = "firebrick", name = "Amount of Flu Shots", label = scales::comma) + 
#   theme(legend.position = "right") 

ggplot(t2, aes(x=reorder(state, x), y=x, fill=state))+geom_col()+geom_text(aes(label = x), vjust = -0.5)+scale_y_continuous(labels = scales::comma) +ggtitle("States with the most flu shots per year")+coord_flip()
#this is impossible to read, make better

t3<-aggregate(data$Tot_Benes, by=list(state=data$Rndrng_Prvdr_State_Abrvtn, year=data$year), FUN=sum)

plot<-function(a){{
  st<-a
  data2<-filter(t3, state==a)
  c<-ggplot(data2, aes(x=year, y=x, fill=year))+geom_col()+geom_text(aes(label = x), vjust = -0.5)+scale_y_continuous(labels = scales::comma) +ggtitle(st)
print(c)
}
  }

states<-(unique(t3$state))
for (st in states) {
  plot(st)
}


t3<-aggregate(datamain$Tot_Benes, by=list(state=data$Rndrng_Prvdr_State_Abrvtn, year=data$year), FUN=sum)
#figure out how to join this with state counts to turn things into a rate, since I don't need to sum I just need to refer to those values

t4<-dplyr::distinct(datamain$abb, datamain$year, datamain$Original.Medicare)

plot<-function(a){{
  st<-a
  data2<-filter(t3, state==a)
  c<-ggplot(data2, aes(x=year, y=x, fill=year))+geom_col()+geom_text(aes(label = x), vjust = -0.5)+scale_y_continuous(labels = scales::comma) +ggtitle(st)
  print(c)
}
}

states<-(unique(t3$state))
for (st in states) {
  plot(st)
}

#WY really went up overtime, WV really went down, VA went up, UT went up, RI really shrank, OR dipped and came back, MN really went up, MI really went down, ME really went down, KY went down, IL really went down, ID substancially up, IA increased, FL went down and then came back up again, CT really went down, CO really went up, AZ has a large spike, AL really went down, AK had a huge jump

#Could turn those graphs into simple linear regression plots, grab the ones that have a negative slope

plot("MI")

#MI, KY, IL all had increases in their populations but decreases in flu shots

#Poisson to use populations as a weight for flu shot rates+offset,
#could just focus on states

#finding if doctors have records of flu shot rates to indicate if they are a good provider or not

#simple linear regressions per state
ggplot(t3, aes(x=year, y=x, color=state, group=state))+geom_smooth(se=F, method="lm", linetype=1)

#ProviderType

t4<-aggregate(data$Tot_Benes, by=list(providertype=data$Rndrng_Prvdr_Type, year=data$year), FUN=sum)

#LocationType (Can't do descriptions because too long)

t5<-aggregate(data$Tot_Benes, by=list(localtype=data$Rndrng_Prvdr_RUCA, year=data$year), FUN=sum)

#simple linear regressions per locationtype
ggplot(t5, aes(x=year, y=x, color=as.factor(localtype), group=as.factor(localtype)))+geom_smooth(se=F, method="lm", linetype=1)
#metropolitan areas have the biggest share with increases

plot("AZ")

#cities


doesthisbreak<-lmer(Tot_Benes~(1|state), data=data2)

summary(doesthisbreak)

