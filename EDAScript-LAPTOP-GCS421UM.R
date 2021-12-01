source("DataCleaningScript.R")


library(ggplot2)
library(usmap)
library(maps)
library(sf)

#This is a year graph

t<-flushots%>%group_by(Year)%>%summarise(Benes=sum(Benes))

year_graph<-ggplot(t, aes(x=Year, y=Benes, Fill=Year))+geom_col()+geom_text(aes(label = scales::comma(Benes)), vjust = -0.5)+scale_y_continuous(labels = scales::comma) +ggtitle("Unique Beneficiaries Receiving Flu Vaccines per Year")


#this is a state map for flu shot volume

t2<-flushots%>%group_by(State)%>%summarise(Benes=sum(Benes))
names(t2)<-c("state", "X")

fmap <- plot_usmap(regions="states", data = t2, values = "X", color = "black") + scale_fill_continuous(low="white", high="red", name="Flu Shots Per State", label = scales::comma)+labs(title = "Unique Beneficiaries Receiving Flu Shots")
fmap

#this is provider gender
t3<-flushots%>%group_by(Gender)%>%summarise(Benes=sum(Benes))
g3<-flushots%>%group_by(Gender)%>%summarise(n())


providergender<-ggplot(t3, aes(x=Gender, y=Benes))+geom_col()+geom_text(aes(label = scales::comma(Benes)), vjust = -0.5)+scale_y_continuous(labels = scales::comma) +ggtitle("Providr Gender Characteristics")

#this is a state map for flu shot volume

t4<-flushots%>%group_by(State)%>%summarise(Payment=mean(ThirdPartypaymentamount))
names(t4)<-c("state", "X")

cmap <- plot_usmap(regions="states", data = t4, values = "X", color = "black") + scale_fill_continuous(low="white", high="green", name="Third Party Costs Per State", label = scales::comma)+labs(title = "Unique Beneficiaries Receiving Flu Shots")
cmap


#histogram for costs

hist(flushots$ThirdPartypaymentamount, breaks=seq(min(flushots$ThirdPartypaymentamount), max(flushots$ThirdPartypaymentamount), length.out=11))

#there's some extremes so might not make sense to use them

t5<-flushots%>%group_by(PrvdrType)%>%summarise(Benes=sum(Benes))

t6<-flushots%>%group_by(Name)%>%summarise(Benes=sum(Benes))

t7<-flushots%>%group_by(InOfficeFlag)%>%summarise(Benes=sum(Benes))


t8<-flushots%>%group_by(EntityFlag)%>%summarise(Benes=sum(Benes))

#Not going to use this
t9<-flushots%>%group_by(Crdntls)%>%summarise(Benes=sum(Benes))

#Not going to use this


hist(flushots$Benes)

#New stuff

f<-aggregate(flushots$Benes, by=list(State=flushots$State, Year=flushots$Year), FUN=sum)
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
f2<-left_join(f, region_lookup, by=c("State"))

thirteenpop<-read.csv("2013pop.csv")
fourteenpop<-read.csv("2014pop.csv")
fifteenpop<-read.csv("2015pop.csv")
sixteenpop<-read.csv("2016pop.csv")
seventeenpop<-read.csv("2017pop.csv")
eighteenpop<-read.csv("2018pop.csv")
nineteenpop<-read.csv("2019pop.csv")

nineteenpop$Year<-"2019"
eighteenpop$Year<-"2018"
seventeenpop$Year<-"2017"
sixteenpop$Year<-"2016"
fifteenpop$Year<-"2015"
fourteenpop$Year<-"2014"
thirteenpop$Year<-"2013"

statepop<-rbind(nineteenpop, eighteenpop, seventeenpop, sixteenpop, fifteenpop, fourteenpop, thirteenpop)


colnames(statepop)[1]<-c(substr(colnames(statepop[1]), 4, 11))

statepop$State<-state.abb[match(statepop$Location,state.name)]

#f2<-left_join(f2, statepop, by=c("Year", "State"))

#f2$rate<-f2$x/f2$Original.Medicare

ggplot(f, aes(x=as.numeric(Year), y=x))+geom_point(aes(color=factor(State)))+geom_smooth(method="lm", se=F)

l<-lm(x~as.numeric(Year),data=f)
summary(l)

#Lines

ggplot(f2%>%filter(Division=="Pacific"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East South Central"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West South Central"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Mountain"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="New England"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="South Atlantic"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West North Central"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East North Central"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Middle Atlantic"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

#linear fits

ggplot(f2%>%filter(Division=="Middle Atlantic"), aes(x=Year, y=x, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East North Central"), aes(x=Year, y=x, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West North Central"), aes(x=Year, y=x, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="South Atlantic"), aes(x=Year, y=x, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="New England"), aes(x=Year, y=x, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Mountain"), aes(x=Year, y=x, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West South Central"), aes(x=Year, y=x, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East South Central"), aes(x=Year, y=x, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Pacific"), aes(x=Year, y=x, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)




ACS<-rbind(ACSdf19, ACSdf18, ACSdf17, ACSdf16, ACSdf15, ACSdf14)

ACS$State<-state.abb[match(ACS$state,state.name)]
ACS$State<-ifelse(ACS$state=="District of Columbia"|ACS$state=="District Of ", "DC", ACS$State)


f<-aggregate(flushots$Benes, by=list(State=flushots$State, Year=flushots$Year), FUN=sum)
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
f2<-left_join(f, region_lookup, by=c("State"))
f2<-f2%>%filter(Year>=2014)
f2<-left_join(f2, ACS, by=c("State", "Year"))

f2$popestimate<-f2$x/f2$estimate




#Rates lines


ggplot(f2%>%filter(Division=="Pacific"), aes(x=Year, y=popestimate, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East South Central"), aes(x=Year, y=popestimate, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West South Central"), aes(x=Year, y=popestimate, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Mountain"), aes(x=Year, y=popestimate, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="New England"), aes(x=Year, y=popestimate, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="South Atlantic"), aes(x=Year, y=popestimate, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West North Central"), aes(x=Year, y=popestimate, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East North Central"), aes(x=Year, y=popestimate, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Middle Atlantic"), aes(x=Year, y=popestimate, color=State, group=State))+geom_line()+facet_wrap(~Division)



#rate linear fits


ggplot(f2%>%filter(Division=="Middle Atlantic"), aes(x=Year, y=popestimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East North Central"), aes(x=Year, y=popestimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West North Central"), aes(x=Year, y=popestimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="South Atlantic"), aes(x=Year, y=popestimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="New England"), aes(x=Year, y=popestimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Mountain"), aes(x=Year, y=popestimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West South Central"), aes(x=Year, y=popestimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East South Central"), aes(x=Year, y=popestimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Pacific"), aes(x=Year, y=popestimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(~Division)






f<-aggregate(flushots$Bene, by=list(State=flushots$State, Year=flushots$Year, Gender=flushots$Gender), FUN=sum)
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
f3<-left_join(f, region_lookup, by=c("State"))
f3<-f3%>%filter(Year>=2014)
f3<-left_join(f3, ACS, by=c("State", "Year"))



#rate linear fits
#gender is not really that interesting, there are more males in the United States for physicians

ggplot(f3%>%filter(Division=="Middle Atlantic"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(Gender~Division)

ggplot(f3%>%filter(Division=="East North Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(Gender~Division)

ggplot(f3%>%filter(Division=="West North Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(Gender~Division)

ggplot(f3%>%filter(Division=="South Atlantic"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(Gender~Division)

ggplot(f3%>%filter(Division=="New England"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(Gender~Division)

ggplot(f3%>%filter(Division=="Mountain"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(Gender~Division)

ggplot(f3%>%filter(Division=="West South Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(Gender~Division)

ggplot(f3%>%filter(Division=="East South Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(Gender~Division)

ggplot(f3%>%filter(Division=="Pacific"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(Gender~Division)




#gender is not really that interesting, there are more males in the United States for physicians
#this is interesting though
#should make a different plot to show the share visuals

f<-aggregate(flushots$Bene, by=list(State=flushots$State, Year=flushots$Year, EntityFlag=flushots$EntityFlag), FUN=sum)
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
f3<-left_join(f, region_lookup, by=c("State"))
f3<-f3%>%filter(Year>=2014)
f3<-left_join(f3, ACS, by=c("State", "Year"))


ggplot(f3%>%filter(Division=="Middle Atlantic"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(EntityFlag~Division)

ggplot(f3%>%filter(Division=="East North Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(EntityFlag~Division)

ggplot(f3%>%filter(Division=="West North Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(EntityFlag~Division)

ggplot(f3%>%filter(Division=="South Atlantic"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(EntityFlag~Division)

ggplot(f3%>%filter(Division=="New England"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(EntityFlag~Division)

ggplot(f3%>%filter(Division=="Mountain"&State!="ID"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(EntityFlag~Division)

ggplot(f3%>%filter(Division=="West South Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(EntityFlag~Division)

ggplot(f3%>%filter(Division=="East South Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(EntityFlag~Division)

ggplot(f3%>%filter(Division=="Pacific"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(EntityFlag~Division)


#In Office Flag

f<-aggregate(flushots$Bene, by=list(State=flushots$State, Year=flushots$Year, InOff=flushots$InOfficeFlag), FUN=sum)
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
f3<-left_join(f, region_lookup, by=c("State"))
f3<-f3%>%filter(Year>=2015)
f3<-left_join(f3, ACS, by=c("State", "Year"))


ggplot(f3%>%filter(Division=="Middle Atlantic"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(InOff~Division)

ggplot(f3%>%filter(Division=="East North Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(InOff~Division)

ggplot(f3%>%filter(Division=="West North Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(InOff~Division)

ggplot(f3%>%filter(Division=="South Atlantic"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(InOff~Division)

ggplot(f3%>%filter(Division=="New England"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(InOff~Division)

ggplot(f3%>%filter(Division=="Mountain"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(InOff~Division)

ggplot(f3%>%filter(Division=="West South Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(InOff~Division)

ggplot(f3%>%filter(Division=="East South Central"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(InOff~Division)

ggplot(f3%>%filter(Division=="Pacific"), aes(x=Year, y=x/estimate, color=State, group=State))+geom_point()+geom_smooth(method="lm",se=F)+facet_wrap(InOff~Division)


f<-aggregate(flushots$Bene, by=list(State=flushots$State, Year=flushots$Year, EntityFlag=flushots$EntityFlag), FUN=sum)
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
f4<-left_join(f, region_lookup, by=c("State"))
f4<-f4%>%filter(Year>=2014)
f4<-left_join(f4, ACS, by=c("State", "Year"))


ggplot(f4, aes(x=Year, y=x/estimate, color=EntityFlag, group=EntityFlag))+geom_point()+geom_smooth(method="lm", se=F)+facet_wrap(f4$Division)

f<-flushots%>%filter(EntityFlag=="O")

f2<-aggregate(f$Bene, by=list(Name=f$Name), FUN=sum)
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
f5<-left_join(f2, region_lookup, by=c("State"))
f5<-f5%>%filter(Year>=2014)
#f5<-left_join(f5, ACS, by=c("State", "Year"))


ggplot(f5, aes(x=Name, y=x/estimate))+geom_col()



