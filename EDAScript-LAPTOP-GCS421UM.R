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

f<-aggregate(flushots$Benes, by=list(State=flushots$State, Year=flushots$Year), FUN=sum)
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
f2<-left_join(f, region_lookup, by=c("State"))
f2<-as.data.frame(f2)

ggplot(f, aes(x=as.numeric(Year), y=x))+geom_point(aes(color=factor(State)))+geom_smooth(method="lm", se=F)

l<-lm(x~as.numeric(Year),data=f)
summary(l)

ggplot(f2, aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Region)

ggplot(f2%>%filter(Division=="Pacific"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East South Central"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West South Central"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Mountain"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="New England"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="South Atlantic"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="West North Central"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="East North Central"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

ggplot(f2%>%filter(Division=="Middle Atlantic"), aes(x=Year, y=x, color=State, group=State))+geom_line()+facet_wrap(~Division)

