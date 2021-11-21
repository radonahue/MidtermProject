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


