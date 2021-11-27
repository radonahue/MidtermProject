library(lme4)
library(rstanarm)
library(lmerTest)

source("DataCleaningScript.R")

test<-flushots[sample(nrow(flushots), 200000),]

model1<-lm(Benes~Gender, data=test)

model2<-lmer(Benes~EntityFlag + (1|State), data=test)

model3<-lmer(Benes~EntityFlag + (1|State)+ (1|Year), data=test)
summary(model3)

test2<-flushots[sample(nrow(flushots), 1000),]

model4<-lmer(Benes~ThirdPartypaymentamount + (1|State), data=test2)

summary(model4)

coef(model4)

model5<-lm(Benes~Year, data=test)

model5<-lmer(Benes~(1|State), data=test2)

#doesn't make any sense
#model6<-lmer(Benes~(1|State)+(1|ThirdPartypaymentamount), data=test2)

m7<-lmer(Benes~Year+(1|State), data=test2)
summary(m7)
coef(m7)



#m8<-lmer(Benes~Year+(State|Year), data=test2)
summary(m8)
coef(m8)



m9<-glmer(Benes~(1|Year), data=test2, family=poisson(link="log"))
summary(m9)
coef(m9)

#pp_check(m9)

#m10<-stan_glmer(Benes~(1|Year), data=test2, family=poisson(link="log"))

m11<-lmer(Benes~Year+(1|State), data=test2)

m12<-lmer(Benes~State, data=test2)


#no pooling model on states

l<-lm(Benes~factor(State)-1, data=test2)
#stop and frisk gelman
  #add offset terms 

#year and state nesting with provider with poisson, try to do population offset

#year as a predictor random slope within nesting

#look at the model fit, see how it fits the data, look at the residuals, see issues and comment on it +that's probably enough


#just think about what is the comparison you're trying to make? what is the interest you have, how does this help fill in some of the gaps? 
#ask the question gather the information required to answer the question

#flu seasons that were bad, does that lead to a spike in the following year

#are individual providers serving more people or organizations?
  #are they more popular in different locations?
#does this population prefer male or female providers?
  #does this vary in different states or is this nationwide?
     #has this changed over time?
      #if there has been declines, is that a state that's currently struggling with covid vaccines? is it an access problem?
#if we end up living in a world with frequent boosters, understanding where/how people do flu shots, where the opportunities are is essential. Medicare population is a vulnerable one, and one of the largest drivers of the us healthcare system to come

#things I can't answer right now: 
  #days of the week people get vaccinated on
  #weather influencing vaccines
  #do vaccine campaigns work?
  #areas where there are high or low provider availability?
  #does electoral politics influence states rates?

l2<-lmer(Benes~EntityFlag+(1+EntityFlag|Year), data=test2)

l3<-lmer(Benes~EntityFlag+(1+EntityFlag|State), data=test)

test3<-flushots[sample(nrow(flushots), 1000000),]

l3<-lmer(Benes~EntityFlag+(1+EntityFlag|State), data=test3)

l4<-lmer(x~as.numeric(Year)+(1|State), data=f2)
summary(l4)
coef(l4)


l5<-lmer(x~as.numeric(Year)+(1+as.numeric(Year)|State), data=f2)
summary(l5)
coef(l5)

l5.2<-lmer(rate~as.numeric(Year)+(1+as.numeric(Year)|State), data=f2)

fixef(l5)
ranef(l5)

unlist(coef(l5))[1:3]


coef(l5)

r<-residuals(l5)

p<-predict(l5, type="response")

plot(p, r)
resid(l5)


plot(fitted(l5), resid(l5))
fitted(l5)

plot(l5)

qqnorm(residuals(l5))
qqnorm(residuals(l5.2))

#f2$Yearnum<-as.numeric(f2$Year)
l6<-glmer(x~Year+(1+Year|State), data=f2, family=poisson(link="log"))


stan_glmer(x~Year+(1+Year|State), data=f2, family=poisson(link="log"))

stan_glm(x~Year+(1+Year|State), data=f2, family=poisson(link="log"))

#plot(fitted(l6), resid(l6))


#fitting on subsets
fit<-lmer(x/estimate~+(Year|State),data=f3)

summary(fit)
coef(fit)


resid(fit)
fitted(fit)

hist(resid(fit))




#testing stuff out
fit<-lmer(x/estimate~+(1|State),data=f3)


g<-aggregate(flushots$MedicarePaymentAmount, by=list(State=flushots$State, Year=flushots$Year), FUN=mean)
fg<-left_join(f3, g, by=c("State", "Year"))
#varyingintercept
fit<-lmer(x.x/estimate~x.y+(1|State), data=fg)
summary(fit)
coef(fit)
#I think you have to figure out what each individual states would be by subtracting their intercept in the coef call from the intercept in the summary call
#varyingslope

fit<-lmer(x.x/estimate~x.y+(1+1|State), data=fg)
summary(fit)
coef(fit)


a<-aggregate(flushots$Benes, by=list(Year=flushots$Year), FUN=sum)
z<-aggregate(ACS$estimate, by=list(Year=ACS$Year), FUN=sum)
left_join(a,z, by="Year")


fit2<-lmer(Benes~(1|State), data=flushots)







#That's all old

library(rstanarm)
library(arm)
library(lme4)


fit1<-glm(Benes~State, data=flushots, family=poisson(link="log"))
display(fit1)

#this is the log number of benes per provider in each state

#how to figure out overdispersion

fit2<-glm(Benes~EntityFlag, data=flushots, family = poisson(link="log"))

display(fit2)

#individual data per entity status, clearly entity matters somewhat

fit3<-glm(Benes~EntityFlag, data=flushots)
display(fit3)

#wanted to see it w/o poisson

fit4<-glm(Benes~EntityFlag+Year, data=flushots)
display(fit4)


#yay statistical significance
fit5<-lmer(Benes~EntityFlag+(1+EntityFlag|Year), data=flushots)
summary(fit5)
coef(fit5)

#Shows the interaction of entity flag and year



#practicing the offsets

fit5<-glm(Benes~EntityFlag+Year+offset(log(estimate)),data=flushots, family=poisson(link="log"))
display(fit5)


a<-aggregate(flushots$Benes, by=list(Year=flushots$Year, State=flushots$State), FUN=sum)
c<-left_join(a,ACS,by=c("Year", "State"))
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
c<-left_join(c, region_lookup, by=c("State"))

ggplot(data=c%>%filter(Division=="Middle Atlantic"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(~Division)


ggplot(data=c%>%filter(Division=="East North Central"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(~Division)


ggplot(data=c%>%filter(Division=="West North Central"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(~Division)


ggplot(data=c%>%filter(Division=="South Atlantic"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(~Division)


ggplot(data=c%>%filter(Division=="New England"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(~Division)

ggplot(data=c%>%filter(Division=="West South Central"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(~Division)

ggplot(data=c%>%filter(Division=="East South Central"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(~Division)

ggplot(data=c%>%filter(Division=="Pacific"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(~Division)


#Entity flags

a<-aggregate(flushots$Benes, by=list(Year=flushots$Year, State=flushots$State, EntityFlag=flushots$EntityFlag), FUN=sum)
c<-left_join(a,ACS,by=c("Year", "State"))
region_lookup <- data.frame(state.abb,state.division)
names(region_lookup)<-c("State", "Division")
c<-left_join(c, region_lookup, by=c("State"))

ggplot(data=c%>%filter(Division=="Middle Atlantic"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(EntityFlag~Division)


ggplot(data=c%>%filter(Division=="East North Central"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(EntityFlag~Division)


ggplot(data=c%>%filter(Division=="West North Central"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(EntityFlag~Division)


ggplot(data=c%>%filter(Division=="South Atlantic"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(EntityFlag~Division)


ggplot(data=c%>%filter(Division=="New England"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(EntityFlag~Division)

ggplot(data=c%>%filter(Division=="West South Central"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(EntitiyFlag~Division)

ggplot(data=c%>%filter(Division=="East South Central"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(EntityFlag~Division)

ggplot(data=c%>%filter(Division=="Pacific"), aes(x=as.numeric(Year), y=x/estimate, group=State, color=State))+geom_point()+geom_smooth(method="lm")+facet_wrap(EntityFlag~Division)



#glmer

fit5<-glmer(Benes~1+(1|EntityFlag), offset=estimate, data=flushots, family=poisson(link="log"))


#trying to actually fit for real

fit6<-glmer(Benes~1+(1|State)+(1|Year), offset=estimate, data=flushot_s, family=poisson(link="log"))

#additional log transformations don't work

fit6<-lmer(Benes~EntityFlag+(1|State)+(1|Year), offset=estimate, data=flushot_s)

#this fits, don't know what it's doing with the offset term

fit7<-glmer(log(Benes)~EntityFlag+(1|State)+(1|Year), data=flushot_s)
#this gives the singluar fit warnings

fit8<-stan_glmer(log(Benes)~EntityFlag+(1|State)+(1|Year), data=flushot_s)


#thinking about it some more, year doesn't seem fair to be treated as a string since I'm interested in modeling the increases/decreases as the year increases

fit9<-glmer(Benes~as.numeric(Year)+(1|State), offset=estimate, data=flushot_s, family=poisson(link="log"))
#this still breaks


fit10<-glm(Benes~Year+EntityFlag, offset=estimate, data=flushot_s, family=poisson(link="log"))

#this really doesn't like being poissoned


fit10<-glm(Benes~Year+EntityFlag, offset=estimate, data=flushot_s)



#omg this actually works! and it fits with no warnings :)

fit11<-glmer(Benes~1+(1|State)+(1|Year), offset=log(estimate), data=flushot_s, family=poisson(link="log"))

#warnings, would it be bad to treat year as continuous?

fit12<-glmer(Benes~as.numeric(Year)+(1|State), offset=log(estimate), data=flushot_s, family=poisson(link="log"))

#model options


fit13<-glmer(Benes~EntityFlag+(1|State)+(1|Year), offset=log(estimate), data=flushot_s, family=poisson(link="log"))
#this gives a warning

fit14<-glmer(Benes~EntityFlag+Year+(1|State)+(1|Year), offset=log(estimate), data=flushot_s, family=poisson(link="log"))
#warning
#this doesn't interest me

fit15<-glmer(Benes~EntityFlag+(1|State)+(1|Year), offset=log(estimate), data=flushot_s, family=poisson(link="log"))


#different options, think about how to validate and interpret




