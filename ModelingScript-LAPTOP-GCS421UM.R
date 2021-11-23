library(lme4)
library(rstanarm)

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
