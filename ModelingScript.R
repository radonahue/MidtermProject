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