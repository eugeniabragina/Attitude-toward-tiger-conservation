# attitude toward the tiger conservation 
library(MuMIn)
setwd('')
var1=read.csv('SurveyData.csv') 
var1=var1[var1$place == "village", ] # villager only
var1$rod_zanatiy[var1$rod_zanatiy !=3] <- "NOh" # hunter versus non-hunters
var1$rod_zanatiy[var1$rod_zanatiy ==3] <- "Oh"
var1$sex[var1$sex ==1] <- "M" # gender as M and F
var1$sex[var1$sex ==2] <- "F"
var1$priroda[var1$priroda !=2] <- "N" ## perecodirovala lubiteli byvat' na prirode i ne lubiteli
var1$priroda[var1$priroda ==2] <- "L"
#var1$atiit[var1$atiit ==1] <- "Y" # Y or N - either respondents support tiger conservation or not
#var1$atiit[var1$atiit ==0] <- "N"
data3 = var1[var1$stebilnost < 3, ] # excluding respondents who said 'other' to a question about stable income
data3 = na.omit(var1) # removing all respondents with NA
data4 = data3[c(2:19)] 
global <-glm(atiit ~ sex + age + med + dosug + transport + school + work + molod + priroda
          + srok + dohod + stebilnost + rod_zanatiy + as.factor(videl),
             family = "binomial", data=data4, na.action=na.pass)
summary(global)
logLik(global)
# looking for a best model 
attitude = dredge(global)
get.models(attitude, subset = 1:15)

#################
m1 = glm(atiit ~ sex + molod,  family = "binomial", data=data4, na.action=na.pass) #AIC 210
m2 = glm(atiit ~ sex + molod + age,  family = "binomial", data=data4, na.action=na.pass) #AIC 208.19
m3 = glm(atiit ~ age + molod + sex + transport,  family = "binomial", data=data4, na.action=na.pass) 
m4 = glm(atiit ~ age + molod + sex + med, family = "binomial",  data = data4, na.action = na.pass)
m5 = glm(atiit ~ age + molod + sex + stebilnost, family = "binomial", data = data4, na.action = na.pass)
m6 = glm(atiit ~ age + dohod + molod + sex, family = "binomial", data = data4, na.action = na.pass)
m7 = glm(atiit ~ age + molod + sex + srok, family = "binomial", data = data4, na.action = na.pass)
m8 = glm(atiit ~ age + molod + rod_zanatiy + sex, family = "binomial", data = data4, na.action = na.pass)
m9 = glm(atiit ~ age + dosug + molod + sex, family = "binomial", data = data4, na.action = na.pass)
m10 = glm(atiit ~ age + molod + sex + work, family = "binomial", data = data4, na.action = na.pass)
m11 = glm(atiit ~ age + molod + priroda + sex, family = "binomial", data = data4, na.action = na.pass)
m12 = glm(atiit ~ age + molod + school + sex, family = "binomial", data = data4, na.action = na.pass)
m13 = glm(atiit ~ age + dohod + molod + sex + transport, family = "binomial", data = data4, na.action = na.pass)  
m14 = glm(atiit ~ molod + sex + transport, family = "binomial", data = data4, na.action = na.pass)
m15 = glm(atiit ~ age + dosug + molod + sex + transport, family = "binomial", data = data4, na.action = na.pass)

AIC(global, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)
BIC(global, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)
Weights(AIC(global, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15))
Weights(BIC(global, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15))
?Weights
logLik(global); logLik(m1); logLik(m2); logLik(m3); logLik(m4); logLik(m5); logLik(m6); logLik(m7); logLik(m8) 
logLik(m9); logLik(m10); logLik(m11); logLik(m12); logLik(m13); logLik(m14); logLik(m15) 
