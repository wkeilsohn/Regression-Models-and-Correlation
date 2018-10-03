#### William Keilsohn
#### Regression Models and Correlation

### Load Packages
library(dplyr)
library(ggplot2)
library(nlme)
library(lmodel2)
library(xlsx)


## Sea Urchin Gonad size is examined as a result of urchin height/diamiter
shapiro.test(URCD$Hght)### Test normality first...in this case this one is normal
shapiro.test(URCD$Diam)#Still normal
shapiro.test(URCD$Gonad)#All normal.

lmodel2(formula = Hght ~ Diam, data = URCD) ### Model 2
(20*0.6278744)-2.1414916
(80*0.6278744)-2.1414916

Reg.out12<-lm(Gonad~Diam,URCD) ### Model 1
summary(Reg.out12)

### Dog litter size as a function of mother/father age
shapiro.test(DOOD$Pups)#Normal
shapiro.test(DOOD$Moth)#Normal
shapiro.test(DOOD$Fath)#Normal

Reg.out2<-lm(Pups~Moth,DOOD)
summary(Reg.out2)

Reg.out.21<-lm(Pups~Fath,DOOD)
summary(Reg.out.21)

# Relationship of the function above is provided.
# Determine if there is a difference in the slope and intersecpt of function
PDS = -0.72
PDI = 15

LDS = -0.8502
LDI = 9.80606

SDS = 0.34176
SDI = 2.07948

Ts = (-0.72 - (-0.8502))/0.34176
Ti = (15 - 9.80606)/2.07948


# Pulse rate is compared to reaction time in humans. 
shapiro.test(RXND$beats)# Not normal
shapiro.test(RXND$time)# Normal
#So if one is normal, and one is not normal, we need to une a non-parametric test.

cor.test(RXND$beats, RXND$time, method = "spearman") #Spearman's rank correlation
#You get the same results regardless of which variable goes first so this is model 2.




