##### Statistics for Ethanol-Formalin Comparison Paper #####

### Load Libraries ###
library(ggplot2)
library(ggpubr)
library(lme4)
library(AICcmodavg)
library(readr)
library(jtools)
library(MASS)
library(glmmTMB)
library(dplyr)
library(tidyverse)

##### Load Data ###
csv <- read_csv("Desktop/EtFrm2.csv")
View(csv)

#### basic plots ####
boxplot(Diversity~Medium, data=csv)
boxplot(PFG~Medium, data=csv)

boxplot(FilaPFG~Medium, data=csv)
boxplot(StrongPFG~Medium, data=csv)

boxplot(AvRating~Medium, data=csv)
boxplot(Total3~Medium, data=csv)
boxplot(Total2~Medium, data=csv)
boxplot(Total1~Medium, data=csv)

##### Plots to publish #######

#plot with jitter#
ggplot(data=csv, aes(Medium,Diversity, fill=Medium))+
  geom_boxplot()+
  geom_line(aes(group=Paired), position = position_dodge(0.2))+
  geom_point(aes(fill=Medium, group=Paired), position = position_dodge(0.2))+
  geom_jitter(aes(fill=Medium,group=Paired), width=0.15)

#Diversity#
ggplot(data=csv, aes(Medium,Diversity, fill=Medium))+
  geom_boxplot()+
  scale_fill_manual(breaks = csv$Medium,
                    values = c("#CDEBC5", "#0059b3"))+
  geom_line(aes(group=Paired, color="darkgrey"))+
  scale_color_manual(values=c('darkgrey'))+
  geom_point(aes(fill=Medium, group=Paired))+
  coord_cartesian(ylim=c(0,10))+
  labs(y="Morphotype Diversity", x="Preservation Medium")+
  theme(legend.position="none")
 
#PFG#  
ggplot(data=csv, aes(Medium,PFG, fill=Medium))+
  geom_boxplot()+
  scale_fill_manual(breaks = csv$Medium,
                    values = c("#CDEBC5", "#0059b3"))+
  geom_line(aes(group=Paired, color="darkgrey"))+
  scale_color_manual(values=c('darkgrey'))+
  geom_point(aes(fill=Medium, group=Paired))+
  labs(y="Parasites per Fecal Gram", x="Preservation Medium")+
  theme(legend.position="none")

#average rating
ggplot(data=csv, aes(Medium,AvRating, fill=Medium))+
  geom_boxplot()+
  scale_fill_manual(breaks = csv$Medium,
                    values = c("#CDEBC5", "#0059b3"))+
  geom_line(aes(group=Paired, color="darkgrey"))+
  scale_color_manual(values=c('darkgrey'))+
  geom_point(aes(fill=Medium, group=Paired))+
  labs(y="Average Rating", x="Preservation Medium")+
  theme(legend.position="none")

#average rating without points or lines
ggplot(data=csv, aes(Medium,AvRating, fill=Medium))+
  geom_boxplot()+
  scale_fill_manual(breaks = csv$Medium,
                    values = c("#CDEBC5", "#0059b3"))+
  labs(y="Average Rating", x="Preservation Medium")+
  theme(legend.position="none")


######is there a correlation between days since preservation and anything?######
plot(csv$DaysSince, csv$PFG)
abline(lm(DaysSince~PFG, data=csv))
Days<-lm(DaysSince~PFG, data=csv)
summary(Days)
#Residual standard error: 59.72 on 40 degrees of freedom
#Multiple R-squared:  0.03158,	Adjusted R-squared:  0.007369 
#F-statistic: 1.304 on 1 and 40 DF,  p-value: 0.2602
confint(Days)
# PFG          -0.1382489   0.03841691

plot(csv$DaysSince, csv$Diversity)
DaysD<-lm(DaysSince~Diversity, data=csv)
summary(DaysD)
#Residual standard error: 60.19 on 40 degrees of freedom
#Multiple R-squared:  0.01619,	Adjusted R-squared:  -0.00841 
#F-statistic: 0.6581 on 1 and 40 DF,  p-value: 0.422
confint(DaysD)
# Diversity   -13.91466   5.943844


plot(csv$DaysSince, csv$AvRating)
DaysAve<-lm(DaysSince~AvRating, data=csv)
summary(DaysAve)
# Residual standard error: 58.16 on 37 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.0007392,	Adjusted R-squared:  -0.02627 
#F-statistic: 0.02737 on 1 and 37 DF,  p-value: 0.8695
confint(DaysAve)
# AvRating    -29.48352  34.72643

DaysAveEth<-lm(DaysSince~AvRating, data=subset(csv,Medium=="Ethanol"))
summary(DaysAveEth)
#Residual standard error: 58.3 on 18 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.002665,	Adjusted R-squared:  -0.05274 
#F-statistic: 0.0481 on 1 and 18 DF,  p-value: 0.8289
confint(DaysAveEth)
# AvRating    -76.90578  62.36657

DaysAveForm<-lm(DaysSince~AvRating, data=subset(csv,Medium=="Formalin"))
summary(DaysAveForm)
#Residual standard error: 61.07 on 17 degrees of freedom
#(2 observations deleted due to missingness)
#Multiple R-squared:  0.001844,	Adjusted R-squared:  -0.05687 
#F-statistic: 0.03141 on 1 and 17 DF,  p-value: 0.8614
confint(DaysAveForm)
#AvRating    -364.4565  307.969


Dayslmer <-lmer(DaysSince~PFG+(1|Monkey), data=csv)
summary(Dayslmer)
summ(Dayslmer)
#p=0.46
DaysDlmer <-lmer(DaysSince~PFG+(1|Monkey), data=csv)
summary(DaysDlmer)
summ(DaysDlmer)
# p = 0.46


