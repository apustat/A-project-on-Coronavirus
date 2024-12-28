setwd("C:/Users/.../COVID 19_Project")
data=read.csv("covid-19-data.csv", header=TRUE)
attach(data)
names(data)

library(tidyverse)
library(ggplot2)
library(gridExtra)
library("ggpubr")
library(dplyr)

###Basic trend plot
data$DATE <- as.Date(data$Date, "%m/%d/%Y")
library("tidyverse")
df <- data %>%
  select(DATE,New.Cases ,New.Deaths, New.Recoveries ) %>%
  gather(key = "variable", value = "value", -DATE)

f1<-ggplot(df, aes(x = DATE, y = value)) + 
  geom_line(aes(color = variable),size = 0.6) +
  scale_color_manual(values = c("midnightblue", "firebrick", "orange")) +
  labs(x = "Day", y = "New cases")+ scale_x_date(date_labels = "%b %d",date_breaks = "1 month")+
  theme(
    plot.title = element_text(hjust = 0.5,color = "black", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text=element_text(size=14)
  )+
  theme(legend.position="top")+
  theme(
    legend.title = element_text(color = "blue", size = 14),
    legend.text = element_text(color = "black", size = 14)
  )


df <- data %>%
  select(DATE,Total.Cases ,Total.Deaths, Total.Recoveries ) %>%
  gather(key = "variable", value = "value", -DATE)

f2<-ggplot(df, aes(x = DATE, y = value)) + 
  geom_line(aes(color = variable),size = 0.6) + 
  scale_color_manual(values = c("midnightblue", "firebrick", "orange")) +
  labs(x = "Day", y = "Total cases",caption="Datasource: kaggle & wikipedia")+ scale_x_date(date_labels = "%b %d",date_breaks = "1 month")+
  theme(
    plot.title = element_text(hjust = 0.5,color = "black", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text=element_text(size=14)
  )+
  theme(legend.position="top")+
  theme(
    legend.title = element_text(color = "blue", size = 14),
    legend.text = element_text(color = "black", size = 14)
  )

grid.arrange(f1,f2,ncol=1, nrow=2)

##Lineplot for tests
data$DATE <- as.Date(data$Date, "%m/%d/%Y")

p1<-ggplot(data=data, aes(x=DATE, y=Tests)) +
  geom_line(stat = "identity",linetype="solid", color="blue", size=0.7)+geom_point(color="red", size=0.7) +
  labs( x = "Day", y = "Daily test") + scale_x_date(date_labels = "%b %d",date_breaks = "1 month")

p1+theme(
  plot.title = element_text(hjust = 0.9,color = "black", size = 14),
  plot.subtitle = element_text(hjust = 0.5),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  axis.text=element_text(size=14)
)




##Our collected data
library(foreign)
data=read.spss("covid19_data_limited.sav", header=TRUE)
attach(data)
names(data)

##Correlation Between Daily Internet Usage Before and During COVID-19
cor(DailyInternetUsageBeforeCOVID19, DailyInternetUsageDuringCOVID19)
plot(DailyInternetUsageBeforeCOVID19, DailyInternetUsageDuringCOVID19)

ggplot(data=data.frame(data), aes(x=DailyInternetUsageBeforeCOVID19, y=DailyInternetUsageDuringCOVID19)) + 
  geom_point()+
  geom_smooth(method=lm)+
  labs( x = "Internet Usage (Before COVID-19)", y = "Internet Usage (During COVID-19)") +
  theme(
  plot.title = element_text(hjust = 0.9,color = "black", size = 14),
  plot.subtitle = element_text(hjust = 0.5),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  axis.text=element_text(size=14)
)

###OLS model for daily usage of internet during COVID-19
fit <- lm(DailyInternetUsageDuringCOVID19 ~ Age + Gender + MaritalStatus+
            InternetType+DailyInternetUsageBeforeCOVID19+AffectSudy+
            MentalHealthChallenge, data=data)
summary(fit)
hist(fit$residuals)##normally distributed
qqnorm(fit$residuals)
qqline(fit$residuals)
p1<-ggplot(fit, aes(x=fit$residuals))+
  geom_histogram()+
  labs(title="Residual plot", x="Residuals", y="Count")+
  theme(
    plot.title = element_text(hjust = 0.5,color = "black", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text=element_text(size=14)
  )

p2<-ggplot(fit, aes(sample = fit$residuals)) + stat_qq() + geom_abline(slope=2.6, color="blue")+
  labs(title="QQ plot", x="Theoretical", y="Sample")+
  theme(
    plot.title = element_text(hjust = 0.5,color = "black", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text=element_text(size=14)
  )
grid.arrange(p1,p2,ncol=2, nrow=1)

##chi-square association
tbl = table(QuarrelsomeEnvironment, MentalHealthChallenge)
chisq.test(tbl)


###Logistic model for mental health challenge
mylogit <- glm(MentalHealthChallenge ~ Age +LeavingHome+QuarrelsomeEnvironment+WFHorOCeffective+
                 FeelingCOVID19 , data = data, family=binomial(link="logit"))
summary(mylogit) ##no multicollinearity(checked by spss)
anova(mylogit,test='Chisq')


##Confusion or Classification Matrix
library(caTools)
set.seed(88)
data=data.frame(data)
split = sample.split(MentalHealthChallenge, SplitRatio = 0.75)##splitting data into train and test
split
dataTrain = subset(data, split == TRUE)
dataTest = subset(data, split == FALSE)
nrow(dataTrain)
nrow(dataTest)
mylogit <- glm(MentalHealthChallenge ~ Age +LeavingHome+QuarrelsomeEnvironment+WFHorOCeffective+
                 FeelingCOVID19, data = dataTrain, family=binomial(link="logit"))
predictTrain = predict(mylogit, type="response")
tapply(predictTrain, dataTrain$MentalHealthChallenge, mean)
table(dataTrain$MentalHealthChallenge, predictTrain > 0.5)##cutoff=0.5
predictTest = predict(mylogit, type = "response", newdata = dataTest)
table(dataTest$MentalHealthChallenge,predictTest >= 0.5)

