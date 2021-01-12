rm(list=ls())

#Model 1 (D:Happiness, I:GDP.per.capita)
happy<-read.csv("2020.csv")
#happy<-happy[-c(153,103),]
str(happy)
sum(is.na(happy$Happiness))
sum(is.na(happy$GDP.per.capita))
lmhappy<-lm(Happiness~GDP.per.capita, data = happy)
plot(Happiness~GDP.per.capita, data = happy)
abline(lmhappy, col = "blue")
summary(lmhappy)
#Yes, this model test shows that it is statistically significant.  
#As the F-statistic reveals that the p value is 2.2e-16, which is less than 0.05, that is an indication that it is statistically significant.
#Furthermore, the multiple R-squared is 0.6012 and the adjusted R-squared is 0.5986, which explains about 60% of the variance.
#Additionally, as the hypothesis test (GDP.per.Capita) gives a p value of 2e-16, which is less than 0.05, this is another indication that it is statistically significant
#and that GDP.per.capita does effect Happiness.
#Equation:  Happiness = -1.19865 + 0.71774 (GDP.per.capita)
par(mfrow=c(2,2))
plot(lmhappy)
#The Residuals vs Fitted graph shows a random pattern.
#The Normal Q-Q graph has points that are very close to the line.
#The Scale-Location graph has a flatline with points above and below.
par(mfrow=c(1,1))
plot(cooks.distance(lmhappy))
plot(lmhappy, which = c(4))
4/153
#Most of the data is around 0.0261
hist(lmhappy$residuals)
mean(lmhappy$residuals) 
#The mean is close to 0
library(lmtest)
lmtest::bptest(lmhappy)
#p is 0.3876, which is greater than 0.05 so it is the model is homoscedastic.



#Model 2 (D:Happiness,I:Perceptions.of.corruption)
corrupt<-read.csv("2020.csv")
#corrupt<-corrupt[-150,]
str(corrupt)
sum(is.na(corrupt$Happiness))
sum(is.na(corrupt$Perceptions.of.corruption))
lmcorrupt<-lm(Happiness~Perceptions.of.corruption, data = corrupt)
plot(Happiness~Perceptions.of.corruption, data = corrupt)
abline(lmcorrupt, col = "blue")
summary(lmcorrupt) #7.428e-08, {0.175, 0.1695}, 7.43e-08
par(mfrow=c(2,2))
plot(lmcorrupt)
par(mfrow=c(1,1))
plot(cooks.distance(lmcorrupt))
plot(lmcorrupt, which=c(4))
4/153
#Most of the data is around 0.0261, except for row 150, which is an outlier.
#You should remove row 150 because in the cook's distance graph, it is near 0.5 and not near 0.0261 at all.  
corrupt<-corrupt[-150,] #remove outlier
lmcorrupt<-lm(Happiness~Perceptions.of.corruption, data = corrupt)
summary(lmcorrupt)
#Yes, this model test shows that it is statistically significant.  
#As the F-statistic reveals that the p value is 3.756e-10, which is less than 0.05, that is an indication that it is statistically significant.
#Furthermore, the multiple R-squared is 0.2308 and the adjusted R-squared is 0.2257, which explains about 23% of the variance.
#Additionally, as the hypothesis test (Perceptions.of.corruption) gives a p value of 3.76e-10, which is less than 0.05, this is another indication that it is statistically significant
#and that Perceptions.of.corruption does effect Happiness.
#Equation: Happiness = 7.7827 - 3.1154 (Perceptions.of.corruption)
par(mfrow=c(2,2))
plot(lmcorrupt)
#The Residuals vs Fitted graph shows a random pattern.
#The Normal Q-Q graph has points that are very close to the line.
#The Scale-Location graph has a flatline with points above and below.
par(mfrow=c(1,1))
plot(cooks.distance(lmcorrupt))
plot(lmcorrupt, which=c(4))
hist(lmcorrupt$residuals)
mean(lmcorrupt$residuals)
#The mean is close to 0
library(lmtest)
lmtest::bptest(lmcorrupt)
#Since P is 0.3393, which is greater than 0.05, the model is homoscedastic
#By removing the outlier, our model was able to improve.


#Model3 (D:Happiness,I:Generosity)
generosity<-read.csv("2020.csv")
#generosity<-generosity[-c(133,142),]
str(generosity) #153 observations
sum(is.na(generosity$Happiness))
sum(is.na(generosity$Generosity))
lmgenerosity<-lm(Happiness~Generosity, data = generosity)
plot(Happiness~Generosity, data = generosity)
abline(lmgenerosity, col = "blue")
summary(lmgenerosity) #0.3964, {0.004767,-0.001824}, 0.396}
par(mfrow=c(2,2))
plot(lmgenerosity)
#The Residuals vs Fitted graph shows a random pattern.
#The Normal Q-Q graph has points that are very close to the line.
#The Scale-Location graph has a flatline with points above and below.
par(mfrow=c(1,1))
plot(cooks.distance(lmgenerosity))
plot(lmgenerosity, which=c(4))
4/153
#Most of the data is around 0.0261 except for row 133 and 142, which are outliers.
#You should remove row 133 and 142 because in the cook's distance graph, both points are near 0.1 and not near 0.0261 at all.  
generosity<-generosity[-c(133,142),]
lmgenerosity<-lm(Happiness~Generosity, data = generosity)
summary(lmgenerosity)
#This model test shows that it is NOT statistically significant.
#As the F-statistic reveals that the p value is 0.0958, which is greater than 0.05, that is an indication that it is NOT statistically significant.
#Furthermore, the multiple R-squared is 0.01851 and the adjusted R-squared is 0.01192, which explains only about 2% of the variance.
#Additionally, as the hypothesis test (Generosity) gives a p value of 0.0958, which is greater than 0.05, is another indication that it is NOT statistically significant
#and that Generosity does NOT effect Happiness.
#Equation: 5.51507 + 1.06535 (Generosity)
par(mfrow=c(2,2))
plot(lmgenerosity)
#The Residuals vs Fitted graph shows a random pattern.
#The Normal Q-Q graph has points that are very close to the line.
#The Scale-Location graph has a flatline with points above and below.
par(mfrow=c(1,1))
plot(cooks.distance(lmgenerosity))
plot(lmgenerosity, which=c(4))
4/151
hist(lmgenerosity$residuals)
mean(lmgenerosity$residuals)
#The mean is close to 0
library(lmtest)
lmtest::bptest(lmgenerosity)
#P is 0.019, which is below 0.05, so the model is heteroskedastic
#Although by removing the outlier, the model was able to improve, 
#it was not able to improve enough to make it statistically significant or homoscedastic.
