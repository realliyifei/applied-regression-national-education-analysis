################## Final Project #########################

library('car')
library('MASS')

#setwd("/Users/yifeili/Library/CloudStorage/data_and_code")
data <- read.csv("Education.csv", header=T)
data$REDSTATE <- factor(data$REDSTATE)


#### Initial Data Analysis ######
range(data$SATSCORE)
hist(data$SATSCORE, main = 'Histogram of SAT Score', xlab="SAT Score")
boxplot(data$SATSCORE, main = 'Boxplot of SAT Score', ylab="SAT Score")

par(mfcol=c(1,4))
boxplot(data$TAKEPCT, main="TAKEPCT")
boxplot(data$EXPEND, main="EXPEND")
boxplot(data$POVRATE, main="POVRATE")
boxplot(data$MEDINCOME, main="MEDINCOME")
table(data$REDSTATE)

boxplot(SATSCORE ~ REDSTATE, data=data, main="SATSCORE by REDSTATE")

my_cols <- c("#FC4E07","#00AFBB")  
pairs(data[,-c(1,5)],col=my_cols[data$REDSTATE])
cor(data[, -c(1,5)])
dev.off()

#### Full Model #### 
fullfit <- lm(SATSCORE ~ TAKEPCT + EXPEND + factor(REDSTATE) + POVRATE + MEDINCOME, data=data)
summary(fullfit)

plot(fullfit$fitted.values, studres(fullfit), xlab="Fitted Values", ylab="Studentized Residuals", main="Full Fitted Model Residual Plot")
abline(h=c(0, 3, -3))
#heteroskedasticity

#data$logSATSCORE <- log(data$SATSCORE)
#fullfitlog <- lm(logSATSCORE ~ TAKEPCT + EXPEND + factor(REDSTATE) + POVRATE + MEDINCOME, data=data)
#log(PSA) corrected residual plot

plot(fullfitlog$fitted.values, studres(fullfitlog), xlab="Fitted Values", ylab="Studentized Residuals", main="Full Fitted Model Residual Plot")
abline(h=c(0, 3, -3))
#heteroskedasticity

qqnorm(fullfit$residuals)
qqline(fullfit$residuals)

#qqnorm(fullfitlog$residuals)
#qqline(fullfitlog$residuals)

hist(fullfit$residuals, main="Histogram of Residuals from Full Fitted Model", xlab="Residuals")
norm <- qqnorm(fullfit$residuals)
cor(norm$x, norm$y)
#looks approximately normal
#large enough sample size

plot(fullfit, 4)
plot(cooks.distance(fullfit), type="o", main="Cook's Distance")
identify(cooks.distance(fullfit))
order(cooks.distance(fullfit))
cooks.distance(fullfit)

#no need any change for current linear regression model

vif(fullfit)
# all fairly low, far less than 10


########### Model Selection ################
step(lm(SATSCORE ~ 1, data=data), SATSCORE ~ TAKEPCT + EXPEND + factor(REDSTATE) + POVRATE + MEDINCOME, direction="both", trace=1)

step(lm(SATSCORE ~ TAKEPCT + POVRATE + factor(REDSTATE), data=data), scope=. ~ .^2, trace = 1, direction = "both")


################## Final Model ################################
fit <- lm(SATSCORE ~ TAKEPCT + POVRATE + factor(REDSTATE), data=data)
summary(fit)
summary(fit)$sigma^2

plot(fit$fitted.values, studres(fit), main="Final Model Residual Plot", xlab="Fitted Values",ylab="Studentized Residuals")
abline(h=c(0,-3,3))
#good

par(mfcol = c(1,3))

plot(data$TAKEPCT, studres(fit), main="TAKEPCT")
plot(data$POVRATE, studres(fit), main="POVRATE")
plot(data$REDSTATE, studres(fit), main="REDSTATE")
#all look good

qqnorm(fit$residuals)
qqline(fit$residuals)
#good

plot(fit, 4)
#moderately high, not too concerned

vif(fit)
#good. Interactions are moderately high, but expected
#This is the best model


#### Interpretation ####
mean(data$TAKEPCT) #39.17647
mean(data$POVRATE) #13.05294
mean(data$SATSCORE)

predict(fit, data.frame(TAKEPCT=39.17647, POVRATE=13.05294, REDSTATE=1),interval="confidence")
predict(fit, data.frame(TAKEPCT=39.17647, POVRATE=13.05294, REDSTATE=2),interval="confidence")
