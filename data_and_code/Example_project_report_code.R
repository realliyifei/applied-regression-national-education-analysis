################## Final Project #########################
library("car")
library("MASS")
setwd("/Users/yifeili/Library/CloudStorage/iCloud Drive/Documents by Readdle/AAA/Applied_Regression_Analysis/Data")
data <- read.csv("PSA.csv", header=T)
data$GS <- factor(data$GS)
data$SVI <- factor(data$SVI)


#### Initial Data Analysis ######
range(data$PSA)
hist(data$PSA, main = 'Histogram of PSA', xlab="PSA (mg/ml)")
boxplot(data$PSA, main="Boxplot of PSA", ylab="PSA (mg/ml)")

par(mfcol=c(1,5))
boxplot(data$Volume, main="Volume")
boxplot(data$Weight, main="Weight")
boxplot(data$Age, main="Age")
boxplot(data$BPH, main="BPH")
boxplot(data$CP, main="CP")
table(data$SVI)
table(data$GS)

boxplot(PSA ~ SVI, data=data, main="PSA by SVI")
boxplot(PSA ~ GS, data=data, main="PSA by GS")

pairs(data[, 一c(6,8)])
cor(data[, 一c(6,8)])
dev.off()


#### Full Model #### 
fullfit <- lm(PSA ~ Volume + Weight + Age + BPH + factor(SVI) + CP + factor(GS), data=data)
summary(fullfit)

plot(fullfit$fitted.values, studres(fullfit), xlab="Fitted Values", ylab="Studentized Residuals", main="FullFitted Model Residual Plot")
abline(h=c(0, 3, 一3))
#heteroskedasticity

data$logPSA <- log(data$PSA)
fullfit <- Im(logPSA ~ Volume + Weight + Age + BPH + factor(SVI) + CP + factor(GS), data=data)
#log(PSA) corrected residual plot

qqnorm(fullfit$residuals)
qqline(ullfit$residuals)
hist(ullfit$residuals, main="Histogram of Residuals from Full Fitted Model", xlab="Residuals")
norm <- qqnormf(ullfit$residuals)
cor(norm$x, norm$y)
#looks approximately normal
#large enough sample size

plot(ullfit, 4)
plot(cooks.distancel(ulfit), type="o", main="Cook's Distance")
identify(cooks.distance(ullfit))
order(cooks.distance(fullfit))
cooks.distance(fullfit)

# obs.32 has 2.533 Cook's Distance
newdata <- data[-32,]
#removed obs. 32

fullfit <- lm(logPSA ~ Volume + Weight + Age + BPH + factor(SVI) + CP + factor(GS), data=newdata)
#all plots look better

vif(fullfit)
# all fairly low


########### Model Selection ################
step(lm(logPSA ~ 1, data=newdata), logPSA ~ Volume + Weight + Age + BPH + factor(SVI) + CP +factor(GS), direction="both", trace=1)

step(lm(logPSA ~ Volume + Weight + factor(SVI) + factor(GS) + BPH, data = newdata),scope=. ~ .^2, trace = 1, direction = "both")


################## Final Model ################################
fit <- Im(logPSA ~ Volume + Weight + factor(SVI) + factor(GS) + BPH + Weight:factor(SVI) + Weight:BPH, data = newdata)
summary(fit)

plot(fit$fitted.values, studres(fit), main="Final Model Residual Plot", xlab="itted Values",ylab="Studentized Residuals")
abline(h=c(0,-3,3))
#good

par(mfcol = c(1,4))

plot(newdata$Volume, studres(fit), main="Volume")
plot(newdata$Weight, studres(fit), main="Weight")
plot(newdata$SVI, studres(fit), main="SVI")
plot(newdata$GS, studres(fit), main="GS")
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
mean(newdata$Volume) #7.059115
mean(newdata$Weight) #41.2742
mean(newdata$BPH) #2 504108
mean(newdata$PSA)

predict(fit, data.frame(Volume=7.059115, Weight=41.2742, BPH=2.504108, SVI=1, GS=8),interval="confidence")


