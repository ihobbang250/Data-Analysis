#setting working directory
setwd("C:\\Users\\green\\Desktop\\Programming\\Data Analysis\\R\\Dataset")

####Example1. ROE-IT####
#step1. read data
iroed <- read.csv("ITroe.csv")

#step2. check data
head(iroed)
dim(iroed)

#step3. scatter plot, Correlation, Coeffient
plot(ROE~IT, data=iroed)
cor(iroed$ROE, iroed$IT)

#step4. Model fitting
model.iroed <- lm(ROE~IT, data = iroed)
summary(model.iroed)

####Example2. WAGE####
data <- read.csv("Table1_1.csv")
plot(wage ~ education, data = wage)
head(wage)
model.wage <- lm(wage ~ education+age, data = wage )
summary(model.wage)
# H0: B1 = 0, Ha: B1 != 0
# 추정 ols b1 = 1.23
# bk 확률분포 b1/s.e(b1) -> t분포 따를시 H0 True t분포 데이터량 n-k
# n - k 에서 k는 b0,b1,b2 갯수 
# t분포 평균 0 이기에 t value 절대값 높을수록 p-value 작아진다
# P-value 와 알파값을 통하여 H0 reject 여부 결정


##Example3. log wage
data <- read.csv("Table1_1.csv")
data$logwage <- log(data$wage)
model <- lm(wage~as.factor(female)+ as.factor(nonwhite)+ as.factor(union) 
            + education+ exper, data = data)
model3 <- lm(logwage~as.factor(female)+ as.factor(nonwhite)+ as.factor(union)
             + education+ exper, data= data)
summary(model3)

par(mfrow=c(1, 2))
plot(model$residuals~ model$fitted.values, main= "before trans")
abline(h=0, col="red")
plot(model3$residuals ~ model3$fitted.values, main= "after log trans")

model4 <- lm(logwage~female+nonwhite+union+education+exper, data = data)
summary(model4)
