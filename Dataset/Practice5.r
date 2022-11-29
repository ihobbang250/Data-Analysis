#setting working directory
setwd("C:\\Users\\green\\Desktop\\Programming\\Data Analysis\\R\\Dataset")
#data1 example
data <- read.csv("Table2_1.csv")

model<- lm(output~labor+capital-1, data = data)
summary(model) 
loglogmodel <- lm(lnoutput~lnlabor+lncapital, data = data)
summary(loglogmodel)

par(mfrow = c(1, 2))
plot(model$residuals~model$fitted)
plot(loglogmodel$residuals~loglogmodel$fitted)

library(lmtest)
bptest(loglogmodel)
gqtest(loglogmodel)

# Data2 Example
data2 <- read.csv("Table2_5.csv")
head(data2)
model <- lm(rgdp~time, data = data2)
summary(model)

par(mfrow = c(1,1))
plot(model$residuals~data2$time)
plot(rgdp~time, data = data2)
abline(model, col="red")

model2 <- lm(lnrgdp~time, data = data2)
summary(model2)
par(mfrow = c(1,2))
plot(model$residuals~data2$time)
plot(model2$residuals~data2$time, data = data2)
