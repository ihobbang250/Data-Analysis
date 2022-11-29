setwd("C:\\Users\\green\\Desktop\\Programming\\Data Analysis\\R\\Dataset")

data <- read.csv("Table6_1.csv")

model <- lm(lnconsump~ lndpi + lnwealth+interest, data = data)
summary(model)
cor(data$lndpi, data$lnwealth)

par(mfrow = c(2, 2))
plot(model$residuals ~ model$fitted, main = "residual-fitted")
plot(model$residuals ~ data$time, main = "time plot of residual")
plot(model$residuals ~ data$time, type = "l", main = "time plot of residual type = l")
plot(model$residuals ~ data$time, type = "b", main = "time plot of residual type = b")

par(mfrow=c(1, 1))
acf(model$residuals, lag.max = 24)

library(lmtest)
dwtest(model)
##AutoCorr Solution1 -> Dummy Variable##
modelwithtrend <- lm(lnconsump~ lndpi + lnwealth + interest + time, data = data)
summary(modelwithtrend)
dwtest(modelwithtrend)
##AutoCorr Solution2 -> Past Value to New Variable##
nn = dim(data)[1]
data$LCm1 <- c(NA, data$lndpi[1:nn-1])
modelwithLCm1 <- lm(lnconsump~ lndpi + lnwealth + interest + LCm1, data = data)
acf(modelwithLCm1$residuals)
##AutoCorr Solution3 -> Difference##
ynew <- diff(data$lnconsump, difference = 1)
lndpinew <- diff(data$lndpi, differences = 1)
lnwealthnew <- diff(data$wealth, differences = 1)
interestnew <- diff(data$interest, differences = 1)

modelwithdiff <- lm(ynew~ lndpinew+ lnwealthnew+ interestnew, data = data)