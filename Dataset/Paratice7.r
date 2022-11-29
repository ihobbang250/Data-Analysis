setwd("C:\\Users\\green\\Desktop\\Programming\\Data Analysis\\R\\Dataset")

calcul <-function(x, y) {
  return (log(x) - log(y))
}
data <- read.csv("MSFTGooglefinance.csv")
dim(data$Adj.Close)
data$logpt <- apply(data$Adj.Close, 1, calcul)

cp2 <- read.csv("capm2_rf.csv")
GE.