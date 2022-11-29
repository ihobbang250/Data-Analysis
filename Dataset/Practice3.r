#BMI 
weight = c(60, 72, 57, 90, 95, 72)
height = c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
BMI = weight / height ^ 2
BMI_ID = rep(0, length(weight)) #initialize

for( i in 1:length(weight)) {
  if (BMI[i] <= 18.5) {
    BMI_ID[i] <- 1
  }
  else if (BMI[i] <= 24.9) {
    BMI_ID[i] <- 2
  }
  else if (BMI[i] <= 29.9) {
    BMI_ID[i] <- 3
  }
  else if (BMI[i] <= 30) {
    BMI_iD[i] <- 4
  }
  else {
    BMI_ID[i] <- 5
  }
}
#indexing condition
BMI_ID2 <- BMI #copy
BMI_ID2[BMI <= 18.5] <- 1
BMI_ID2[BMI > 18.5 & BMI <= 24.9] <- 2
BMI_ID2[BMI > 24.9 & BMI <= 29.9] <- 3
BMI_ID2[BMI > 29.9 & BMI <= 30] <- 4
BMI_ID2[BMI > 30] <- 5

#setting working directory
setwd("C:\\Users\\green\\Desktop\\Programming\\Data Analysis\\R\\Dataset")

#PizzaData manipulation Ver1 (Replace)
#Read Data
data <- read.csv("pizzadata.csv")
#Copy Vec
AGE_G <- data$age1
#Index by Condition
AGE_G[data$age1 >= 18 & data$age1 <= 25] <- 1
AGE_G[data$age1 >= 26 & data$age1 <= 35] <- 2
AGE_G[data$age1 >= 36] <- 3
data[, "AGE_G"] <- AGE_G
head(data)

#Pizzadata Manipulation Ver2 (For loops)
data <- read.csv("pizzadata.csv")
#Initialize AGE_G
data$AGE_G <- 0
n <- dim(data)[1]
for (i in 1:n) {
  if(data$age1[i] >= 18 & data$age1[i] <= 25) (data$AGE_G[i] <- 1)
  if(data$age1[i] >= 26 & data$age1[i] <= 35) (data$AGE_G[i] <- 2)
  if(data$age1[i] >= 36) (data$AGE_G[i] <- 3)
}
head(data)

#Pizzadata Manipulation Ver3 (Custom Func)
Age_grouping <- function(x){
  if (x >= 18 & x <= 25) (y <- 1)
  else if (x <= 35) (y <- 2)
  else (y <- 3)
  return (y)
}
data <- read.csv("pizzadata.csv")
data$AGE_G <- apply(data[c("age1")], 1, Age_grouping)

#Annuity

annuity <- function(n, R, r) {
  xx <- rep(0, n-1)
  
  for( i in 1:(n+1)) {
    xx[i] <- R*(1+r)^(i-1)
  }
  y <- sum(xx)
  return (y)
}

annuity(n=10, R=400, r=0.05)
