#example
solar.radiation <- c(11.1, 10.6, 6.3, 8.8, 10.7,
                    11.2, 8.9, 12.2)

#summary stat
mean(solar.radiation)
var(solar.radiation)
sd(solar.radiation)

#vector slicing by condition
a <- c(3, 6, 9)
a[a != 3]
a[a > 4 & a == 9]

#setting working directory
setwd("C:\\Users\\green\\Desktop\\Programming\\Data Analysis\\R\\Dataset")

#Read Data
data <- read.csv("test2.csv")
hist(data$exam, col="blue", main="hist", xlab="x", ylab ="y")
boxplot(data$exam)
plot(data$exam2 ~ data$exam)
plot(data$exam, data$exam2)
plot(exam2~exam, data=data, pch = 2)


#example2
gender <- c("M", "F", "M", "F", "F", "M", "F", "M")
length <- c(7.9, 6.5, 8.4, 5.5, 6.5, 8.0, 7.0, 7.5)
width <- c(2.3, 1.7, 2.6, 1.7, 1.9, 2.1, 1.8, 1.9)

indexfinger <- data.frame(gender, length, width)
indexfinger.M <- indexfinger[indexfinger$gender == 'M', ]
indexfinger.F <- indexfinger[indexfinger$gender == 'F', ]

par(mfrow = c(1, 2))
plot(indexfinger.M$length, indexfinger.M$width, col = "blue", pch = 15, main = "male finger")
plot(indexfinger.F$length, indexfinger.F$width, col = "red", pch = 12, main = "female finger")

par(mfrow = c(1, 1))
plot(indexfinger$length, indexfinger$width, type = "n")
points(indexfinger.M$length, indexfinger.M$width, col = "blue", pch = 15)
points(indexfinger.F$length, indexfinger.F$width, col = "red" , pch = 24)
legend("topleft", legend = c("male", "female"), col = c("blue", "red"), pch = c(15, 24))

#histogram#
EXAM <- c(30, 80, 73, 92)
hist(EXAM)

install.packages("MASS")
library(MASS)
sigma <- matrix(c(10, 3, 3, 2), 2, 2)
a <- mvrnorm(n= 1000, rep(0, 2), sigma)

?mvrnorm

install.packages("clipr")
library(clipr)
data <- read_clip_tbl()

##
data <- read.table("clipboard")
names(data) <- c("a", "b")

x <- 3
if ( x > 2 ) {
  y <- 2 * x
} else {
  y <- 3 * x
}

y <- ifelse(x > 2, 2 * x, 3 * x)

for (x in 1:4) {
  print(x)
}

x <- rep(1, 100)
for (i in 1:100){
  x[i] <- i
}
