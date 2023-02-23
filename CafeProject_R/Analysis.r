library(corrplot)
library(dplyr)
library(HH)
library(leaps)
library(lmtest)
library(car)
library(gvlma)
library(data.table)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv("Dataset/finaldata.csv") # grdp 영화관 변수 다중공선성 존재 -> grdp 제거
data <- data[-c(1,2)] # 불필요 컬럼 제거

cor_data <- cor(data) # 상관계수플롯
par(mfrow = c(1,1))
corrplot(cor_data,method = "shade", addCoef.col = "black") 

### Parameter Check
m <- regsubsets(점포수~., data=data) # All Subset Regression Method
summary(m)

### 5 - x model # Discard (지하철역수 해석 문제)
model1 <- lm(점포수~ 평균생활인구+ 평균공시지가+ log(지하철역수)+ I(노령화지수/100)+ 영화관+0, data=data)
summary(model1)

### 4 - x model
# Modeling by All Subset
model2 <- lm(점포수~ log(평균생활인구)+ 평균공시지가+ 성비+ 영화관, data=data)
summary(model2)
# Modeling by correlation
data1 <- data[-c(1),] # Influential Data Exception
model3 <- lm(점포수~ log(평균생활인구)+ 평균공시지가+I(log(노령화지수)-log(100))+ 영화관 +0, data=data1)
summary(model3)


# 정규성 검정 // H0:자료 정규분포 따른다 // p-value > 0.05 -> H0 accept
shapiro.test(model3$residuals)

# 등분산성 검정 // H0: 등분산 가정함 // p value > 0.05 -> H0 accept
ncvTest(model3)


# 자기상관 검정 // H0: 자기상관이 아니다 // 자기상관 p-value > 0.05 -> H0 accept
dwtest(model3) 

# 이상치 테스트 // H0: 이상치 존재 // Bonferroni p < 0.05 -> H0 reject
car::outlierTest(model3)

# 다중공선성 검정 // vif < 10
vif(data[-c(2)]) 

shapiro.test(model2$residuals)
ncvTest(model2)
dwtest(model2)
car::outlierTest(model2)
# 가정 적합성 Double-Check
# Global stat: 선형성
# Link Function : 종속변인 연속형 변수 여부
# Heteroscedasticity: 이분산성
gvlma(model2, alphalevel = 0.01)
gvlma(model3, alphalevel = 0.01)

#plot Check
# 1. 독립성 -> plot상 어떤 관계가 안보여야한다
# 2. 정규성 -> 점선에서 크게 벗어나지 않아야한다
# 3. 등분산성 -> plot상 어떤 관계가 안보여야한다
# 4. 잔차 이상치 -> cook distance 이내 관측되어야한다
par(mfrow = c(2,2))
plot(model2)
plot(model3)

# plot 각 변수별 선형성 관측체크
crPlots(model2)
crPlots(model3)

