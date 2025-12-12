setwd("C:\\Users\\user\\Desktop\\STAL4207")
setwd("C:/Users/user/Desktop/STAL4207")

getwd()

dat <- read.csv("Ex_data_1.csv", header=TRUE)

dim(dat)
colnames(dat) ## to get names of the column

dat[ ,1] ##to get column 1
dat$Y
dat[,2] ##to get column 2

dat[5,1] ## to get colm 1 and row 5

summary(dat)

boxplot(dat$Y)
boxplot(dat$X)

#library("readxl") # for both xls and xlsx files
#my_data <- read_excel("Ex_data_1.xlsx")

plot(dat$X, dat$Y)
plot(dat$X, dat$Y, pch=16, col= "red", xlab = "X", ylab = "Y", main = "Scatter plot of X and Y")

reg <- lm(Y ~ X, data = dat)
reg
coef(reg)

abline(reg, lwd=2, col="blue") ###Fitted line 


round(fitted(reg),2)
round(resid(reg),2) 


summary(reg)$coefficients


summary(reg)$r.squared ### Coefficients of determination
summary(reg)$adj.r.squared ### adj Coefficients of determination
summary(reg)$adj.r.squared ### adj Coefficients of determination

anova(reg) ##ANOVA 

anova(reg)$"F value"[1]

anova(reg)$"Df"[1]
anova(reg)$"Pr(>F)"[1]

cor(dat$X,dat$Y, method = "pearson")


summary(reg)$sigma
summary(reg)$sigma**2 #MSE

##test
summary(reg)$coefficients[2,2]  ##std of b_1
summary(reg)$coefficients[2,3]  ##t-value of b_1
summary(reg)$coefficients[2,4]  ##p-value of b_1


confint(reg, level = 0.95) ##Confidence interval


###########Multiple Linear Regression
dat2 <- read.csv("Example_2.csv", header=T)

reg2 <- lm(Y ~ X1 + X2, data = dat2)


y <- as.vector(dat2$Y)
x <- as.matrix(cbind(1, dat2$X1, dat2$X2))


reg2 <- lm(Y~X1 + X2, data=dat2)

b_coef <- solve(t(x)%*%x) %*%t(x)%*%y

anova(reg2)
fitted(reg2)                

resid(reg2)

   
library(ggcorrplot)  
corr <- cor(dat2)
ggcorrplot(corr, type="lower", lab=TRUE)


###CRD
trt1 <- c(23,36,31, 33)
trt2 <- c(42,26,47, 34)
trt3 <- c(47,43,43,39)

y <- c(trt1, trt2, trt3)

x <- c(rep("trt1",4), rep("trt2",4), rep("trt3",4))


res <- lm(y~x)
summary(res)
anova(res)



setwd("C:\\Users\\user\\Desktop\\BDHS_data\\BDHS 2022\\BDHS 2022\\BDIR81SV")

library(foreign)


dat <- read.spss("BDIR81FL.sav", to.data.frame=TRUE)
dim(dat)

Y <- dat$V133
X <- dat$V101

dat_2 <- data.frame(Y,X)

tapply(Y, X, sum)


lm(Y~x)
