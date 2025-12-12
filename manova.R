set.seed(123)
group <- gl(3, 20, labels = c("Method1", "Method2", "Method3"))
math <- rnorm(60, mean = rep(c(75, 80, 85), each=20), sd = 5)
reading <- rnorm(60, mean = rep(c(70, 75, 80), each = 20), sd = 5)
writing <- rnorm(60, mean = rep(c(72, 76, 78), each = 20), sd = 5)

df <- data.frame(group, math, reading, writing)
head(df)


#Create a Manova Model
manova_ml <- manova(cbind(math, reading, writing)~group, data = df)
summary(manova_ml)
summary(manova_model, test = "Wilks")   # Wilks' Lambda

#Post-hoc Anova for each response
summary.aov(manova_ml)
#This tells you which individual dependent variables are significantly affected.

#Assumption Check
install.packages("MVN")
library(MVN)
#Multivariate Normality
mvn(df[, 2:4], mvnTest = "mardia")
#Homogeneity of covariance


