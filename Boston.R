library(MASS)
library(ggplot2)
library(scales)
library(arules)
library(gridExtra)
data("Boston")
head(Boston)
summary(Boston)
pairs(Boston)

#A


p1 <- ggplot(data = Boston, aes(x = medv)) + geom_histogram()
p2 <- ggplot(data = Boston, aes(x = crim)) + geom_histogram()
p3 <- ggplot(data = Boston, aes(x = zn)) + geom_histogram()
p4 <- ggplot(data = Boston, aes(x = indus)) + geom_histogram()
grid.arrange(p1, p2, p3, p4, ncol=2)


p5 <- ggplot(data = Boston, aes(x = chas)) + geom_histogram()
p6 <- ggplot(data = Boston, aes(x = nox)) + geom_histogram()
p7 <- ggplot(data = Boston, aes(x = rm)) + geom_histogram()
p8 <- ggplot(data = Boston, aes(x = age)) + geom_histogram()
grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(data = Boston, aes(x = dis)) + geom_histogram()
p10 <- ggplot(data = Boston, aes(x = rad)) + geom_histogram()
p11 <- ggplot(data = Boston, aes(x = tax)) + geom_histogram()
p12 <- ggplot(data = Boston, aes(x = ptratio)) + geom_histogram()
grid.arrange(p9, p10, p11, p12, ncol=2)

p13 <- ggplot(data = Boston, aes(x = black)) + geom_histogram()
p14 <- ggplot(data = Boston, aes(x = lstat)) + geom_histogram()
grid.arrange(p13, p14, ncol=2)

#B

Boston[["crim"]] <- ordered(cut(Boston[["crim"]], c(0,0.5,1,5,100)), labels = c('very-low-crime','low-crime','moderate-crime','high-crime'))
Boston[["zn"]] <- ordered(cut(Boston[["zn"]], c(-1,10,20,50,100)), labels = c('non-residential','small-residence','moderate-residence','large-residence'))
Boston[["indus"]] <- ordered(cut(Boston[["indus"]], c(0,5,10,20,30)), labels = c('very-low-business-concentration','low-business-concentration','moderate-business-concentration','high-business-concentration'))
Boston[["chas"]] <- NULL
Boston[["nox"]] <- NULL
Boston[["rm"]] <- ordered(cut(Boston[["rm"]], c(1,5,7,10)), labels = c('small-house','large-house','very-large-house'))
Boston[["age"]] <- ordered(cut(Boston[["age"]], c(0,20,50,100)), labels = c('few-old-houses','old-houses-in-moderation','lots-of-old-houses'))
Boston[["dis"]] <- ordered(cut(Boston[["dis"]], c(0,2,5,10,15)), labels = c('very-near','near','far','very-far'))
Boston[["rad"]] <- ordered(cut(Boston[["rad"]], c(0,5,10,25)), labels = c('easily-accessible','moderately-accessible','not-accessible'))
Boston[["tax"]] <- ordered(cut(Boston[["tax"]], c(0,300,500,1000)), labels = c('low-tax','moderate-tax','high-tax'))
Boston[["ptratio"]] <- ordered(cut(Boston[["ptratio"]], c(10,15,20,25)), labels = c('small-class','mediam-sized-class','large-class'))
Boston[["black"]] <- NULL
Boston[["medv"]] <- ordered(cut(Boston[["medv"]], c(1,20,35,60)), labels = c('cheap','moderate','expensive'))
Boston[["lstat"]] <- NULL
summary(Boston)

Boston_tr <- as(Boston,'transactions')
itemFrequencyPlot(Boston_tr,support = 0.03, cex.names = 0.8 )

rules <- apriori(Boston_tr, parameter = list(support = 0.001, confidence = 0.6))
summary(rules)

#Part C
rules_CloseToCity <- subset(rules, subset = rhs %in% "dis=very-near" & lift >1.2)
rules_LowCrime <- subset(rules, subset = rhs %in% "crim=very-low-crime" & lift >1.2)
summary(rules_CloseToCity)
summary(rules_LowCrime)

inspect(head(sort(rules_LowCrime)))
rulesLowCrimeNearCity <- subset(rules, subset = rhs %in% "crim=low-crime" & lhs %in% "dis=very-near" & lift >1.2)
summary(rulesLowCrimeNearCity)
inspect(head(sort(rulesLowCrimeNearCity, by ='lift'),n = 6))

#Part D
rulesLowPupil_TeacherRatio <- subset(rules, subset = rhs %in% "ptratio=small-class" & lift >1.2)

summary(rulesLowPupil_TeacherRatio)
inspect(head(sort(rulesLowPupil_TeacherRatio, by ='lift'),n = 6))

#Regression Model
subset = data.frame(lapply(subset, function(x) as.numeric(as.character(x))))
model = lm(rulesLowPupil_TeacherRatio, subset)
summary(model)

write(rulesLowCrime_NearCity, file='./LowCrime_NearCity.csv', sep=',',colnames = NA )