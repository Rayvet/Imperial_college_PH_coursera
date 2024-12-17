# linear regression in R
getwd()
rm(list = ls())
setwd()

COPD <- read.csv("COPD_student_dataset.csv")

View(COPD)

colnames(COPD)

hist(COPD$MWT1Best, main = "Histogram of MWT1Best", xlab ="MWT1Best", breaks = 12)


subset(COPD, MWT1Best>650)

hist(COPD$FEV1, main = "Histogram of FEV1", xlab = "FEV1")

list("Summary" = summary(COPD$MWT1Best), 
     "Mean" = mean(COPD$MWT1Best, na.rm = TRUE),
     "Standard Deviation" = sd(COPD$MWT1Best, na.rm = TRUE),
     "Range" = range(COPD$MWT1Best, na.rm = TRUE),
     "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm = TRUE))


list("Summary" = summary(COPD$FEV1), 
     "Mean" = mean(COPD$FEV1, na.rm = TRUE),
     "Standard Deviation" = sd(COPD$FEV1, na.rm = TRUE),
     "Range" = range(COPD$FEV1, na.rm = TRUE),
     "Inter-Quartile Range" = IQR(COPD$FEV1, na.rm = TRUE))

plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best")

cor.test(COPD$FEV1,COPD$MWT1Best, use = "complete.obs", method = "pearson")
cor.test(COPD$FEV1,COPD$MWT1Best, use = "complete.obs", method = "spearman")

hist(COPD$AGE, main = "Histogram of age", xlab = "AGE")
summary(COPD$AGE)

plot(COPD$AGE, COPD$MWT1Best, xlab = "AGE", ylab = "MWT1Best")

cor.test(COPD$AGE,COPD$MWT1Best, use = "complete.obs", method = "pearson")
cor.test(COPD$AGE,COPD$MWT1Best, use = "complete.obs", method = "spearman")


MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)

summary(MWT1Best_FEV1)
confint(MWT1Best_FEV1)
par(mfrow= c(2,2))
plot(MWT1Best_FEV1)

par(mfrow= c(1,1))


MWT1Best_AGE <- lm(MWT1Best~AGE, data = COPD)
summary(MWT1Best_AGE)
confint(MWT1Best_AGE)
plot(MWT1Best_AGE)

predictedVals <- predict(MWT1Best_AGE) # Get predicted values for the model
residualVals <- residuals(MWT1Best_AGE) # Get residuals between model and data
par(mfrow= c(1,1)) # Set plotting format
plot(MWT1Best_AGE) # See residual plots

hist(residualVals, main = "Histogram of residuals", xlab = "Residuals")

MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+ AGE, data = COPD)
summary(MWT1Best_FEV1_AGE)
confint(MWT1Best_FEV1_AGE)


MWT1Best_FEV1_AGE_smoking <- lm(MWT1Best~FEV1+ AGE + smoking, data = COPD)
summary(MWT1Best_FEV1_AGE_smoking)
confint(MWT1Best_FEV1_AGE_smoking)

MWT1Best_FVC <- lm(MWT1Best~FVC, data = COPD)
summary(MWT1Best_FVC)
confint(MWT1Best_FVC)


MWT1Best_FVC_AGE <- lm(MWT1Best~FVC+ AGE, data = COPD)
summary(MWT1Best_FVC_AGE)
confint(MWT1Best_FVC_AGE)


dim(COPD)

head(COPD)

class(COPD$AGE)
summary(COPD$AGE)
hist(COPD$AGE)

class(COPD$CAT)
summary(COPD$CAT)
hist(COPD$CAT)

class(COPD$gender)
COPD$gender <- as.factor(COPD$gender)
class(COPD$gender)
table(COPD$gender, exclude = NULL)

class(COPD$MWT1Best)
class(COPD$copd)
COPD$copd <- factor(COPD$copd)

str(COPD$copd)

lr1 <- lm(MWT1Best~copd, data = COPD)
summary(lr1)
confint(lr1)

COPD$copd <- relevel(COPD$copd, ref = 3)
lr1 <- lm(MWT1Best~copd, data = COPD)
summary(lr1)
confint(lr1)

comorbid <-length(COPD$Diabetes)
comorbid[COPD$Diabetes == 1 | COPD$muscular == 1 | COPD$hypertension == 1 | COPD$ AtrialFib == 1| COPD$IHD == 1] <- 1
comorbid[is.na(comorbid)] <- 0
comorbid <- factor(comorbid)
print(comorbid)
str(comorbid)
comorbid[15]
COPD$comorbid <- comorbid

install.packages("Hmisc")
describe(COPD)

install.packages("gmodels")
require(gmodels)
CrossTable(COPD$copd)
sum(is.na(COPD$copd))

summary(COPD$MWT1Best)

hist(COPD$AGE)

my_data <- COPD[,c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")]
cor_matrix <- cor(my_data)
cor_matrix
round(cor_matrix, 2)

pairs(~AGE + PackHistory + FEV1 + FEV1PRED + FVC + CAT + HAD + SGRQ, data = COPD )

CrossTable(COPD$hypertension, COPD$IHD)

lr1 <- lm(MWT1Best ~ gender, data = COPD)
summary(lr1)
confint(lr1)


library(Hmisc)
describe(COPD)
class(COPD$gender)
COPD$gender <- factor(COPD$gender)
describe(COPD$gender)
CrossTable(COPD$gender)

hist(COPD$CAT)
COPD$CAT[COPD$CAT>40] <- NA
describe(COPD$CAT)

CrossTable(COPD$gender, COPD$IHD)


mlr1 <- lm(MWT1Best~ FEV1 + AGE + factor(gender) + factor(COPDSEVERITY) +  factor(comorbid), data = COPD)
summary(mlr1)
confint(mlr1)

install.packages("mctest")
library(mctest)

imcdiag(mlr1, method = 'VIF', all= TRUE)

#Interaction between predictors
COPD$Diabetes <- as.integer(COPD$Diabetes)
COPD$AtrialFib <- as.integer(COPD$AtrialFib)

DAF <- COPD$Diabetes * COPD$AtrialFib
r1 <- lm(MWT1Best~factor(Diabetes) + factor(AtrialFib) + factor(DAF), data =  COPD)
r2 <- lm(MWT1Best~factor(Diabetes) + factor(AtrialFib) + factor(Diabetes * AtrialFib), data =  COPD)
summary(r1)
confint(r1)

install.packages("prediction")
summary(r2)
confint(r2)
list("Diabetes"= prediction(r2, at = list(Diabetes=c(0, 1))),"AtrialFIb"= prediction(r2, at = list(AtrialFib=c(0, 1))),
"Diabetes*AtrialFib"= prediction(r2, at = list(Diabetes=c(0, 1), AtrialFib = c(0,1))))

