rm(list = ls())
g <- read.csv(file = "Diabetes.csv", header = TRUE, sep =",")
dim(g)
colnames(g)
dimnames(g)[[2]]

chol <- g["chol"]
chol
gender <- as.factor(g[,"gender"])
dm <- as.factor(g[, "dm"])
t <-table(gender) #store table for future manipulation
addmargins(t) #This will sum up the gender totals to give an overall total and print the results

round(prop.table(t), digits = 3) #get proportions
round(100*prop.table(t), digits = 1)

dm2 <-factor(dm, exclude = NULL)
table(dm2)

summary(chol)

height <- g[,'height']
weight <- g[,'weight']
summary(weight)
summary(height)

height.si <- height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2
summary(bmi)

bmi_categorised <- ifelse(bmi<18.5, "underweight",
                          ifelse(bmi >= 18.5 & bmi <= 25,"normal",
                                 ifelse(bmi > 25 & bmi <= 30, "overweight",
                                        ifelse(bmi >30, "obese", NA)))) 
table(bmi_categorised, exclude = NULL)

dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL)
dm_by_bmi_category
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1)
round(100 * prop.table(dm_by_bmi_category, margin = 2), digits = 1) #just for just

age <- g[, "age"]
age_categorised <-ifelse(age<45, "under 45",
                         ifelse(age >=45 & age <= 64,"45-64",
                                ifelse(age >= 65 & age <= 74, "65-74",
                                       ifelse(age >=75, "75 or over", NA)))) 

table(age_categorised, exclude = NULL)

age_categorised_by_gender <- table(age_categorised, gender, exclude = NULL)
round(100 * prop.table(age_categorised_by_gender, margin = 2), digits = 1)
round(100 * prop.table(age_categorised_by_gender), digits = 1)

head(cbind(age_categorised, age))

#logistic regression
m <- glm(dm ~1, family = binomial(link = logit))
summary(m)
table(m$y)

#Check gender
m <- glm(dm ~gender, family = binomial(link = logit))
summary(m)
exp(0.08694)

#check continuous age
m <- glm(dm ~age, family = binomial(link = logit))
summary(m)

#cross tabulation of age and diabetes status
dm_by_age <- table(age, dm)

#output frequencies of diabetes status by age
freq_table <- prop.table(dm_by_age, margin = 1)

#Calculate the odds of having diabetes
odds <- freq_table[, "yes"]/freq_table[,"no"]

#calculate the log odds
logodds <- log(odds)

#plot the ages found in the sample against the log odds of having diabetes
plot(rownames(freq_table), logodds)

contrasts(gender)
levels(gender)
gender <-relevel(gender, ref = "male")
levels(gender)
gender <-relevel(gender, ref = "female")
levels(gender)

#view model's coefficients
m$coefficients

#convert the log odds to odds
exp(m$coefficients)

d <- density(age)
plot(d, main= "")

#Multiple regression analysis
m <- glm(dm ~ age+ gender + bmi, family = binomial(link = logit))
summary(m)
exp(confint(m))

insurance <- as.factor(g[, "insurance"])
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit))
full_model <- glm(dm ~ age + insurance, family = binomial(link = logit))
summary(full_model)

null_model <-glm(dm~1, family = binomial(link = logit))
summary(null_model)

#Calculate McFadden's R-square
R2 <- 1-logLik(full_model)/logLik(null_model)
R2

# C- statistic
install.packages("DescTools")
require(DescTools)
full_model <- glm(dm ~ age + insurance, family = binomial(link = logit))
summary(full_model)
Cstat(full_model)

#Hosmer-Lemeshow statistic and test
install.packages("ResourceSelection")
require(ResourceSelection)
full_model$y

#run Hosmer-Lemeshow test
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10)
HL
plot(HL$observed[, "y1"], HL$expected[,"yhat1"])
plot(HL$observed[, "y0"], HL$expected[,"yhat0"])
plot(x = HL$observed[, "y1"]/(HL$observed[, "y1"]+ HL$observed[, "y0"]),
     y = HL$expected[,"yhat1"]/( HL$expected[,"yhat1"]+  HL$expected[,"yhat0"]))

install.packages("generalhoslem")
require(generalhoslem)
logitgof(obs = full_model$y, exp = fitted(full_model), g=10)

anova(full_model, test = "Chisq")

# Backwards Elimination in model design
dm <- as.factor(g[, "dm"])
insurance <- as.factor(g[, "insurance"])
fh <- as.factor(g[, "fh"])
smoking <- as.factor(g[, "smoking"])
chol <- g[,'chol']
hdl <- g[,'hdl']
ratio <- g[,'ratio']
location <- as.factor(g[, "location"])
frame <- as.factor(g[,"frame"])
systolic <- g[, "bp.1s"]
diastolic <- g[, "bp.1d"]
model <- glm(dm~ age + bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit))
summary(model)
anova(model, test = "Chisq")

#drop the blood pressures
model1 <- glm(dm~ age + bmi + chol + hdl, family = binomial(link = logit))
summary(model1)

#test why blood pressure doesn't correlate
cor.test(systolic, hdl)
cor.test(systolic, bmi)
cor.test(systolic, chol)
cor.test(systolic, age)

model2 <- glm(dm~bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit))
summary(model2)

model3 <- glm(dm~age + bmi + chol + hdl + systolic + diastolic+ gender + location + frame+ insurance + smoking, family = binomial(link = logit))
summary(model3)