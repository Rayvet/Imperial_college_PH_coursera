getwd()
rm(list = ls())
g <- read.csv(file = "simulated-HF-mort-data-for-GMPH-_1K_-final-_2_.csv", header = TRUE, sep = ",")
dim(g)
head(g)
g[1:5,]

install.packages("survival")
library(survival)
library(ggplot2)
gender <- as.factor(g[, "gender"])
fu_time <- g[,"fu_time"]
death <- g[,"death"]

#Kaplan_Meier plot
km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)

summary(km_fit, times = c(1:7,30,60,90*(1:10)))

km_gender_fit <- survfit(Surv(fu_time, death) ~ gender)
plot(km_gender_fit)

survdiff(Surv(fu_time, death) ~ gender, rho = 0)

age_65plus <- ifelse(g[,"age"]>=65, 1,0)
table(age_65plus, exclude = NULL)
table(g$age,age_65plus, exclude = NULL)
survdiff(Surv(fu_time, death) ~ age_65plus, rho = 0)

#Cox
cox1 <- coxph(Surv(fu_time, death) ~ age, data = g)
summary(cox1)

install.packages("survminer")
library(survminer)
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = g)
summary(cox)
#Make ethnic group a  factor to make sense
ethnicgroup <- factor(g[,"ethnicgroup"])
fu_time <- g[, "fu_time"]
death <- g[, "death"]
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

table(gender, exclude = NULL)
summary(age)
age <- g[,"age"]

prior_dnas <- g[,"prior_dnas"]
table(prior_dnas, exclude = NULL)
copd <- as.factor(g[,"copd"])
table(copd, exclude = NULL)

t <- table(gender, exclude = NULL)
addmargins(t)
round(100*prop.table(t), digits = 1)

t <- table(copd, exclude = NULL)
addmargins(t)
round(100*prop.table(t), digits = 1)

t <- table(prior_dnas, exclude = NULL)
addmargins(t)
round(100*prop.table(t), digits = 1)
#Create extra ethnicgroup for those  missing  one
levels(ethnicgroup) <- c(levels(ethnicgroup), "8") # add level 8 to the factor
ethnicgroup[is.na(ethnicgroup)] <-"8" # Change NA to "None"

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + prior_dnas + ethnicgroup)
summary(cox)

#Non convergency
quintile <- as.factor(g[, "quintile"])
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox)
table(quintile, exclude = NULL)
t <- table(quintile, death)
t
round(100*prop.table(t,1), digits = 1)

#Handling non_convergency --Method 1
quintile <- relevel(quintile, ref = "1") # Make the reference category quintile = 1
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox)

#Handling non_convergency --Method 2
quintile_5groups <- g[, "quintile"]
quintile_5groups[quintile_5groups == 0] <-5 # This picks individuals with quintile = 0 and sets them to 5
quintile_5groups <- factor(quintile_5groups)
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup)
summary(cox)
table(quintile_5groups, exclude = NULL)

#Handling non_convergency --Method 3
quintile_5groups <- g[, "quintile"]
quintile_5groups[quintile_5groups == 0] <- NA
quintile_5groups <- factor(quintile_5groups)
table(quintile_5groups, exclude = NULL)
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup)
summary(cox)

#Handling non_convergency --Method 4 (Drop offending variable)
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + ethnicgroup)
summary(cox)

#Checking proportionality assumption
cox.zph(fit, transform = "km",global = TRUE)# Default
fit <- coxph(Surv(fu_time, death)~ gender)
temp <- cox.zph(fit)
print(temp)
plot(temp)

km_fit <- survfit(Surv(fu_time, death)~ gender)
require(ggplot2)
autoplot(km_fit)
plot(km_fit, xlab = "time",ylab = "Survival probability")

library(survminer)
res.cox <- coxph(Surv(fu_time, death) ~ age)
ggcoxdiagnostics(res.cox, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxfunctional(Surv(fu_time, death) ~ age + log(age) + sqrt(age), data = g)

#When proportionality assumption is not met
fit <- coxph(Surv(fu_time, death) ~ gender + tt(gender))
summary(fit)

#Model selection and backward elimination
ihd <-factor(g[, "ihd"])
valvular <-factor(g[, "valvular_disease"])
pvd <-factor(g[, "pvd"])
stroke <-factor(g[, "stroke"])
copd <-factor(g[, "copd"])
pneumonia <-factor(g[, "pneumonia"])
ht <-factor(g[, "hypertension"])
renal <-factor(g[, "renal_disease"])
ca <-factor(g[, "cancer"])
mets<-factor(g[, "metastatic_cancer"])
mental_health <- factor(g[,'mental_health'])
los <- g[,'los']
prior_dna <- g[, 'prior_dnas']
#for cognitive impairment
cog_imp <-as.factor(ifelse(g$dementia == 1 | g$senile ==1,  1,0))
cox <-coxph(Surv(fu_time, death) ~ age + gender + ethnicgroup + ihd + 
              valvular + pvd + stroke + copd + pneumonia + ht + renal + 
              ca + mets +mental_health + cog_imp + los + prior_dna)
summary(cox)
#Application of Backward elimination
cox <-coxph(Surv(fu_time, death) ~ age + gender + valvular + pneumonia + mets + cog_imp)
summary(cox)

table(cog_imp)
t <- table(cog_imp,death)
t
round(100*prop.table(t,1),digits = 1)

fit <-coxph(Surv(fu_time, death) ~ age + gender + valvular + pneumonia + mets + cog_imp)
temp <- cox.zph(fit)
print(temp)
