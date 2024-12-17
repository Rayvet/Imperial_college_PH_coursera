# Script for importing cancer data for coursera mooc
# By Buker Raymond on 19/09/24
g <- read.csv(file = 'cancer data for MOOC 1.csv', header = TRUE, sep= ",")
fruit <- g[, 'fruit']
veg <- g[, 'veg']
fruitveg <- fruit + veg
table(fruitveg)
g$fruitveg <- g$fruit + g$veg

hist(g$fruitveg, xlab = "Portions of fruit and vegetables",
main = "Daily consumption of fruit and vegetables combined", axes = F)

axis(side= 1, at = seq(0, 11,1))

axis(side= 2, at = seq(0, 16, 2))


g$healthy_bmi <- ifelse(g$bmi > 18.5 & g$bmi < 24.9, 'normal', 'not normal')

hist(g$fruit, xlab = "Portions of fruit",
     main = "Daily consumption of fruit", axes = F)

axis(side= 1, at = seq(0, 4,1))

axis(side= 2, at = seq(0, 24, 4))

hist(g$veg, xlab = "Portions of vegetables",
     main = "Daily consumption of vegetables", axes = F)

axis(side= 1, at = seq(0, 9,1))

axis(side= 2, at = seq(0, 18, 2))
install.packages("ggplot2")
require(ggplot2)

ggplot() + geom_histogram(data = g, aes(x = fruitveg))

ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10)

ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen")

ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen", col = "black") + 
labs(x = "Portions of fruit and vegetables", y = "Frequency")

ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen", col = "black") + 
labs(x = "Portions of fruit and vegetables", y = "Frequency") +
scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1))

ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen", col = "black") + 
  labs(x = "Portions of fruit and vegetables", y = "Frequency") +
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1)) + theme_bw()

ggplot() + geom_histogram(data = g, aes(x = fruit), bins = 5, fill = "darkgreen", col = "black") +
theme_bw() + labs(x = "Portions of fruit", y = "Frequency") + 
  scale_x_continuous(breaks = seq(from = 0, to = 4, by = 1))

ggplot() + geom_histogram(data = g, aes(x = veg), bins = 10, fill = "darkgreen", col = "black") +
  theme_bw() + labs(x = "Portions of vegetables", y = "Frequency") + 
  scale_x_continuous(breaks = seq(from = 0, to = 9, by = 1))

cancer <- g[,'cancer']
chisq.test(x = five_a_day, y = cancer)

bmi <- g[, 'bmi']
t.test(bmi~cancer)
t.test(bmi, mu = 25)
t.test(bmi~cancer, var.equal = TRUE)

overweight <- ifelse(bmi >= 25, 1,0)
table(overweight)

chisq.test(x = overweight, y = cancer)
