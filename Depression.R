
# Correlation between depression and socio-economical and environmental factors
# Alisia Sara Baielli and Diego Cerretti


# install.packages('olsrr')
# install.packages('ggplot2')
# install.packages('tidyverse')
# install.packages('Rcmdr')
# install.packages('rgl')
# install.packages('asympTest')
# install.packages("plot3D")
library(Rcmdr)
library(olsrr)
library(MASS)
library(ggplot2)
library(tidyverse)
library(rgl)
library(asympTest)
library(plot3D)


######## GLOBAL ########


# Depression Heat Map


# Loading data from global dataset

Globaldata <- read.table(file.choose(), header = T, sep=";")

mapdata <- map_data("world")
mapdata <- left_join(mapdata, Globaldata, by = "region")

mapdata1 <- mapdata %>% filter(!is.na(mapdata$Depression_rate2022))

map1 <- ggplot(mapdata1, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill = Depression_rate2022), color = "black")

map2 <- map1 + scale_fill_gradient(name = "Depression Rate 2022(%)",
                                   low = "yellow", high = "red", 
                                   na.value = "grey50") + 
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
map2

 
# Loading data from global dataset

data1 <- read.table(file.choose(), header=T, sep=";")

# Multivariate Linear Regression

attach(data1)
fit1 <- lm(Depression_rate2022 ~ GINI + GPI + Avg_temp + Education_idx + GDP_pc 
           + Urbanisation + Internet_usage, data = data1)
summary(fit1)

# Error rate

Error_rate <-sigma(fit1)/mean(Depression_rate2022)
Error_rate

#3D plot depression-education

scatter3d(Depression_rate2022,Education_idx, Internet_usage)

# Model selection with Step-up method

stepup_1 = ols_step_forward_p(fit1, penter=0.05)
stepup_data_1 = subset(data1, select = c(stepup_1$predictors))
stepup_model_1 = lm(Depression_rate2022 ~ ., data = stepup_data_1)
summary(stepup_model_1)


# Internet Usage vs Depression

plot(Internet_usage, Depression_rate2022, xlab= "Internet usage", ylab= "Depression Rate", main = "Global Depression Rate vs. Internet Usage")
abline(lm(Depression_rate2022 ~ Internet_usage))

# Education level vs Depression

plot(Education_idx, Depression_rate2022, xlab= "Education level", ylab= "Depression Rate", main = "Global Depression Rate vs. Education Index")
abline(lm(Depression_rate2022 ~ Education_idx))


# Model selection with Step-down method

summary(fit1)
# Urbanisation has the highest p-value: ignore

model1_1 <- lm(Depression_rate2022 ~ GINI + GPI + Avg_temp + Education_idx +
               GDP_pc + Internet_usage, data=data1)
summary(model1_1)
# GINI has the highest p-value: ignore

model1_2 <- lm(Depression_rate2022 ~ GPI + Avg_temp + Education_idx + GDP_pc +
                  Internet_usage, data=data1)
summary(model1_2)
# Avg_temp has the highest p-value: ignore

model1_3 <- lm(Depression_rate2022 ~ GPI + Education_idx + GDP_pc +
                 Internet_usage, data=data1)
summary(model1_3)
# GDP_pc has the highest p-value: ignore

model1_4 <-lm(Depression_rate2022 ~ GPI + Education_idx + Internet_usage, 
              data=data1)
summary(model1_4)
#GPI has the highest p-value: ignore

model1_5 <- lm(Depression_rate2022 ~ Education_idx + Internet_usage, data=data1)
summary(model1_5)
# All p-values are below 0.05: we keep this model

stepdown_model_1 = model1_5

# Model comparison using AIC

AIC(stepup_model_1, stepdown_model_1, fit1)
# stepup_model_1 and stepdown_model_1 are equivalent
final_model_1 = stepup_model_1

# Graph Depression rate - education index, internet usage
scatter3D(Depression_rate2022, Internet_usage, Education_idx,
          xlab = "Internet usage",
          ylab = "Education",
          zlab = "Global Depression Rate",
          phi = 0, bty = "g", pch = 20, cex = 2,
          main = "Global Depression vs. Internet usage and Education")

# Check constant variance

plot(fitted(final_model_1), residuals(final_model_1), xlab='Fitted values',
     ylab='Residuals', main = "Residuals' variance")
abline(h=0)

# Check normality

hist(residuals(final_model_1), prob=TRUE, xlab="Residuals", main="Residuals distribution")
curve(dnorm(x, mean=0, sd(residuals(final_model_1))), add=TRUE, col="red")
qqnorm(residuals(final_model_1), ylab="Residuals", main="Residuals Q-Q Plot")
qqline(residuals(final_model_1), col="red")

# Test normality using Shapiro-Wilk

shapiro.test(residuals(final_model_1))

# Check residuals' mean

mean(residuals(final_model_1))

# Identify outliers qqplot

QQ_y=qqnorm(residuals(final_model_1))
identify(QQ_y)

detach(data1)


######## OECD ########


# Loading data from OECD dataset

data2 <- read.table(file.choose(), header = T, sep=";")


# Multivariate Linear Regression

attach(data2)
fit2 <- lm(Depression_rate2022 ~ GINI + GPI + Avg_temp + Education_idx + GDP_pc 
           + Urbanisation + Internet_usage + Avg_hrs_worked_yearly, data = data2)
summary(fit2)


# Internet Usage vs Depression

plot(Internet_usage,Depression_rate2022, xlab = "Internet usage", ylab = "Depression rate", main = "OECD Depression rate vs. Internet Usage")
abline(lm(Depression_rate2022 ~ Internet_usage))


# Education level vs Depression

plot(Education_idx, Depression_rate2022, xlab= "% below upper secondary education", ylab= "Depression Rate", main = "OECD Depression Rate vs. % below upper secondary education")
abline(lm(Depression_rate2022 ~ Education_idx))


# Error rate

sigma(fit2)/mean(Depression_rate2022)


# Model selection with Step-up method

stepup_2 = ols_step_forward_p(fit2, penter=0.05)
stepup_data_2 = subset(data2, select = c(stepup_2$predictors))
stepup_model_2 = lm(Depression_rate2022 ~ ., data = stepup_data_2)
summary(stepup_model_2)


# Model selection with Step-down method

summary(fit2) 
# Urbanisation has the highest p-value: ignore

model2_1 <- lm(Depression_rate2022 ~ GINI + GPI + Avg_temp + Education_idx +
               GDP_pc + Avg_hrs_worked_yearly + Internet_usage, data = data2)
summary(model2_1) 
# GDP has the highest p-value: ignore

model2_2 <- lm(Depression_rate2022 ~ GINI + GPI + Avg_temp + Education_idx +
               Avg_hrs_worked_yearly + Internet_usage, data = data2)
summary(model2_2) 
# Avg_hours_worked_yearly has the highest p-value: ignore

model2_3 <- lm(Depression_rate2022 ~ GINI + GPI + Avg_temp + Education_idx +
               Internet_usage, data = data2)
summary(model2_3)
# GINI has the highest p-value: ignore

model2_4 <- lm(Depression_rate2022 ~ GPI + Avg_temp + Education_idx +
               Internet_usage, data = data2)
summary(model2_4)
# Avg_temp has the highest p-value: ignore

model2_5 <- lm(Depression_rate2022 ~ GPI + Education_idx + Internet_usage,
             data = data2)
summary(model2_5)
# GPI has the highest p-value: ignore

model2_6 <- lm(Depression_rate2022 ~ Education_idx + Internet_usage,
             data = data2)
summary(model2_6)
# All p-values are below 0.05: we keep this model

stepdown_model_2 = model2_6


# Model comparison using AIC

AIC(stepup_model_2, stepdown_model_2, fit2)
# stepup_model_2 and stepdwon_model_2 are equivalent
final_model_2 = stepup_model_2
final_data_2 = stepup_data_2

scatter3D(Depression_rate2022, Internet_usage, Education_idx,
          xlab = "Internet usage",
          ylab = "% below upper secondary education",
          zlab = "OECD Depression Rate",
          phi = 0, bty = "g", pch = 20, cex = 2,
          main = "OECD Depression vs. Internet usage and 
  % below upper secondary education")

# Check constant variance

plot(fitted(final_model_2), residuals(final_model_2), xlab='Fitted values',
     ylab='Residuals Variance')
abline(h=0)

# Can we get a better plot? Square root, log transformations

squared_model_2 <- lm(sqrt(Depression_rate2022) ~ ., data = final_data_2)
plot(fitted(squared_model_2), residuals(squared_model_2), xlab='Fitted values',
     ylab='Residuals')
abline(h=0)

log_model_2 <- lm(log(Depression_rate2022) ~ ., data = final_data_2)
plot(fitted(log_model_2), residuals(log_model_2), xlab='Fitted values',
     ylab='Residuals', main = "Residuals' Variance")
abline(h=0)


# select log_model as our preferred

new_model_2 = log_model_2


# Check normality

hist(residuals(new_model_2), prob=TRUE, xlab="Residuals", main = "Residuals Distribution")
curve(dnorm(x, mean=0, sd(residuals(new_model_2))), add=TRUE, col="red")
qqnorm(residuals(new_model_2), ylab="Residuals", main = "Residuals Q-Q Plot")
qqline(residuals(new_model_2), col="red")


# Test normality using Shapiro-Wilk

shapiro.test(residuals(new_model_2))

detach(data2)



######## ARE OECD POPULATIONS SIGNIFICANTLY MORE DEPRESSED THAN NON-OECD? #########


attach(data2)
Depression_OECD = Depression_rate2022
detach(data2)

# Loading Non-OECD countries data

data3 <- read.table(file.choose(), header = T, sep=";")
attach(data3)
Depression_nonOECD= Depression_rate2022
detach(data3)

# Test homoscedasticity : are variances of OECD and non-OECD similar?
var.test(Depression_nonOECD, Depression_OECD)
# p>0.05: we can assume homogeneous variances
boxplot(Depression_nonOECD, Depression_OECD, ylab="Depression rate",
        xlab = "Non-OECD   vs   OECD")

# Test normality of Depression in both samples

# OECD countries
hist(Depression_OECD, prob=TRUE, xlab="Depression in OECD countries", main = "OECD Depression Distribution")
curve(dnorm(x, mean=mean(Depression_OECD), sd(Depression_OECD)), add=TRUE, col="red")
qqnorm(Depression_OECD, ylab="Depression rate in OECD countries 2022", main = "OECD Depression Rate Q-Q Plot")
qqline(Depression_OECD, col="red")
# Visually normal

# Non-OECD countries
hist(Depression_nonOECD, prob=TRUE, xlab="Depression in non-OECD countries", main = "Non-OECD Depression Distribution")
curve(dnorm(x, mean=mean(Depression_nonOECD), sd(Depression_nonOECD)), add=TRUE, col="red")
qqnorm(Depression_nonOECD, ylab="Depression rate in non-OECD countries 2022", main = "Non-OECD Depression Rate Q-Q Plot")
qqline(Depression_nonOECD, col="red")
# Visually normal

# Kolmogorov-Smirnov test
ks.test(Depression_OECD, "pnorm", mean(Depression_OECD), sd(Depression_OECD))
ks.test(Depression_nonOECD, "pnorm", mean(Depression_nonOECD), sd(Depression_nonOECD))
# We have no significant evidence against normality

# Shapiro-Wilk test
shapiro.test(Depression_OECD)
shapiro.test(Depression_nonOECD)
# We have no significant evidence against normality
# We can interpret the distributions of depression rate as normal distributions

# Student's two-sample t-test:
# H0: mean depression rate in OECD countries <= mean depression rate in non_OECD countries
# one-sided test
# assume equal variances

t.test(Depression_OECD, Depression_nonOECD, alt="greater", conf=0.95, 
       var.equal=T, paired=F)
# We reject the null hypothesis: Depression rate in OECD countries is significantly larger

# Compare Densities
plot(density(Depression_OECD), col="red", xlab = "Depression Rate", ylab="Density", main="Depression Rate Density Graph OECD vs non-OECD")
lines(density(Depression_nonOECD), col="blue", add=T)


