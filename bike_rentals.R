#' ---
#' title: Bike Rental Prediction
#' author: Noah Khan, Connor Kordes, Nathan Tompkins, Ysabella Salas
#' date: April 1, 2022
#' ---

# open the dataset containing the hourly bike rentals
rental.data <-  read.table("hour.csv", sep=",", header= TRUE)


# Clean Dataset -----------------------------------------------------------
str(rental.data)

# remove the dteday, casual, registered, workingday, yr, 
# instant, and mnth variables
rental.data <- subset(rental.data, select = -c(dteday, casual, 
                                               registered, workingday, yr,
                                               instant, mnth))

# convert variables to factors
rental.data$weekday <- as.factor(rental.data$weekday)
rental.data$season <- as.factor(rental.data$season)
levels(rental.data$season) <- c("winter", "spring", "summer", "fall")
rental.data$weathersit <- as.factor(rental.data$weathersit)
levels(rental.data$weathersit) = c("great", "good", "bad", "terrible")
rental.data$holiday <- as.factor(rental.data$holiday)
rental.data$hr <- as.factor(rental.data$hr)

# check for missing data
library(purrr)
map(rental.data, ~sum(is.na(.)))

# Exploratory Plots -------------------------------------------------------
library(ggplot2)
library(reshape2)

# Bike Rental Frequency
ggplot(data = rental.data) + geom_histogram(mapping = aes(x = cnt),
                             fill = rgb(44, 177, 190, maxColorValue = 255)) +
  labs(x = "Bike Rental Count", y = "Frequency", 
       title = "Bike Rental Frequency")


# mapping of predictors

# melt the dataframe
rental.melted.quant <- melt(subset(rental.data, select = -c(season, holiday, 
                                weekday, weathersit, hr)), id.vars = c("cnt"))
rental.melted.qual <- melt(subset(rental.data, select = c(season, holiday, 
                            weekday, weathersit, hr, cnt)), id.vars = c("cnt"))
colnames(rental.melted.quant) <- c("rental_count", "predictor", "value")
colnames(rental.melted.qual) <- c("rental_count", "predictor", "value")
rental.melted.quant$value <- as.numeric(rental.melted.quant$value)

# plot the figure
ggplot(data = rental.melted.quant) + 
  geom_jitter(aes(x = value, y = rental_count, color = predictor)) + 
  facet_wrap(~ predictor, scales =  "free_x", nrow = 2) +
  labs(x = "Predictor Value", y = "Bikes Rented Out", title = 
         "Mapping of Quantitative Predictors")

ggplot(data = rental.melted.qual) + 
  geom_boxplot(aes(x = value, y = rental_count, color = predictor)) + 
  facet_wrap(~ predictor, scales =  "free_x", nrow = 2) +
  labs(x = "Predictor Value", y = "Bikes Rented Out", title = 
         "Mapping of Qualitative Predictors")



# Initialize Full Model ----------------------------------------------------
model.full <- lm(data=rental.data, cnt~.)
summary(model.full)

# Multicolinearity Check
library(car)
vif(model.full)

# Indicator Variables -----------------------------------------------------

# consider splitting the hour variable into peak hours and non peak hours
library(dplyr)
# create subdata of the sum of rentals for each hour
hours <- c(0:23)
rentalCount.hour <- vector(mode = "numeric")

for (i in 1:24) {
  subdata <- filter(rental.data, hr == i - 1)
  rentalCount.hour[i] <- sum(subdata$cnt)
}
rm(subdata)
rm(i)

rental.hour <- data.frame(hours, rentalCount.hour)

#print the data
rental.hour

ggplot(data = rental.hour, mapping = aes(x=hours, y=rentalCount.hour)) +
  geom_line(color='#f7ac2d') + geom_vline(xintercept=7, color='#ed0f51') +
  geom_vline(xintercept=20, color='#ed0f51') + labs(x="Hour", y="Rental Count")


# initialize the dummy variables
peakHours = nonPeakHours=array(0, length(rental.data$cnt))

# consider peak hours for bike rentals is 12-19:00. We want to consider
# that has a constant uptick in rentals.
peakHours[as.numeric(rental.data$hr) >= 7& as.numeric(rental.data$hr) <= 20]<- 1
sum(is.na(peakHours))

model.hourTypes <- lm(rental.data$cnt ~ rental.data$season +
                        as.factor(peakHours) +
                        rental.data$holiday + rental.data$weekday + 
                        rental.data$weathersit + rental.data$temp +
                        rental.data$hum + 
                        rental.data$windspeed)
summary(model.hourTypes)
vif(model.hourTypes)

# Hypothesis Testing
model.noHours <- lm(rental.data$cnt ~ rental.data$season + rental.data$holiday + 
                      rental.data$weathersit + rental.data$temp +
                      rental.data$atemp + rental.data$hum + 
                      rental.data$windspeed)

anova(model.noHours, model.hourTypes)

model.full <- model.hourTypes

# Reduced Model -----------------------------------------------------------
model.reduced <- lm(rental.data$cnt ~ rental.data$season + 
                      as.factor(peakHours) + rental.data$holiday + 
                      rental.data$weekday + rental.data$weathersit + 
                      rental.data$atemp + rental.data$hum)
summary(model.reduced)



# Compare Models ----------------------------------------------------------
anova(model.reduced, model.full)

#' The p-value of the anova test is less than 0.05, so we conclude that the
#' original model is a better fit for our data.

# Residual Analysis -------------------------------------------------------
library(MASS)

main.model <- model.hourTypes

# Standardized Residual Values
residual.standardized <- stdres(main.model)

# Studentized Residual Values
residual.studentized <-  studres(main.model)

# R-Student Residual Values
residual.R_Student <- rstudent(main.model)

# plot the standardized residual
range(residual.standardized)

barplot(height = residual.standardized, names.arg = 1:nrow(rental.data),
        main = "Standardized Residuals", xlab = "Index",
        ylab = "Standardized Resid", ylim=c(-5,5), 
        col = rgb(237/255, 15/255, 81/255))
abline(h=3, col = rgb(247/255, 172/255, 45/255), lwd=2)
abline(h=-3, col = rgb(247/255, 172/255, 45/255), lwd=2)

# plot the studentized residual
range(residual.studentized)

barplot(height = residual.studentized, names.arg = 1:nrow(rental.data),
        main = "Studentized Residuals", xlab = "Index",
        ylab = "Studentized Resid", ylim=c(-5, 5))
abline(h=3, col = rgb(247/255, 172/255, 45/255), lwd=3)
abline(h=-3, col = rgb(247/255, 172/255, 45/255), lwd=3)

# plot the R-Student Residual
range(residual.R_Student)

cor.level <- 0.05/(2*nrow(rental.data))
cor.qt <- qt(cor.level, 17367, lower.tail=F)

barplot(height = residual.R_Student, names.arg = 1:nrow(rental.data),
        main = "R Student Residuals", xlab = "Index",
        ylab = "R Student Resid", ylim=c(-6,6))


abs(residual.R_Student) > cor.qt

abline(h = cor.qt , col = rgb(247/255, 172/255, 45/255), lwd=3)
abline(h = -cor.qt , col = rgb(247/255, 172/255, 45/255), lwd=3)

# Normality Plot
par(mfrow=c(1,2))
hist(residual.studentized, breaks=10, freq=F, col=rgb(247/255, 172/255, 45/255),
     cex.axis=1.5, cex.lab=1.5, cex.main=2, main = "Distribution")
qqPlot(main.model, col=rgb(247/255, 172/255, 45/255))

# Residual Plot
par(mfrow=c(1,1))
residualPlot(main.model, type="rstudent", quadratic=F, 
             col = rgb(247/255, 172/255, 45/255),
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)


temp_model <- lm(rental.data$cnt ~ rental.data$season + as.factor(peakHours) +
                   rental.data$holiday + rental.data$weekday + 
                   rental.data$weathersit + rental.data$temp+ 
                   rental.data$hum + rental.data$windspeed)

wt <- 
  1 / lm(abs(main.model$residuals) ~ main.model$fitted.values)$fitted.values^2

#perform weighted least squares regression
wls_model <- lm(rental.data$cnt ~ rental.data$season + as.factor(peakHours) +
                  rental.data$holiday + rental.data$weekday + 
                  rental.data$weathersit + rental.data$temp+ 
                  rental.data$hum + rental.data$windspeed, weights=wt)

#view summary of model
summary(wls_model)

vif(wls_model)

temp_model <- lm(rental.data$cnt ~ rental.data$season + as.factor(peakHours) +
                   rental.data$weekday + 
                   rental.data$weathersit + rental.data$temp+ 
                   rental.data$hum)

wt <- 
  1 / lm(abs(temp_model$residuals) ~ temp_model$fitted.values)$fitted.values^2

# reduce the model
wls_model.reduced <- lm(rental.data$cnt ~ rental.data$season + 
                          as.factor(peakHours) +
                  rental.data$weekday + 
                  rental.data$weathersit + rental.data$temp+ 
                  rental.data$hum, weights=wt)

summary(wls_model.reduced)

vif(wls_model.reduced)

# Residual Plot
residualPlot(wls_model, type="rstudent", quadratic=F, 
             col = rgb(247/255, 172/255, 45/255),
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)


residualPlot(wls_model.reduced, type="rstudent", quadratic=F, 
             col = rgb(247/255, 172/255, 45/255),
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

par(mfrow=c(1,2))
hist(studres(wls_model.reduced), breaks=10, freq=F, col=rgb(247/255, 172/255, 45/255),
     cex.axis=1.5, cex.lab=1.5, cex.main=2, main = "Distribution")
qqPlot(wls_model.reduced, col=rgb(247/255, 172/255, 45/255))

# Influence Analysis ------------------------------------------------------
data.influence <- influence.measures(main.model)
summary(data.influence)

data.influence <- influence.measures(wls_model)
summary(data.influence)

data.influence <- influence.measures(wls_model.reduced)
summary(data.influence)


# Transformations ---------------------------------------------------------

boxcox.utility <- boxCox(main.model,
                         lambda=seq(-2,2,1/10))
boxcox.utility <- boxCox(main.model,
                         lambda=seq(-70,70,1/10))

boxcox.utility$x[which.max(boxcox.utility$y)]

model.transform <- lm( rental.data$cnt^(.3) ~ rental.data$season + 
                         as.factor(peakHours) +
                        rental.data$holiday + rental.data$weekday + 
                        rental.data$weathersit + rental.data$temp +
                        rental.data$hum + rental.data$windspeed )
summary(model.transform)


residualPlot(model.transform, type="rstudent", quadratic=F, 
             col = rgb(247/255, 172/255, 45/255), pch=16, cex=1.5, 
             cex.axis=1.5, cex.lab=1.5, ylim=c(-5, 5))


# plot the graphs
dfbetaPlots(wls_model.reduced, intercept = T, col=rgb(247/255, 172/255, 45/255))
influenceIndexPlot(wls_model.reduced, vars = c("Cook", "Hat"),
                   col=rgb(247/255, 172/255, 45/255))

temp_model <- lm( rental.data$cnt ~ rental.data$season + as.factor(peakHours) +
                    rental.data$holiday + rental.data$weekday + 
                    rental.data$weathersit + rental.data$temp +
                    rental.data$atemp + rental.data$hum + 
                    rental.data$windspeed + 
                    rental.data$season*rental.data$weekday)
summary(temp_model)


# Residual Plot
residualPlot(temp_model, type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)


wt <- 1 / lm(abs(temp_model$residuals) ~ temp_model$fitted.values)$fitted.values^2

model_with_interaction_and_weights <- lm(rental.data$cnt ~ rental.data$season + 
                                           as.factor(peakHours) +
                    rental.data$holiday + rental.data$weekday + 
                    rental.data$weathersit + rental.data$temp +
                    rental.data$atemp + rental.data$hum + 
                    rental.data$windspeed + 
                      rental.data$season*rental.data$weekday, weights = wt)
summary(model_with_interaction_and_weights)

vif(model_with_interaction_and_weights)

residualPlot(model_with_interaction_and_weights, type="rstudent", 
             quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)
