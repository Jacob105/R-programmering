# Installera paket
install.packages("readxl")
install.packages("leaps")
install.packages("ISLR2")
library(leaps)
library(ISLR2)
library(readxl)

# Ladda in data
data <- read_excel("C:/Users/jacob/Documents/R Programmering/bilar2.xlsx")
head(data)

#data$Bränsle_bensin <- ifelse(data$Fuel == 1, 1, 0)
#data$Bränsle_diesel <- ifelse(data$Fuel == 2, 1, 0)

# log(Y)
data$logPrice = log(data$Price)

regcars <- regsubsets(logPrice~.-Price, data = data)
reg.summary <- summary(regcars)
reg.summary
names(reg.summary)

sum(is.na(regcars))

any(!is.finite(regcars$bic))
any(is.na(regcars$bic))
summary(regcars$bic)
head(regcars$bic)
str(regcars)


# Removing an outlier
data <- data[-22, ]

lm_2 <- lm(logPrice~.-Price, data = data)

summary(lm_2)
par(mfrow = c(1, 1))
plot(lm_2)
dim(data)



par(mfrow = c(1, 1))

#Plot RSS values against the number of variables
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")


par(mfrow = c(1, 1))
plot(regcars, scale = "r2")

plot(regcars, scale = "bic")

plot(regcars, scale = "adjr2")

plot(regcars, scale = "Cp")

# coef extracts for BIC, where can I see this in documentation???
coef(regcars, 5)


# Forward and Backward Stepwise Selection ---------------------------------

regcars_fwd <- regsubsets(logPrice ~ .-Price, data = data, nvmax = 5, method = "forward")
summary(regcars_fwd)

regcars_bwd <- regsubsets(logPrice ~ .-Price, data = data, nvmax = 5, method = "backward")
summary(regcars_bwd)

coef(regcars, 5)
coef(regcars_fwd, 5)
coef(regcars_bwd, 5)



# Fit a linear regression model
model <- lm(logPrice ~.-Price, data = data)

# Summarize the model
summary(model)

# Predict prices using the linear regression model
data$predicted_log <- predict(model)

# Convert predicted log prices back to regular prices
data$predicted <- exp(data$predicted_log)

# Plot observed vs. predicted prices
plot(data$Price, data$predicted, main = "Observed vs. Predicted Prices", 
     xlab = "Observed Prices", ylab = "Predicted Prices", col = "blue", pch = 16)

# Add a diagonal line representing perfect prediction
abline(0, 1, col = "red")

data$PercentageOfTheRealPrice = data$predicted/data$Price

# Looking at the data, you can see all the predicted prices for every car
View(data)
