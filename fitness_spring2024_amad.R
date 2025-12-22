##  Final R Project Spring 2024
##  Tajrian Amad
##  5/6/2024
##  Version 4.4.0

# Load the following libraries into your R session: haven (for importing SAS datasets),
# tidyverse (which includes dplyr and ggplot2).
library(haven)
library(dplyr)
library(ggplot2)


# List working directory, objects in this session, and files in working directory.

# Get working directory
getwd()

# Get objects
a <- 5
a1 <- a * 10

# List the objects in our environment
ls()

rm(a, a1)

# List the files in our working directory
dir()


#Read in the sas dataset: fitness.sas7bdat.
fitness <- read_sas("fitness (1).sas7bdat")

# We want to look at the "structure" of our data
str(fitness) # Base R
fitness$Name # object$variable

# Look at the data using dplyr
glimpse(fitness)

# Look at the data in a spreadsheet
View(fitness)

# Confirm what type of object is fitness
is.data.frame(fitness)
typeof(fitness)
class(fitness)

# View histogram of oxygen consumption
fit_hist <- ggplot(data = fitness, aes(x = Oxygen_Consumption))
fit_hist
fit_hist + geom_histogram()
fit_hist + geom_histogram(binwidth = 3)

# Look at oxygen consumption by sex
fit_box <- ggplot(data = fitness, aes(x = Gender, y = Oxygen_Consumption))
fit_box
fit_box + geom_boxplot()

# Simple analyses ------------------------------------------------------------------------

# Calculate t-test of oxygen consumption by sex
fit_test <- t.test(fitness$Oxygen_Consumption ~ fitness$Gender, var.equal = TRUE)
fit_test

# Look at correlations in fitness
cor(fitness)

str(fitness)

# Select just numeric variables using dplyr
fit_corr <- fitness %>% select_if(is.numeric) # select only those variables that are numeric
str(fit_corr)

# Look at correlations in only those numeric variables in fitness
cor(fit_corr)

# Use the round function to clean up the output a little
round(cor(fit_corr), 3) # round to 3 decimal places

# Scatterplot matric 
pairs(fit_corr)


# Best fit lines ------------------------------------------------------------------

# Graph the linear model 
sreg_graph <- ggplot(data = fitness, aes(x = RunTime, y = Oxygen_Consumption))

# Linear Best Fit 
sreg_graph + geom_point() + geom_smooth(method = "lm") # lm = linear model

# Loess smoother 
sreg_graph + geom_point() + geom_smooth()

# Can discuss bias-variance tradeoff and overfitting/generalizability

sreg_graph + geom_point() + geom_smooth(span = 0.8) # span argument controls wiggliness


# Simple Linear Regression -------------------------------------------------------

# Linear Model
lm1 <- lm(Oxygen_Consumption ~ RunTime, data = fitness)
lm1$coefficients

lm1
summary(lm1)

# Plot regression diagnostics
plot(lm1)


# Multiple Linear Regression ------------------------------------------------------

# Multiple regression model
lm2 <- lm(Oxygen_Consumption ~ RunTime + Performance, data = fitness) 

summary(lm2)
