###Installing required packages to run the commands.
install.packages("purrr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("car")

###Setting the directory
getwd()
setwd("C:/Users/Arun/Desktop/BA WITH R/Homework 1")


###Opening the mortality.csv file
library(readr)
mydata <- read.csv("mortality.csv", header=TRUE)
head(mydata)
#View(mydata)

###Removing the unwanted cityID variable
mydata$City <- NULL
head(mydata)


~~~~~~Data Analysis before building the model~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      Univariate anaysis (Histograms)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("purrr") #DataCleaning
install.packages("tidyr") #DataCleaning 
install.packages("ggplot2") ##Visualisations
install.packages("car") #Calculating VIF
library(purrr)
library(tidyr)
library(ggplot2)
mydata %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value))+facet_wrap(~ key, scales = "free") +geom_histogram(bins = 10)

###The three air pollution variables (HCPot, NOxPot and S02Pot)look very asymmetric and skewed to the right. 
###This suggests that variable transformation is needed in order to satisfy the standard assumption about the errors made in regression analysis.

##Doing log transformations for variables HCPot, NOxPot and S02Pot
Final_data <- mydata
Final_data$HCPot <- log(Final_data$HCPot)
Final_data$NOxPot <- log(Final_data$NOxPot)
Final_data$S02Pot <- log(Final_data$S02Pot)

###Checking if they became symmetric after log transformation
Final_data %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value))+facet_wrap(~ key, scales = "free") +geom_histogram(bins = 10)
###The histograms of log transformations look symmetric now

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      Bivariate anaysis (Scatterplots & Correlation Matrix)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Pairwise Scatter plots for variables Mortality,HCPot, NOxPot and S02Pot
pairs(Final_data[,12:15])

###we can see a linear pattern between log(HCPot), log(NOxPot) and log(S02Pot). This seems to be a violation of the independence assumption due to collinearity problem. 

##Correlation matrix for variables Mortality,HCPot, NOxPot and S02Pot
install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(Final_data[,12:15]))
###Above stated multicollinearity issue is also supported by correlation matrix.


###Removal of multicollinearity by looking at their VIF values.
###We build a model with all variables first
 
###Building Model using all variables.
All_Var <- lm(Mortality~.,data=Final_data)
summary(All_Var)

###We observe that only few variables (4) are coming up as significant based on p values, and that is because we have not selected the variables to be inluded in the model.
###Next step would be to identify and delete the variables which cause multicollinearity.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           Heteroscedasticity and mulitcollinearity check
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(All_Var)  ##checking for heteroscedasticity (Residuals vs fitted values plot)

###Identify and delete the variables with high VIF values
install.packages("car")
library(car)
all_vifs <- car::vif(All_Var)
print(all_vifs)

###It is observed that the variables log(HCPot) and log(NOxPot) have higher VIF values.
###We deleted log(HCPot) variable and observed that the VIF range has come down explaining reduction in multicollinearity.

Final_data$HCPot <- NULL
Model2 <- lm(Mortality~.,data=Final_data)
all_vifs <- car::vif(Model2)
print(all_vifs)


###Variable selection and Final model
null <- lm(Mortality ~ 1, data = Final_data)
null
full <- lm(Mortality ~ ., data = Final_data)
full
selectedmodel <- step(null, scope=list(lower=null, upper=full), direction="forward")
selctmodel

##Running regression with variables chosen from forward selection

fwdmdl <- lm(formula = Mortality ~ NW + Education + S02Pot + JanTemp + Rain + NOxPot + WC + HHSiz, data = Final_data)  
summary(fwdmdl)

##Running regression again after removing the insignificanyt variables
finalmodel <- lm(formula = Mortality ~ NW + Education  + JanTemp +  Rain + NOxPot , data = Final_data)
summary(finalmodel)

###Leaps also gave the same variables.
	
leaps=regsubsets(Mortality ~ ., data = Final_data, nbest=10)
plot(leaps)

leapsmod <- lm(Mortality ~ JanTemp + Rain  + NW + Education + NOxPot,data = Final_data) 



~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           Principal Component analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("readxl")
install.packages("Hmisc")
install.packages("MASS")

library(readxl) #Read excel files
library(Hmisc) #Contents and Describe
library(MASS) #Variable selection

pcadata <- mydata
pcadata$Mortality <- NULL
fit <- princomp(pcadata, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit, type="lines")

mydata<- cbind(mydata, fit$scores)
head(mydata)

PCModel <- lm(Mortality~Comp.1 +Comp.2 + Comp.3 + Comp.4 + Comp.5 ,data = mydata)
summary(PCModel)

PCModel1 <- lm(Mortality~Comp.1 +Comp.2 + Comp.3 ,data = mydata)
summary(PCModel1)




~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           Market Basket Analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

getwd()
setwd("C:/Users/Arun/Desktop/BA WITH R/Homework 1")


txns = read.transactions("transactions.csv", format = "single", sep = ",", cols = c("Transaction", "Product"), rm.duplicates = FALSE)

itemFrequencyPlot(txns,topN=20,type="absolute")
rules <- apriori(txns, parameter = list(supp = 0.03, conf = 0.20, minlen = 2))
rules <- sort(rules, by="lift", decreasing=TRUE)

# Limit output to 2 digits
options(digits=2)

# Show rules and summary
inspect(rules)
summary(rules)


# Remove duplicate rules
redundant_index <- is.redundant(rules)
pruned_rules <- rules[!redundant_index]
inspect(pruned_rules)
summary(pruned_rules)
