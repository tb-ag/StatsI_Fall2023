# Name: Tolga Bag | Student No: 23371290
# I manually created the table on a CSV called PS01.CSV in my directory.
# first, I set up my directory

getwd()
setwd("C:/Users/tolga/OneDrive/Documents/GitHub/StatsI_Fall2023")

#I'll use tidyverse for convenience with code.
library(tidyverse)

################################QUESTION 1 ############################

# I create the shared table as a  dataframe.

data <- data.frame(
  variable = c("upper_class", "lower_class"),
  not_stopped = c(14, 7),
  bribe_requested = c(6, 7),
  stopped_given_warning = c(7, 1)
)

print(data)
chisq.test(data)
##a.Calculating the x2 test statistic##
#chi square test gives a warning. I need to ensure that the x axis is
#numerical for it to work. I assign 1 to upper class and 2 to lower class

data$variable <- as.numeric(factor(data$variable, 
                                   levels = c("upper_class", 
                                              "lower_class")))
print(data)
chisq.test(data)
#our chi test score is 4.844. I spent hours to fix the error
#but I couldn't fix it.

##b.Calculating the p score##

#Our question is if there is a relationship between the class and the
#solicitation of bribes from people who are violating traffic rules.
#our null hypothesis is there is none and they are independent. 
#Our alternative hypothesis is there is a casual relationship.

chisq.test(data)
#p value is 0.1836, in other words higher than the 0.1 threshold. I can't
#reject the hypothesis and will accept that they are independent.

##c.Calculating the residuals and putting them in the table##
#I create a table with the chi square test to calculate the residuals
cht <- chisq.test(data)
data_res <- cht$residuals
print(data_res)

#I need to place upper class and lower class
data_res[data_res == -0.634335047416547] <- "upper_class"
data_res[data_res == 0.814091578410694] <- "lower_class"

print(data_res)
#I'll save it as a PDF file to make it easy to c/p. I find the solution:
#https://bookdown.org/yihui/rmarkdown-cookbook/kable.html 
install.packages("knitr")
library(knitr)    
kable(data_res, format= "latex")

##d.interpreting the results##
#residual is the difference between what we observe and what we expected
#per our model. Here the residuals confirm there is no dependency between
#the variables because there is no similarity between them.

################################QUESTION 2 ############################
#I pull the data from the link:
poldata <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

#a. our null and alternative (two-tailed) hypothesis#
#since we know females complain more about the quality of water then men,
#I need to check the relationship between new or repaired water facilities,
#and irrigation works that are not requested from women. 
# I'll make a refined table that contains these.

poldata_fwi <- poldata [c("female", "water", "irrigation")]
#however, this includes both the villages with and without female leaders
#I filter out the villages without female leaders to check the ones led
#by females.
poldata_fwi <- subset(poldata_fwi, female == 1)

#my null hypothesis is there is no dependency between irrigation works
#and the number of new or repaired water facilities in the female-led
#GPs. My alternative hypothesis is there is a statistically significant 
#relationship as I expect female leaders to prioritize water works.


#b. bivariate regression testing my hypothesis#
#I use the the linear regression code to check the model
bivar_fwi <- lm(irrigation ~ water, data = poldata_fwi)
#I check the summary to see the statistical significance.
summary(bivar_fwi)
#p value is 2.601e-09/ I check what it is in 0 terms.
sprintf("%.20f",2.601e-09)

#That is 0.000000002601. Hence, I reject the null hypothesis and
#establish that there is a statistically significant relationship between
#water works and irrigation. In other words, there is a statistically
#significant signal that female leaders prioritize the water works
#requested by their female constituencies.

#c. Interpret the coefficient estimate for reservation policy.#
#to do that I'll filter the reservation and water columns and then check

poldata_rwi <- subset(poldata[c("reserved", "water")])

#this includes both the ones with reserved policy and not. I filter the
#ones without the reserve policy to check the affect on female leadership.

poldata_rwi <- subset(poldata_rwi, reserved == 1)

#then, I run the model to see the coefficients
bivar_rwi <- lm(reserved ~ water, data = poldata_rwi)
summary(bivar_rwi)
#Per the coefficient estimates, reservation policy enables a slight 
#increase in the number of water works conducted.