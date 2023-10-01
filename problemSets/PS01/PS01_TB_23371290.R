#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stringr"),  pkgTest)

#####################
# Question 1.1 
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# I calculated the mean, variability, standard deviation and standard error.
# I made them vectors for convenience. 

mean(y)
meany <- mean(y)
vary <- var(y)
sdv <- sd(y)
sde <- sdv/sqrt(length(y))

# I check the distribution of the y. It is normal distribution.
hist(y)

# I calculate the quantile percentages per the 90% confidence interval
qnorm(0.05) # value for first 5%
qnorm(0.95) # value for last 5%

# I calculate the lower and upper bound for 90 confidence level.
lower_90 <- qnorm(0.05, 
                    mean = meany, 
                    sd = sde)

# Upper bound, 95 confidence level
upper_90 <- qnorm(0.95,
                    mean = meany,
                    sd = sde)

print(lower_90)
print(meany)
print(upper_90)

#Based on a 90% confidence interval calculation for y, 
#the CI ranges from 94.13283 to 102.7472 and the mean is 98.44.

#####################
# Question 1.2 
#####################
#our hypothesis is average student IG is higher than the ones in the country
#our null hypothesis is it is lower. P is lower or equals to 0.05.
#I already have mean (meany) and standard deviation (sde) as vectors
#before the p score, I need to find the z score. 

yzscore <- (meany-100)/1
print (yzscore)

#I have the zscore as -1.56. I calculate the p score:

ypscore <- 2-pnorm(-abs(yzscore))
print (ypscore)
#our null hypothesis is correct since p is 1.94062 and lower than 5.
#we reject our hypothesis and can say that the average student IQ in 
#the counselor`s school is lower than the average IQ score (100) among 
#all the schools in the country.


#####################
# Question 2.1
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
# State 50 states in US
# Y: per capita expenditure on shelters/housing assistance in state
# X1: per capita personal income in state
# X2: Number of residents per 100,000 that are "financially insecure" in state
# X3: Number of people per thousand residing in urban areas in state
# Region: 1=Northeast, 2= North Central, 3= South, 4=West

#just reviewing the table and its structure
View(expenditure)
head(expenditure)
str(expenditure)

Y <- expenditure$Y
X1 <- expenditure$X1
X2 <- expenditure$X2
X3 <- expenditure$X3


# I researched to plot all 4 variables. I used the ggplot approach detailed in 
#`https://smin95.github.io/dataviz/basics-of-ggplot2-and-correlation-plot.html`
install.packages("smplot2", "ggplot2", "tidyverse")
library(tidyverse)

ggplot(data = expenditure) + 
  geom_point(mapping = aes(x=X1, y=Y, color = X2))
#per ggplot1, there is positive correlation between Y & X1, but X2 is independent
#from both. It might be more meaningful to check the Y & X2. 

ggplot(data = expenditure) + 
  geom_point(mapping = aes(x=X2, y=Y, color = X3))
#per ggplot2 there is no correlation between Y and X2, which is interesting

ggplot(data = expenditure) + 
  geom_point(mapping = aes(x=X3, y=Y, color = X1))
#per ggplot3, there is a positive correlation between x3 and Y.

#####################
# Question 2.2
#####################
ggplot(data = expenditure) + 
  geom_point(mapping = aes(x=Y, y=Region))
#as it is merely two variables, I am using the plot command:

plot(expenditure$Region,expenditure$Y)
#however, this doesn't look good. There are decimals in the x axis and axis
#names are with the expenditure$ sign. After Googling I find:
#https://ggplot2.tidyverse.org/articles/faq-axes.html as a solution.

library(scales)
ggplot(expenditure, aes(x = Region, y = Y)) +
  geom_point() +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  scale_y_continuous(labels = label_number(accuracy = 1))

#however, when I run this, I can"t see the median on the plot. I research more
# and find the answer here: https://stackoverflow.com/questions/39628480/plotting-median-of-the-points-in-r-ggplot

ggplot(expenditure, aes(x = Region, y = Y, group = Region)) +
  geom_point() +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) + geom_boxplot()

#Per ggplot4, region 4 has the highest per capita expenditure on housing assistance on average
#On average, the highest per expenditure on housing assistance is ranked from highest:
#West, North Central, North East and South.

#####################
# Question 2.3
#####################
ggplot(data = expenditure) + 
  geom_point(mapping = aes(x=X1, y=Y))
#On average, there is a positive correlation between per capita expenditure and 
#per capita personal income in a state as seen on ggplot5. 
##Again, I use https://smin95.github.io/dataviz/basics-of-ggplot2-and-correlation-plot.html
#as a reference to enrich my code and add the regions in ggplot5.

ggplot(data = expenditure) + 
  geom_point(mapping = aes(x = X1, y = Y, color = Region))

# I attempt to add shape into the mix but it is giving me an error. I find a solution
#here: https://github.com/tidyverse/ggplot2/issues/519 and ggplot6 has the shapes, but
#it is not clear which shape represents which region as it is mixed with color.

ggplot(data = expenditure) + 
  geom_point(mapping = aes(x = X1, y = Y, color = Region, shape= Region)) +
  scale_shape_identity()

# I search for solutions on Google and asked ChatGPT. ChatGPT mentioned something called
# scale_shape_manual and I search and find the answer here: https://blog.albertkuo.me/post/point-shape-options-in-ggplot/#:~:text=You%20can%20change%20the%20number,Below%20is%20an%20example.

ggplot(data = expenditure) + 
  geom_point(mapping = aes(x = X1, y = Y, shape = Region, color = Region)) +
  scale_shape_manual(values = c("Region 1" = 1, "Region 2" = 2, "Region 3" = 3, "Region 4" = 4)) +
  theme(legend.position = "top")

#It gives an error, but it is possible to see the regions per the color:
#o is Region 1, triangle is region 2, + is region 3 and x is Region 4.
