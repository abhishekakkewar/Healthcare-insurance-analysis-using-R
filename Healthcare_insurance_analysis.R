#Importing Data 
df <- read.csv("C:/Users/aakkewar/Documents/BOOTCAMP (PowerBI, SQL, Python, R)/R-Programming/R_NICE/POC/Healthcare Insurance Analysis using R-Programming/insurance.csv")
df
#Reading Data
View(df)
summary(df$charges)
#distribution is right skewed
hist(df$charges) #fig showing right skewed

#To see the distribution of data
table(df$region)
table(df$sex)
table(df$smoker)

#To find the relationship among variables
cor(df[c("age","bmi","children","charges")])

#using scatter plot matrix
pairs(df[c("age","bmi","children","charges")])

#To enhance the plot i am using psych
#Installing package
install.packages("psych")
#Loading Package
library(psych)

#Adding more information to the plot
pairs.panels(df[c("age","bmi","children","charges")])

#Using function lm() to fit the linear regression model
ins_model <- lm(charges ~ age + children + bmi + sex + region, data=df)

ins_model <- lm(charges ~ ., data=df)

#Building model
ins_model

#Viewing more information about the model 
summary(ins_model)

#model specification by adding non-linear relationship

#The relationship between independent and dependent variable
#is assumed to be linear, but this may not necessarily true.
#e.g. the effect of age on medical expenditure may not be constant

#creating new variable to add non-linear

df$age2 <- df$age^2

# creating numeric variable to binary indicator to model the relationship
# by creating a binary obesity indicator that is 1 if the BMI is at least 30 and 0 id less
df$bmi30 <- ifelse(df$bmi >= 30, 1 ,0)

#To interact obesity indicator(bmi30) and the smoking indicator
#using formula in the form charges ~ bmi30*smoker

#putting it all together
ins_model2 <- lm(charges ~ age + age2 + children + bmi +sex + bmi30*smoker + region, data = df)

#finally summarize the result
summary(ins_model2)
