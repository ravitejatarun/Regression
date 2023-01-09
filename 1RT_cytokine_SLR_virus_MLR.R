
# Simple Linear Regression ####
library(readr)
# Importing the cytokine data
cytokine <-  read.csv('rt_cytokine_SLR.csv')
attach(cytokine)
library(Hmisc)
describe(cytokine$cytokine)
class(cytokine$days)
describe(cytokine$days)
#Gini's mean difference (Gmd)is defined as the mean absolute difference between any two distinct elements of a vector

sum(is.na(cytokine$cytokine))

summary(cytokine$cytokine)
hist(cytokine$cytokine, main="Histo of Cytokine", xlab="Cytokine")

plot(cytokine$days,cytokine$cytokine) 

cor(cytokine$days,cytokine$cytokine)

#cor.test(cytokine$days,cytokine$cytokine, use= "complete.obs",  method ="spearman")
cor.test(cytokine$days,cytokine$cytokine, use= "complete.obs",  method ="pearson")


regressor1 = lm(formula =  cytokine ~ days,
               data = cytokine)
summary(regressor1) 
#confint(regressor1)
par(mfrow=c(2,2)) #4 plots in one output 
plot(regressor1) 

par(mfrow=c(1,1)) #one plot per page 

# Predicting the Test set results
y_pred1 = predict(regressor1, newdata = cytokine)

summary(y_pred1)

library(ggplot2)
ggplot() +
  geom_point(aes(x = cytokine$days, y = cytokine$cytokine),
             colour = 'red') +
  geom_line(aes(x = cytokine$days, y = predict(regressor1, newdata = cytokine)),
            colour = 'blue') +
  ggtitle('Cytokine production vs No. of days') +
  xlab('Days') +
  ylab('Cytokine (pg/mL)')



#MLR Virus production#######################

#1. Inspect the dataset 
#to examine the datatype and distribution for all of these variables. 
#for missing values and outliers 
# describe() gives number of values,Range of the values, # of missing values, 
# mean,different quartiles of values in the data set/variable

library(Hmisc)
virus_prodn <- read.csv("rt_virus_prodn_MLR.csv")
head(virus_prodn)
attach(virus_prodn) #with attach() no need to refer dataset name now onwards
describe(virus_prodn)

# use summary statistics and tabulations to examine each variable
#(for outliers missing etc)

library(gmodels)
CrossTable(Clone) #for categorical variable
sum(is.na(Clone)) # to check missing values
#*******************

# for CONTINUOUS variables
summary(cytokine1)
summary(cytokine2)
summary(cytokine3)
summary(Production)
hist(cytokine1) # any potential outliers

#************************************

#2.Examine the relationship:candidate (Outocme) vs predictor variables 

# Just variable categorical = CrossTable()
# Just variable continous =summary() and hist()

# between variables CATEGORICAL =Cross tabulations
# between variables CONTINUOUS =cor(); scatterplot()

#with outcome = lin. reg. with 1 variable
my_data <- virus_prodn[,c("Production","cytokine1", "cytokine2", "cytokine3")]
cor_matrix <- cor(my_data)
cor_matrix
round(cor_matrix,2)

#cor() default output is Pearson's correlation coefficient.
# if you want Spearman's correlation coefficient, need to specify; method = 'spearman' 


#There is an easy way to visually assess the correlation using the pairs() function  #CONTINUOUS

pairs(~Production+cytokine1+cytokine2+cytokine3, data=virus_prodn) # scatterplot matrix

#examine associations between categorical variables,cross tabulations
CrossTable(Clone)

mlr_virus = lm(formula = Production ~ cytokine1+cytokine2+cytokine3,
               data = virus_prodn)

summary(mlr_virus)
confint(mlr_virus)


#***********************************

















par(mfrow=c(2,2))
plot(mlr_virus)
par(mfrow=c(1,1))

library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$mRNA1, y = test_set$Production),
             colour = 'red') +
  geom_line(aes(x = training_set$mRNA1, y = predict(regressor1, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Virus Production vs mRNA1 Expression (Test set)') +
  xlab('mRNA1 Expression') +
  ylab('Virus mRNA Expression/Production')


#********************************************************************************************************







