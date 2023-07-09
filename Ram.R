# RAM Analysis looking at the impact of XMP on performance

## Data Wrangling 

library(tidyverse)
library(readxl)
library(lubridate) #for date extraction and manipulation
library(ggthemes)
library(moments) #used to calculate skewness and kurtosis
library(emmeans) #Estimated marginal means (Least-squares means) - used for confidence interval calculations

excel_file_hwinfo = "Data/experiment_hwinfo.xlsx"

ram <- read_excel(excel_file_hwinfo, sheet = "ram_test")



#Getting the trial number order for the test set 

set.seed(123)  # set the seed to reproduce the same results
digits <- 1:8  # create a vector of digits 1-8
shuffled_digits <- sample(digits)  # randomly order the digits
shuffled_digits  # print the shuffled digits



## Visualizing the dataset prior to the t-test and checking assumptions ###### 


ram %>% ggplot(aes(x= MHz, y = timespy_total)) + 
  geom_point() + 
  labs(y = "Timespy Score",
       title = "Parallel Dot Plot of Timespy Score and Ram MHz") + 
  theme_clean()


### Residual vs predicted graph 

# create a linear regression model
reg_model_ram <- lm(timespy_total ~ MHz, data = ram)

# calculate predicted values and residuals and inputs the values into the original data frame
ram$predicted <- predict(reg_model_ram)
ram$residual <- residuals(reg_model_ram)

# plot the residuals vs predicted values using a scatter plot
ggplot(ram, aes(x = predicted, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Predicted values", 
       y = "Residuals",
       title = "Residual vs Predicted plot") +
  theme_clean()



### Normal Quantiles plot

ggplot(ram, aes(sample = reg_model_ram$residuals)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Normal Quantiles",
       y = "Residuals",
       title = "Normal Quantiles Plot to Examine Normality")



### Calculating skewness and kurtosis

# Calculate skewness
skewness(reg_model_ram$residuals)

# Calculate kurtosis
kurtosis(reg_model_ram$residuals)

hist(reg_model_ram$residuals)
summary(reg_model_ram$residuals)



### plot of residuals vs. nuisance influence (such as trial number) to examine Independence assumption 

ggplot(ram, aes(x = Trial, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Trial Number", 
       y = "Residuals",
       title = "Independence Assumption") +
  theme_clean()



## Means table grouped by ram speed (MHz) t-tset analysis 

#Means table grouped by ram speed (MHz)
ram %>% group_by(MHz) %>% summarise(n_mean = mean(timespy_total))

#Organizing the timespy data by creating vector of just timespy scores when ram speed is 4800 MHz and 6000MHz

r_4800 <- ram %>% filter(MHz == 4800) %>% select(timespy_total) 
r_6000 <- ram %>% filter(MHz == 6000) %>% select(timespy_total) 

#conducting a t-test (difference in means) between the timespy score when ram MHz is set to default 4800 vs EXPO setting of 6000

t_test_ram <- t.test(r_6000,r_4800)

t_test_ram

############## CANT GO ANY FURHTER IN THIS ANALYSIS SINCE I'VE ONLY GOT TWO MEANS TO COMPARE NO ANOVA ############## 
