# CPU Experiment Desktop

## Data wrangling

library(tidyverse)
library(readxl)
library(lubridate) #for date extraction and manipulation
library(ggthemes)
library(moments) #used to calculate skewness and kurtosis 
library(emmeans) #Estimated marginal means (Least-squares means) - used for confidence interval calculations

#Reading in the excel file
excel_file_hwinfo = "../Data/experiment_hwinfo.xlsx"

cpu <- read_excel(excel_file_hwinfo, sheet = "cpu_test")

#Converting voltage and clock speed into factor variables for the following analysis 

cpu$Voltage <- factor(cpu$Voltage, levels = c(1.1, 1.175, 1.225, 1.275))
cpu$Clock <- factor(cpu$Clock, levels = c(4, 4.5, 5, 5.4))




#Getting the trial number order for the test set 

set.seed(123)  # set the seed to reproduce the same results
digits <- 1:32  # create a vector of digits 1-8
shuffled_digits <- sample(digits)  # randomly order the digits
shuffled_digits  # print the shuffled digits



## Visualizing the dataset and checking assumptionsprior prior to t-test 


### Parallel Dot Plot

cpu$treatment <- paste(cpu$Clock, cpu$Voltage, sep = " & ")

cpu %>% ggplot(aes(x= treatment, y = Cinebench)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0.15)) + 
  
  labs(y = "Cinebench Score",
       x = "Core Clock (MHz) & Voltage (Volt)",
       title = "Parallel Dot Plot of Core Clocks and Voltage",
       caption = "Figure 1.1") + 
  
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### Residual vs predicted graph 

# create a linear regression model 
base_model_cpu <- lm(Cinebench ~ Voltage + Clock , data = cpu)

# calculate predicted values and residuals and inputs the values into the original data frame
cpu$predicted <- predict(base_model_cpu)
cpu$residual <- residuals(base_model_cpu)

# plot the residuals vs predicted values using a scatter plot
ggplot(cpu, aes(x = predicted, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Predicted values", 
       y = "Residuals",
       title = "Residual vs Predicted plot",
       caption = "Figure 1.2") +
  theme_clean()



### Normal Quantiles plot

ggplot(cpu, aes(sample = base_model_cpu$residuals)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = "Normal Quantiles",
       y = "Residuals",
       title = "Normal Quantiles Plot to Examine Normality")



### Calculate skewness and kurtosis

# Calculate skewness
skewness(base_model_cpu$residuals)

# Calculate kurtosis
kurtosis(base_model_cpu$residuals)

hist(base_model_cpu$residuals)
summary(base_model_cpu$residuals)



### Plot of residuals vs. your nuisance influence (such as trial number) to examine Independence assumption 

ggplot(cpu, aes(x = Trial, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Trial Number", 
       y = "Residuals",
       title = "Independence Assumption") +
  theme_clean()



### Interaction Plot - no interaction 

mean_cinebench_cpu <- cpu %>% 
  group_by(Voltage, Clock) %>% 
  summarise(m_cinebench = mean(Cinebench, na.rm = TRUE), n = n()) %>% 
  ungroup() %>% 
  #complete(Voltage, Clock) %>% 
  as.data.frame()

#mean_cinebench_cpu$Voltage <- factor(mean_cinebench_cpu$Voltage, levels = c(1.1, 1.175, 1.225, 1.275))
#mean_cinebench_cpu$Clock <- factor(mean_cinebench_cpu$Clock, levels = c(4, 4.5, 5, 5.4))


#Interaction Plot with Voltage on x-axis (looks nicer)
ggplot(mean_cinebench_cpu, aes(x = Voltage, y = m_cinebench, color = Clock, group = Clock)) +
  geom_point(size = 2, na.rm = TRUE) +
  geom_line(size = 1, na.rm = TRUE) +
  
  scale_color_manual(values = c("4" = "green", "4.5" = "red",  "5" = "blue", "5.4" = "purple")) + 
  
  scale_x_discrete(limits = c("1.1", "1.175", "1.225", "1.275")) +
  
  labs(x = "Clock Speed MHz", y = "Cinebench Score") +
  theme_bw() +
  guides(color = guide_legend(reverse = TRUE))


# Create a column for line type
mean_cinebench_cpu$linetype <- ifelse(mean_cinebench_cpu$Voltage == "1.1", "solid",
                                      ifelse(mean_cinebench_cpu$Voltage == "1.175", "dashed",
                                             ifelse(mean_cinebench_cpu$Voltage == "1.225", "dotted", "dotted")))

#Interaction Plot with clock on the x-axis (not nice visually)
ggplot(mean_cinebench_cpu, aes(x = Clock, y = m_cinebench, color = Voltage, group = Voltage)) +
  geom_point(size = 2, na.rm = TRUE) +
  geom_line(aes(linetype = linetype), size = 1, na.rm = TRUE) +
  
  scale_color_manual(values = c("1.1" = "green", "1.175" = "red",  "1.225" = "blue", "1.275" = "purple")) + 
  
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash"), guide = "none") + # Set line type for each factor level
  
  scale_x_discrete(limits = c("4", "4.5", "5", "5.4")) +
  
  labs(x = "Clock Speed MHz", y = "Cinebench Score") +
  theme_bw() 

#If the lines in an interaction plot are on top of each other, it suggests that there is no interaction effect between the two variables being plotted. This means that the effect of one variable on the response variable is the same regardless of the level of the other variable.

#In other words, if you have an interaction plot of two factors A and B on a response variable Y, and the lines representing the different levels of A are parallel to each other across all levels of B, then there is no interaction effect between A and B. This means that the effect of A on Y is the same regardless of the level of B.

#On the other hand, if the lines representing the different levels of A are not parallel across all levels of B, then there is an interaction effect between A and B. This suggests that the effect of A on Y depends on the level of B.



## Regression Model

#Voltage:Clock is the interaction variable(but it;s not significant)

cpu$Voltage<- as.numeric(cpu$Voltage)
cpu$Clock<- as.numeric(cpu$Clock)

cpu_model <- lm(Cinebench ~ temp + power + Voltage + Clock + Voltage:Clock, data = cpu)
summary(cpu_model)
anova(cpu_model)

#Testing regression models with just one of the main effects 
mod <- lm(Cinebench ~ power, data = cpu)
summary(mod)



### Confidence Interval Chart

# obtain the 95% confidence intervals for main effects of voltage and clock speed

ci_volts <- emmeans(cpu_model, ~ Voltage)
ci_clock <- emmeans(cpu_model, ~ Clock)


# obtain the 95% confidence intervals for simple effects
ci_both <- emmeans(cpu_model, ~ Voltage + Clock)