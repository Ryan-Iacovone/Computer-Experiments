# CPU Experiment Laptop

## Data Wrangling

library(tidyverse)
library(readxl)
library(lubridate) #for date extraction and manipulation
library(ggthemes)
library(moments) #used to calculate skewness and kurtosis 
library(emmeans) #Estimated marginal means (Least-squares means) - used for confidence interval calculations
library(corrplot)

excel_file_hwinfo = "Data/experiment_hwinfo.xlsx"

cpu_lap <- read_excel(excel_file_hwinfo, sheet = "cpu_lap")

#Converting voltage and clock speed into factor variables for the following analysis 

cpu_lap$Voltage <- factor(cpu_lap$Voltage, levels = c(0, -80.1, -160.2))
cpu_lap$Clock <- factor(cpu_lap$Clock, levels = c(3.3, 3.4, 3.5))



### Parallel Dot Plot (Can't put the x axis in same order as SAS)

#Creating a new variable called treatment that combines my two predictors together for easy visualization
cpu_lap$treatment <- paste(cpu_lap$Voltage, cpu_lap$Clock, sep = " & ")

cpu_lap %>% ggplot(aes(x= treatment, y = Cinebench)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0.15)) + 
  labs(y = "Cinebench Score",
       x = "Core Clock (MHz) & Voltage (Volt)",
       title = "Parallel Dot Plot of Core Clocks and Voltage") + 
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



### create a linear regression model 

model_cpu_lap <- lm(Cinebench ~ Voltage + Clock, data = cpu_lap)

summary(model_cpu_lap)



### Residual vs predicted graph

# calculate predicted values and residuals and inputs the values into the original data frame
cpu_lap$predicted <- predict(model_cpu_lap)
cpu_lap$residual <- residuals(model_cpu_lap)

# plot the residuals vs predicted values using a scatter plot
ggplot(cpu_lap, aes(x = predicted, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Predicted values", 
       y = "Residuals",
       title = "Residual vs Predicted plot") +
  theme_clean()



### Normal Quantiles plot

#Normal Quantiles plot
ggplot(cpu_lap, aes(sample = model_cpu_lap$residuals)) + 
  stat_qq(color  = "steelblue") + 
  stat_qq_line( color = "darkorange") + 
  #scale_y_continuous(limits = c(-155, 110)) +
  labs(x = "Normal Quantiles",
       y = "Residuals",
       title = "Normal Quantiles Plot to Examine Normality") +
  theme_bw()

#ks.test(model_cpu_lap$residuals, "pnorm")
#shapiro.test(model_cpu_lap$residuals)



### Calculate skewness and kurtosis

# Calculate skewness
skewness(model_cpu_lap$residuals)

# Calculate kurtosis
kurtosis(model_cpu_lap$residuals)

hist(model_cpu_lap$residuals)
summary(model_cpu_lap$residuals)



### plot of residuals vs. your nuisance influence (such as trial number) to examine Independence assumption 

ggplot(cpu_lap, aes(x = Trial, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Trial Number", 
       y = "Residuals",
       title = "Independence Assumption") +
  theme_clean()



### Interaction Plot 

mean_cinebench_cpulap <- cpu_lap %>% group_by(Voltage, Clock) %>% summarise(m_cinebench = mean(Cinebench), n = n()) %>% ungroup() %>% as.data.frame()

#mean_cinebench_cpulap$Voltage <- factor(mean_cinebench_cpulap$Voltage, levels = c(0, -80.1, -160.2))
#mean_cinebench_cpulap$Clock <- factor(mean_cinebench_cpulap$Clock, levels = c(3.3, 3.4, 3.5))


#Interaction Plot
ggplot(mean_cinebench_cpulap, aes(x = Clock, y = m_cinebench, color = Voltage, group = Voltage)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  
  scale_color_manual(values = c("0" = "green", "-80.1" = "red",  "-160.2" = "blue")) + 
  
  scale_x_discrete(limits = c("3.3", "3.4", "3.5"), expand = c(.01, .01)) +
  
  labs(x = "Clock Speed MHz", y = "Cinebench Score") +
  theme_bw() 


#An interaction plot with parallel lines suggests that the effect of one variable on the response variable is consistent across the levels of the other variable. In other words, the relationship between the response variable and one predictor variable is the same regardless of the level of the other predictor variable.

#If the top line has a greater slope than the bottom lines, it suggests that the effect of the predictor variable on the response variable differs across the levels of the other predictor variable. This suggests that there is an interaction effect between the two predictor variables.

#To determine if the interaction effect is statistically significant, you can perform an analysis of variance (ANOVA) or a regression analysis that includes an interaction term. If the p-value associated with the interaction term is less than your chosen significance level, it suggests that the interaction effect is statistically significant.



### Confidence Interval Chart

# obtain the 95% confidence intervals for main effects of voltage and clock speed

ci_volts <- emmeans(model_cpu_lap, ~ Voltage)
ci_clock <- emmeans(model_cpu_lap, ~ Clock)


# obtain the 95% confidence intervals for simple effects
ci_both <- emmeans(model_cpu_lap, ~ Voltage + Clock)

#In statistics, a main effect refers to the overall effect of one independent variable on the dependent variable, averaged across all levels of the other independent variables. In other words, a main effect represents the relationship between an independent variable and the dependent variable while ignoring the other independent variables in the model. For example, in a study examining the effect of a drug on blood pressure, the main effect of the drug represents the average effect of the drug on blood pressure, without considering other factors that may affect blood pressure.

#On the other hand, a simple effect refers to the effect of an independent variable on the dependent variable at a specific level of another independent variable. In other words, a simple effect represents the relationship between an independent variable and the dependent variable while holding other independent variables in the model constant at a particular level. For example, in a study examining the effect of a drug on blood pressure, the simple effect of the drug at a particular dose level represents the effect of the drug on blood pressure at that specific dose level, while holding other factors constant.

#To illustrate the difference between a main effect and a simple effect, consider a study examining the effect of a new teaching method (Method A vs. Method B) on students' test scores, while controlling for students' prior knowledge (low vs. high). In this study, the main effect of teaching method represents the average effect of Method A compared to Method B, across all levels of students' prior knowledge. The simple effect of teaching method at a particular level of students' prior knowledge represents the effect of Method A compared to Method B, but only for students at that specific level of prior knowledge.

#In summary, a main effect represents the overall effect of an independent variable on the dependent variable, while a simple effect represents the effect of an independent variable at a specific level of another independent variable.



anova(model_cpu_lap)