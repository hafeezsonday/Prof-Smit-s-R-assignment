# R Assignment.R
# 21 June 2018
# Female_Secondary_Education_and_Total_Fertility_Rates


# Set-up ------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data ---------------------------------------------------------------

Global_correlation <- read_csv("Secondary.csv")
kenya_combo <- read.csv('Kenya_combo.csv')

# Assumptions ---------------------------------------------------------------
    
shapiro.test(Global_correlation$Percent_enrolled)
shapiro.test(Global_correlation$TFR)
var(Global_correlation$TFR)
var(Global_correlation$Percent_enrolled)

# The simple linear regression equation --------------------------------------------------------------

Global.lm <- lm(Percent_enrolled ~ TFR, data = Global_correlation)
summary(Global.lm)

#Call:
#lm(formula = Percent_enrolled ~ TFR, data = Global_correlation)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-34.370  -8.623   0.558   8.521  41.307 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 113.7353     2.5891   43.93   <2e-16 ***
#  TFR         -16.5220     0.8348  -19.79   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 14.26 on 149 degrees of freedom
#Multiple R-squared:  0.7245,	Adjusted R-squared:  0.7226 
#F-statistic: 391.7 on 1 and 149 DF,  p-value: < 2.2e-16


# A graph of the linear regression ----------------------------------------

slope <- round(Global.lm$coef[2], 3)
p.val = 0.001
r2 <- round(summary(Global.lm)$r.squared, 3)

ggplot(data = Global_correlation, aes(x = Percent_enrolled, y = TFR)) +
  geom_point() +
  stat_smooth(method = "lm", se = F, colour = "red") +
  theme_pubclean()+
  theme(plot.caption = element_text(hjust = 0.5)) +
  labs(x = "Females enrolled in secondary school (%)",
       y = "Total fertility rate (average number of children per woman)",
       caption = "Figure 1: The negative correlation between female secondary education enrollment (%) and total fertitlity rate")

# use the accessor function to grab the coefficients:
Global.coef <- coefficients(Global.lm)
Global.coef

#(Intercept)         TFR 
#113.73531   -16.52198 

cor.test(Global_correlation$Percent_enrolled, Global_correlation$TFR, method = "kendall")

#Kendall's rank correlation tau

#data:  Global_correlation$Percent_enrolled and Global_correlation$TFR
#z = -9.7068, p-value < 2.2e-16
#alternative hypothesis: true tau is not equal to 0
#sample estimates:
#tau 
#-0.5330448 

# Normality ---------------------------------------------------------------

shapiro.test(kenya_combo$Percent_enrollment)
shapiro.test(kenya_combo$TFR)

# Regression --------------------------------------------------------------

Kenya.lm <- lm(Percent_enrollment ~ TFR, data = kenya_combo)
summary(Kenya.lm)

slope <- round(Kenya.lm$coef[2], 3)
p.val = 0.001
r2 <- round(summary(Kenya.lm)$r.squared, 3)

ggplot(data = kenya_combo, aes(x = Percent_enrollment, y = TFR)) +
  geom_point() +
  stat_smooth(method = "lm", se = F, colour = "red") +
  theme_pubclean()+
  theme(plot.caption = element_text(hjust = 0.5)) +
  labs(x = "Females enrolled in secondary school (%)",
       y = "Total fertility rate (average number of children per woman)",
       caption = "Figure 2: The negative correlation between female secondary education enrollment (%) and total fertitlity rate in Kenya from 1999 to 2009")

# use the accessor function to grab the coefficients:
Kenya.coef <- coefficients(Kenya.lm)
Kenya.coef


cor.test(kenya_combo$Percent_enrollment, kenya_combo$TFR, use = "everything", method = "pearson")










