# R Assignment.R
# 21 June 2018
# Female_Secondary_Education_and_Total_Fertility_Rates


# Set-up ------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data ---------------------------------------------------------------

Global_correlation <- read_csv("Secondary.csv")
kenya_fertility <- read.csv("kenya_fertility.csv")
kenya_enrollment <- read.csv("kenya_enrollment.csv")
kenya_combo <- read.csv('Kenya_combo.csv')

# plot --------------------------------------------------------------------

ggplot(data = Global_correlation, aes(x = Percent_enrolled, y = TFR)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink")+
  theme_pubclean()+
  labs(title = "Figure 1: The negative correlation between female secondary education enrollment (%) and total fertitlity rate",
       x = "Females enrolled in secondary school (%)",
       y = "Total fertility rate (average number of children per woman)")

ggplot(data = kenya_combo, aes(x = Percent_enrollment, y = TFR)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink")+
  theme_pubclean()+
  labs(title = "Figure 2: The negative correlation between female secondary education enrollment (%) and total fertitlity rate in Kenya from 1999 to 2009",
       x = "Females enrolled in secondary school (%)",
       y = "Total fertility rate (average number of children per woman)")

#plt1 <- ggplot(data = kenya_enrollment, aes(x = Year, y = Female_School_enrollment_secondary)) +
 #geom_point() +
  # geom_smooth(method = "lm", se = F, colour = "green") +
  #theme_pubclean() 

#plt2 <- ggplot(data = kenya_fertility, aes(x = Year, y = Total_Fertility_Rate)) +
 # geom_point() +
  ##theme_pubclean()

#library(ggpubr)
#ggarrange(plt1, plt2, nrow = 2, ncol = 1, labels = "AUTO")


# Normality ---------------------------------------------------------------

shapiro.test(Global_correlation$Percent_enrolled)
shapiro.test(Global_correlation$TFR)

# Regression --------------------------------------------------------------

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

slope <- round(Global.lm$coef[2], 3)
p.val = 0.001
r2 <- round(summary(Global.lm)$r.squared, 3)

ggplot(data = Global_correlation, aes(x = Percent_enrolled, y = TFR)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", se = F, colour = "red") +
  theme_pubclean()+
  labs(title = "Figure 1: The negative correlation between female secondary education enrollment (%) and total fertitlity rate",
       x = "Females enrolled in secondary school (%)",
       y = "Total fertility rate (average number of children per woman)")

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
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, 
           label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", se = F, colour = "red") +
  theme_pubclean()+
  labs(title = "Figure 2: The negative correlation between female secondary education enrollment (%) and total fertitlity rate in Kenya from 1999 to 2009",
       x = "Females enrolled in secondary school (%)",
       y = "Total fertility rate (average number of children per woman)")

# use the accessor function to grab the coefficients:
Kenya.coef <- coefficients(Kenya.lm)
Kenya.coef


cor.test(Kenya.coef$Percent_enrollment, Kenya.coef$TFR, method = "kendall")










