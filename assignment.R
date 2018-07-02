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

# Assumptions for Global data---------------------------------------------------------------
    
shapiro.test(Global_correlation$Percent_enrolled)
shapiro.test(Global_correlation$TFR)
var(Global_correlation$TFR)
var(Global_correlation$Percent_enrolled)

# The simple linear regression equation --------------------------------------------------------------

Global.lm <- lm(Percent_enrolled ~ TFR, data = Global_correlation)
summary(Global.lm)

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


# Global data correlation -------------------------------------------------

cor.test(Global_correlation$Percent_enrolled, Global_correlation$TFR, method = "kendall")

# Assumptions for Kenya data ---------------------------------------------------------------

shapiro.test(kenya_combo$Percent_enrollment)
shapiro.test(kenya_combo$TFR)
var(kenya_combo$Percent_enrollment)
var(kenya_combo$TFR)

# Kenya Regression --------------------------------------------------------------

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


# Kenya correlation -------------------------------------------------------

kenya_pearson <- cor.test(kenya_combo$Percent_enrollment, kenya_combo$TFR, use = "everything", method = "pearson")










