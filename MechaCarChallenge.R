library(tidyverse)

### MPG Regression

# Read the csv file 
MechaCar <- read.csv('MechaCar_mpg.csv')

# MLR
MechaMPG_lm <- lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar)
summary(MechaMPG_lm)

### Suspension Coil Summary

# Read csv file
suspension_data <- read_csv('Suspension_Coil.csv')

# Summary stats
lot_summary <- suspension_data %>% group_by(Manufacturing_Lot) %>% 
  summarise(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance=var(PSI),SD=sd(PSI))
psi_summary <- suspension_data %>% summarise(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance=var(PSI),SD=sd(PSI))

# t-test for PSI
psi_test <- t.test(suspension_data$PSI,mu = 1500)
