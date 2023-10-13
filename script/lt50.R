#GOALS: 
# 1) Determine LT50 and CTMax of juvenile snow crab from lab experiments run at
    # KFRC in Fall 2023
# 2) Assess variation in physiological thresholds by sex and crab size 

# Author: Erin Fedewa
# last updated: 10/13/23

# load ----
library(tidyverse)
library(MASS)

t1 <- read.csv("./data/LT50_tank1.csv")
t2 <- read.csv("./data/LT50_tank2.csv")
bio <- read.csv("./data/LT50_biometrics.csv")

#############################
#Tidy up data
t1 %>%
  mutate(no_alive = rowSums(dplyr::select(7:16)),
         no_dead = no_alive - 10) #10 crab in each tank
  pivot_longer(7:16, names_to = "tag_no", values_to = "survival") %>%
#pivot longer and join tank 1 and 2 data

#join biometric data 

 
#GLM to fit a curve to temperature and alive/dead counts  
y = cbind(no_alive, no_dead)
  
model.results = glm(y ~ C_temp_read, binomial)
summary(model.results)

#Calculate LT50, median lethal temperature
dose.p(model.results, p = 0.5)

#Plots
plot(temperature, (alive / (alive + dead)), ylab = "Proportional Survival")

# See: https://lukemiller.org/index.php/2010/02/calculating-lt50-median-lethal-temperature-aka-ld50-quickly-in-r/

    
