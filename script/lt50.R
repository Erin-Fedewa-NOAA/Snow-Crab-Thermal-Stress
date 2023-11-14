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
#Tidy up tank 1 data
t1 %>%
  rowwise() %>%
  mutate(no_alive = sum(c_across(starts_with('tag'))),
         no_dead = 10 - no_alive) %>% #10 crab in each tank
  ungroup() %>%
  mutate(tank_no = 1) %>%
  pivot_longer(7:16, names_to = "tag_no", values_to = "survival") %>% 
#join to tank 2 
 bind_rows(t2 %>% 
             rowwise() %>%
             mutate(no_alive = sum(c_across(starts_with('tag'))),
                    no_dead = 10 - no_alive) %>% #10 crab in each tank
             ungroup() %>%
             mutate(tank_no = 2) %>%
             pivot_longer(7:16, names_to = "tag_no", values_to = "survival")) -> dat
#still need to join biometric data

#Dataset for GLM
dat %>%
  group_by(C_temp_read) %>%
  summarise(no_alive = mean(no_alive, na.rm=TRUE),
            no_dead = mean(no_dead, na.rm=TRUE)) -> mod.dat

#GLM to fit a curve to temperature and alive/dead counts  
  
y = cbind(mod.dat$no_alive, mod.dat$no_dead)
  
model.results = glm(y ~ mod.dat$C_temp_read, binomial)
summary(model.results)

#Calculate LT50, median lethal temperature
lt50 <- dose.p(model.results, p = 0.5)

#Plots
mod.dat %>%
  mutate(prop_survival = no_alive/(no_alive + no_dead)) %>%
  ggplot(aes(C_temp_read, prop_survival)) +
  geom_point() + 
  stat_smooth(method="glm", color="grey", alpha=.1, se=FALSE, 
              method.args = list(family=binomial)) +
  theme_bw() +
  geom_vline(xintercept = 22.46882, linetype="dotted", 
             color = "red", size=1) +
  labs(y="Proportion Survival", x="Temperature (C)")
  


    
