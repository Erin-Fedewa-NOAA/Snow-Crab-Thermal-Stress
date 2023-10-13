#GOALS: 
# 1) Plot out Tidbit temp data for 10/13 LT50 trials  

# Author: Erin Fedewa
# last updated: 10/13/23

# load ----
library(tidyverse)


tid <- read.csv("./data/.csv")

########################################
#tidy data
tid %>%
  #join both tanks