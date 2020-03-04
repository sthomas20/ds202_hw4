library(ggplot2)
library(dplyr)
library(datasets)
?ChickWeight

#Question 1
weight0 <- ChickWeight %>%
  filter(Time == 0) %>%
  group_by(Diet) %>%
  summarize(avg_weight = mean(weight, na.rm = TRUE), sd_weight = sd(weight, na.rm = TRUE)) %>%
  print

  #Extra Credit

#Question 2
#use slice function
ChickWeight

#find number of occurences for each chick
ChickWeight %>%
  group_by(Chick) %>%
  summarize(max12 = n()) %>%
  filter(max12 == 12) %>%
  
  print(n= Inf)


ChickWeight %>% 
  group_by(Chick) %>%
  filter()
  


  
#then go to back orginal data frame to print out data frame that only contains chicks with complete data fram and filter it out

