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
#find number of occurences for each chick
ChickWeight %>%
  group_by(Chick) %>%
  summarize(max12 = n()) %>%
  filter(max12 != 12) %>% 
  print(n= Inf)


complete <- filter(ChickWeight, Chick != 8 & Chick != 15 & Chick != 16 & Chick != 18 & Chick != 44)
head(complete, n=3)


#Question 3
complete<- complete %>%
  group_by(Chick) %>%
  mutate(weightgain = (weight - weight[Time == 0]))

head(complete, n= 3)

#Question 4 ?
day21<- filter(complete, Time == 21)
one<- tmp <- ggplot(day21, aes(x = reorder(Diet, weightgain, FUN = median), y = weightgain)) + geom_boxplot() + expand_limits(y=0) + xlab("Weight Gain") + ylab("Amount of Weight Gain by day 21") 
one<- one + theme(plot.title = element_text(hjust=0.5))
one

#Question 5
#a
#complete$weightgain = as.factor(complete$weightgain)
str(complete)

ggplot(complete, aes(x=Time, y=weightgain, group=Chick, colour= Chick)) + geom_line() + geom_point() + guides(colour = guide_legend(reverse=T))


#Question 6
least<- complete[complete[, 'Time'] == 0, ]

least<- least[order(least$weight), ] 
View(least)

most<- complete[complete[, 'Time'] == 21, ]
most<- most[order(most$weight), ]
most



#Question 7





#Question 8

