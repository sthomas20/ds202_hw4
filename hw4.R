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
one<- tmp <- ggplot(day21, aes(x = reorder(Diet, weightgain, FUN = median), y = weightgain)) + geom_boxplot() + expand_limits(y=0) + xlab("Diet") + ylab("Weight Gain") + ggtitle("Amount of weight gained over time")
one<- one + theme(plot.title = element_text(hjust=0.5))
one

#The boxplot I think clear shows that the increase in weight gain as the value of your diet again increases based on looking at the median but breaks after diet 2. I see this because the median for diet 3 is larger than diet 4. You can also see that the diet there has the highest weight gain based on the IQR and the median shown. 

#Question 5

maxWeight <- max(complete$weightgain)
maxWeight

minWeight<- min(complete$weightgain)
minWeight

weightGain<- complete %>% 
  group_by(Chick) %>%
  filter(weightgain == maxWeight | weightgain == minWeight)
print(weighGain)

ggplot(weightGain, aes(x=Time, y=weight, colour= Chick)) + geom_line() + geom_point() + guides(colour = guide_legend(reverse=T)) + facet_wrap(~Diet) + xlab("Time") + ylab("Weight") + ggtitle("Most/Least weight gained Chick")


#Question 6
chicks<- complete %>% 
  group_by(Chick) %>%
  filter(Chick == 3 | Chick == 35)

ggplot(chicks, aes(x=Time, y=weight, colour= Chick)) + geom_line() + geom_point() + ggtitle("Chick # 3 and 35 growth trajectories") + xlab("Time") + ylab("Weight")


#Question 7
ggplot(chicks, aes(x=Time, y=weight, colour= Chick)) + geom_line() + geom_point() + ggtitle("Chick # 3 and 35 growth trajectories") + xlab("Time") + ylab("Weight") + geom_smooth(method="lm") + geom_smooth(data=complete, aes(x = Time, y = weight, colour="Trend Line"), method="lm")

  




#Question 8

