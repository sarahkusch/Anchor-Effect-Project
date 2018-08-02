library("tidyverse") 
library("reshape2")
library("afex")
library("lme4")
library("plyr") 

setwd("C:/Users/Sarah/Desktop/Uni/6. Semester/Experimental Psychology Lab/Anchor-Effect-Project/data/main")

# read in data from the experiment
data <- read_csv("results_main.csv")

# z-transform the answers of the first question and attach them to data set data_ttest
data_ttest <- data %>% filter(question == 1)
z_score <- scale(data_ttest$answer)
data_ttest <- cbind(data_ttest, z_score)

# transformation for answers to questions 2 to 12 and attach them to data set data_ttest
for (i in 2:12) {
  question_i <- data %>% filter(question == i)
  
  z_score <- scale(question_i$answer)
  question_i <- cbind(question_i, z_score)
  data_ttest <- rbind(data_ttest, question_i)
}

# test hyopethesis I by using a paired t-test
hyp_1 <- t.test(data_ttest$z_score ~ data_ttest$anchor, paired = TRUE)

#check the direction of the effect by computing the means of the answers to a 'low (high) anchor question'
high <- filter(data_ttest, anchor == 'high')
mean_zH <- mean(high$z_score)

low <- filter(data_ttest, anchor == 'low')
mean_zL <- mean(low$z_score)

#compare the means, if TRUE direction of hypothesis correct
comp <- mean_zL < mean_zH

# check for normality
#diff <- data$mean_zH - data$mean_zL
#shapiro.test(diff)

# test hypothesis II by using a linear mixed model with random intercept per participant and per question
hyp_2 <- afex::mixed(z_score ~ anchor * anchor_knowledge + (1 | submission_id) + (1 | question), data = data_ttest)



