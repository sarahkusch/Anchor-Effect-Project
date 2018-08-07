library("tidyverse") 
library("reshape2")
library("ggplot2")
library("afex")
library("lme4")
library("plyr") 

setwd("C:/Users/Sarah/Desktop/Uni/6. Semester/Experimental Psychology Lab/Anchor-Effect-Project/data/pilot")

data <- read_csv("results_pilot.csv")


data_ttest <- data %>% filter(question == 1)

z_score <- scale(data_ttest$answer)
data_ttest <- cbind(data_ttest, z_score)

for (i in 2:12) {
  question_i <- data %>% filter(question == i)
  
  z_score <- scale(question_i$answer)
  question_i <- cbind(question_i, z_score)
  data_ttest <- rbind(data_ttest, question_i)
}


hyp_1 <- t.test(data_ttest$z_score ~ data_ttest$anchor, paired = TRUE)

high <- filter(data_ttest, anchor == 'high')
mean_zH <- mean(high$z_score)

low <- filter(data_ttest, anchor == 'low')
mean_zL <- mean(low$z_score)

comp <- mean_zL < mean_zH
# check for normality
#diff <- data$mean_zH - data$mean_zL
#shapiro.test(diff)


####################second hypothesis####################
# linear mixed model with 
# random intercept per participant and per question
hyp_2 <- afex::mixed(z_score ~ anchor * anchor_knowledge + (1 | submission_id) + (1 | question), data = data_ttest)

# z_score.x ist high
d <- inner_join(high, low, by ="submission_id")

d <- select(d, z_score.x, z_score.y, submission_id)
d<- melt(d,id="submission_id")

ggplot(d, aes(variable, value)) + geom_boxplot() + xlab("Anchor") + ylab("z-scores") + ggtitle("Title")

