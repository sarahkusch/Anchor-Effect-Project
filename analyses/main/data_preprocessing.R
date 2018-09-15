library("tidyverse") 
library("reshape2")
library("afex")
library("lme4")
library("plyr") 

setwd("C:/Users/Sarah/Desktop/Uni/6. Semester/Experimental Psychology Lab/Anchor-Effect-Project/data/main")

# read in data from the experiment
data <- read_csv("results_main.csv")

#descriptives
age_data <- data %>% filter(age != 99) # exclude false data input
median_age <- median(age_data$age, na.rm = TRUE)
range_age <- range(age_data$age, na.rm = TRUE)

gender <- count(data$gender) # is *12 because there are 12 trials
female <- filter(gender, x=='female')
female_abs <- female$freq/12

male <- filter(gender, x=='male')
male_abs <- male$freq/12

edu <- count(data$education)
college <- filter(edu, x=='graduated_college')
college_abs <- college$freq/12

high_school <- filter(edu, x=='graduated_high_school')
high_school_abs <- high_school$freq/12

higher_degree <- filter(edu, x=='higher_degree')
higher_degree_abs <- higher_degree$freq/12

know_anchor <- count(data$anchor_knowledge)
know_yes <- filter(know_anchor, x=='yes')
know_yes_abs <- know_yes$freq/12

know_no <- filter(know_anchor, x=="no")
know_no_abs <- know_no$freq/12


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


# test hypothesis II by using a linear mixed model with random intercept per participant and per question
hyp_2 <- afex::mixed(z_score ~ anchor * anchor_knowledge + (1 | submission_id) + (1 | question), data = data_ttest)


#PLOTTING

# z_score.x ist high
d <- inner_join(high, low, by ="submission_id")
d <-rename(d, c(z_score.x = "high_anchor", z_score.y = "low_anchor"))

d <- select(d, high_anchor, low_anchor, submission_id)
d <- melt(d,id="submission_id")


plot_anchor <- ggplot(d, aes(variable, value)) + geom_boxplot() + xlab("Anchor condition") + ylab("z-scores") + ggtitle("High vs Low Anchor condition")

