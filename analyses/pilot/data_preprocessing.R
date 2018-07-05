library("tidyverse") 
library("reshape2")
library("afex")
library("lme4")
library("plyr")
# load piloting data into variable data (incl. VP, knowledge, anchor per question per person, absolute estimate per question per person)

# data <- data.frame(VP,abs_qu_1, anchor1, abs_qu_2, anchor2, abs_qu_3, anchor3, abs_qu_4, anchor4,..., knowledge)


# z transform all answer based on the questions' mean
data$z1 <- scale(data$abs_qu_1)
data$z2 <- scale(data$abs_qu_2)
data$z3 <- scale(data$abs_qu_3)
data$z4 <- scale(data$abs_qu_4)
data$z5 <- scale(data$abs_qu_5)
data$z6 <- scale(data$abs_qu_6)
data$z7 <- scale(data$abs_qu_7)
data$z8 <- scale(data$abs_qu_8)
data$z9 <- scale(data$abs_qu_9)
data$z10 <- scale(data$abs_qu_10)
data$z11 <- scale(data$abs_qu_11)
data$z12 <- scale(data$abs_qu_12)



# save the z-scores in a new variable based on whether 
# the anchor of the respective question has been low or high 
# setting those variable to NA where the other anchor has been given
data <- data %>%
  rowwise() %>%
  mutate(
    z1L = ifelse(anchor1 == 0, z1, NA),
    z1H = ifelse(anchor1 == 1, z1, NA),
    z2L = ifelse(anchor2 == 0, z2, NA),
    z2H = ifelse(anchor2 == 1, z2, NA),
    z3L = ifelse(anchor3 == 0, z3, NA),
    z3H = ifelse(anchor3 == 1, z3, NA),
    z4L = ifelse(anchor4 == 0, z4, NA),
    z4H = ifelse(anchor4 == 1, z4, NA),
    z5L = ifelse(anchor5 == 0, z5, NA),
    z5H = ifelse(anchor5 == 1, z5, NA),
    z6L = ifelse(anchor6 == 0, z6, NA),
    z6H = ifelse(anchor6 == 1, z6, NA),
    z7L = ifelse(anchor7 == 0, z7, NA),
    z7H = ifelse(anchor7 == 1, z7, NA),
    z8L = ifelse(anchor8 == 0, z8, NA),
    z8H = ifelse(anchor8 == 1, z8, NA),
    z9L = ifelse(anchor9 == 0, z9, NA),
    z9H = ifelse(anchor9 == 1, z9, NA),
    z10L = ifelse(anchor10 == 0, z10, NA),
    z10H = ifelse(anchor10 == 1, z10, NA),
    z11L = ifelse(anchor11 == 0, z11, NA),
    z11H = ifelse(anchor11 == 1, z11, NA),
    z12L = ifelse(anchor12 == 0, z12, NA),
    z12H = ifelse(anchor12 == 1, z12, NA)
  ) %>%
  select(VP, z1L:z4H) %>%
  mutate(mean_zL = mean(c(z1L, z2L, z3L, z4L, z5L, z6L, z7L, z8L, z9L, z10L, z11L, z12L), na.rm = TRUE),
         mean_zH = mean(c(z1H, z2H, z3H, z4H, z5H, z6H, z7H, z8H, z9H, z10H, z11H, z12H), na.rm = TRUE))



# check for normality
diff <- data$mean_zH - data$mean_zL
shapiro.test(diff)

# paired t-test based on difference between z- scores 
# from the low vs high anchor condition
t.test(data$mean_zL,data$mean_zH, alternative = "less", paired=TRUE)


####################second hypothesis####################

#TODO: rename variables, comments#
data2 <- select(data,VP, anchor1, anchor2, anchor3, anchor4, anchor5, anchor6, anchor7, anchor8, anchor9, anchor10, anchor11, anchor12, knowledge, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12)


data_test <- melt(data2, id=c("VP","knowledge"), variable.name = "trial")

data_test2 <- data_test[which(data_test$variable == "z1")[1]:nrow(data_test), ]$anchor

data_final <- cbind(data_test[1:(which(data_test$trial == "z1")[1]-1),],data_test2)
data_final$variable <- NULL
data_final <- rename(data_final,c("data_test2" = "z score", "value" = "anchor"))

afex::mixed(data_final$response ~ data_final$anchor * data_final$knowledge + (1 | data_final$VP) + (1 | data_final$trial), data=data_final)

