library("tidyverse") 
library("reshape2")
library("ggplot2")
library("afex")

# creating a fake data set
columns <- c("comparative_qu_1","abs_qu_1","anchor1","comparative_qu_2","abs_qu_2","anchor2","comparative_qu_3","abs_qu_3","anchor3","comparative_qu_4","abs_qu_4","anchor4")
VP <- c("A","B","C","D","E")
abs_qu_1 <- c(9000,7500,6000,10000,5000)
anchor1 <- c(1,1,0,1,0)
abs_qu_2 <- c(350, 500, 280, 600, 400)
anchor2 <- c(1,1,0,1,0)
abs_qu_3 <- c(17, 25, 50, 30, 45)
anchor3 <- c(0,0,1,0,1)
abs_qu_4 <- c(4,5,9,4,8)
anchor4 <- c(0,0,1,0,1)

# creating a data frame from the fake data
data <- data.frame(VP,abs_qu_1, anchor1, abs_qu_2, anchor2, abs_qu_3, anchor3, abs_qu_4, anchor4)


# z transform all answer based on the questions' mean
data$z1 <- scale(data$abs_qu_1)
data$z2 <- scale(data$abs_qu_2)
data$z3 <- scale(data$abs_qu_3)
data$z4 <- scale(data$abs_qu_4)

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
    z4H = ifelse(anchor4 == 1, z4, NA)
  ) %>%
  select(VP, z1L:z4H) %>%
  mutate(mean_zL = mean(c(z1L, z2L, z3L, z4L), na.rm = TRUE),
         mean_zH = mean(c(z1H, z2H, z3H, z4H), na.rm = TRUE))

# setting absolute answer to null such that they aren't displayed anymore, 
#data$abs_qu_1 <- NULL
#data$abs_qu_2 <- NULL
#data$abs_qu_3 <- NULL
#data$abs_qu_4 <- NULL

# check for normality
diff <- data$mean_zH - data$mean_zL
shapiro.test(diff)

# paired t-test based on difference between z- scores 
# from the low vs high anchor condition
t.test(data$mean_zL,data$mean_zH,paired=TRUE)


d <- melt(select(data, ) id="VP")
#plotting
#ggplot(d, aes(variable, value)) + geom_boxplot() + xlab("Anchor") + ylab("z-scores") + ggtitle("Title")


# has to be melted beforehand; response is z-sores; anchor is high/low; knowledge of anchor is yes/no, paricipant is VP, trial is 1-12 
mixed(response ~ anchor * knowldege_of_anchor + (1 | participant) + (1 | trial))



# code for data_preprocessing.R
# setting absolute answer to null such that they aren't displayed anymore
#data$abs_qu_1 <- NULL
#data$abs_qu_2 <- NULL
#data$abs_qu_3 <- NULL
#data$abs_qu_4 <- NULL
#data$abs_qu_5 <- NULL
#data$abs_qu_6 <- NULL
#data$abs_qu_7 <- NULL
#data$abs_qu_8 <- NULL
#data$abs_qu_9 <- NULL
#data$abs_qu_10 <- NULL
#data$abs_qu_11 <- NULL
#data$abs_qu_12 <- NULL