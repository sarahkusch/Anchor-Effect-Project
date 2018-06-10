library("tidyverse") 
library("reshape2")
install.packages("nortest")

# load piloting data into variable data

# data <- data.frame(VP,abs_qu_1, anchor1, abs_qu_2, anchor2, abs_qu_3, anchor3, abs_qu_4, anchor4)


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

# setting absolute answer to null such that they aren't displayed anymore
data$abs_qu_1 <- NULL
data$abs_qu_2 <- NULL
data$abs_qu_3 <- NULL
data$abs_qu_4 <- NULL
data$abs_qu_5 <- NULL
data$abs_qu_6 <- NULL
data$abs_qu_7 <- NULL
data$abs_qu_8 <- NULL
data$abs_qu_9 <- NULL
data$abs_qu_10 <- NULL
data$abs_qu_11 <- NULL
data$abs_qu_12 <- NULL

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
nortest::lillie.test(diff)

# paired t-test based on difference between z- scores 
# from the low vs high anchor condition
t.test(data$mean_zL,data$mean_zH,paired=TRUE)
