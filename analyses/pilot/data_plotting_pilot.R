library("tidyverse")
library("ggplot2")
library("reshape2")
# import data in variable data (after analysis)


setwd("C:/Users/Sarah/Desktop/Uni/6. Semester/Experimental Psychology Lab/Anchor-Effect-Project/data/pilot")

data <- read_csv("results_pilot.csv")

d <- melt(select(data, submission_id, mean_zH, mean_zL), id="submission_id")

ggplot(d, aes(variable, value)) + geom_boxplot() + xlab("Anchor") + ylab("z-scores") + ggtitle("Title")
