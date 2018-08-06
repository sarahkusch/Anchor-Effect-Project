library("ggplot2")


setwd("C:/Users/Sarah/Desktop/Uni/6. Semester/Experimental Psychology Lab/Anchor-Effect-Project/data/pilot")

data <- read_csv("results_main.csv")

d <- melt(select(data, VP, mean_zH, mean_zL), id="VP")

ggplot(d, aes(variable, value)) + geom_boxplot() + xlab("Anchor") + ylab("z-scores") + ggtitle("Title")