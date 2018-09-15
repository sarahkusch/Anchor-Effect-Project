library("ggplot2")


setwd("C:/Users/Sarah/Desktop/Uni/6. Semester/Experimental Psychology Lab/Anchor-Effect-Project/data/pilot")

data <- read_csv("results_main.csv")

# z_score.x ist high
d <- inner_join(high, low, by ="submission_id")

d <- select(d, z_score.x, z_score.y, submission_id)
d<- melt(d,id="submission_id")

ggplot(d, aes(variable, value)) + geom_boxplot() + xlab("Anchor") + ylab("z-scores") + ggtitle("Title")
