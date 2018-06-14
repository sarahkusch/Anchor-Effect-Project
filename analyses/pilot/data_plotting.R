library("ggplot2")
# import data in variable data (after analysis)

d <- melt(select(data, VP, mean_zH, mean_zL), id="VP")

ggplot(d, aes(variable, value)) + geom_boxplot() + xlab("Anchor") + ylab("z-scores") + ggtitle("Title")
