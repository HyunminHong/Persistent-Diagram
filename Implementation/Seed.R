library(mclust)
library(datasetsICR)
library(pdfCluster)
library(scatterplot3d)
setwd("/Users/hyunminhong/Documents/Programmings/R_R studio/Bachelor Thesis/EMC-master/")
source("EMC.R")

data(seeds)

unique(seeds$variety) # Kama Rosa Canadian (ground truth)

# the ground truth labels
ground_truth = as.numeric(seeds$variety)

seeds <- scale(seeds[,-8])

#write.table(seeds, file = "seeds_w_o_labels.txt", sep = " ",
#            row.names = FALSE, col.names = FALSE)



#################################################################
############ EMC ################################################
#################################################################

# Enhanced mode clustering
seed_EMC <- EMC(seeds)


# SC-plot
plot(seed_EMC$SC.plot, type = "p", lwd = 2,
     xlim = c(0,10), ylim = c(0,100),
     xlab = "Index of ordered cluster",
     ylab = "Size of cluster",
     main = "SC-plot (Seeds)",
)
abline(h = seed_EMC$size.threshold, lwd = 2, col = "purple") # results that there are 3 clusters
legend("topright", expression((n*log(n)/20)^{frac(d,d+6)}), col="purple", lwd=8, cex=1)


# plot clusters
n_emc <- 3 # the number of clusters
palette_emc <- rainbow(n_emc) # generate n distinct colors to plot the clusters
palette_emc = palette_emc[seed_EMC$labels] # assign each point to the color where colors represent the clusters

plot(seeds[,2:3], pch = 16, cex = 0.8, col = palette_emc, 
     xlab = "", ylab = "", xaxt="n", yaxt="n", main = "2d Projection Seeds Clustering (EMC)") # plot clusters (2d projection)

scatterplot3d(seeds[,1:3], pch = 16, type = "p",
              cex.symbols = 0.8, color = palette_emc,
              xlab = "X", ylab = "Y", zlab = "Z", main = "3d Projection Seeds Clustering (EMC)")

palette <- rainbow(length(unique(ground_truth)))

plot(seed_EMC, pch=19, cex=0.7,
     xlab="", ylab="", main="Color by Seeds", 
     xaxt="n", yaxt="n", txt.pos=4, col=palette[ground_truth])

legend("topleft", levels(seeds$variety), col=palette, pch=rep(19,6), cex=1)


# compute the adjusted Rand index
adj.rand.index(ground_truth, seed_EMC$labels) # results in 0.7647773


# confusion matrix
confusion_mat_emc <- table(ground_truth, seed_EMC$labels)
row.names(confusion_mat_emc) <- levels(seeds$variety)
colnames(confusion_mat_emc) <- c(1:nrow(seed_EMC$modes))
confusion_mat_emc

#################################################################
############ ToMATo #############################################
#################################################################

ToMATo_labels <- read.csv("~/Downloads/ToMATo/clusters.txt", header = F, sep = " ")
ToMATo_labels <- as.numeric(unlist(ToMATo_labels))

# plot clusters
n <- 3 # the number of clusters
palette_tom <- rainbow(n) # generate n distinct colors to plot the clusters
palette_tom = palette_tom[ToMATo_labels] # assign each point to the color where colors represent the clusters

plot(seeds[,2:3], pch = 16, cex = 0.8, col = palette_tom, 
     xlab = "", ylab = "", xaxt="n", yaxt="n", main = "2d Projection Seeds Clustering (ToMATo)") # plot clusters (2d projection)

# compute the adjusted Rand index
adj.rand.index(ground_truth, ToMATo_labels) # results in 0.794092

# confusion matrix
confusion_mat_tom <- table(ground_truth, ToMATo_labels)
row.names(confusion_mat_tom) <- levels(seeds$variety)
colnames(confusion_mat_tom) <- c(1:max(na.omit(ToMATo_labels)))
confusion_mat_tom

#################################################################
############ k-means ############################################
#################################################################

seed_kmean <- kmeans(seeds, 3)

# compute the adjusted Rand index
adj.rand.index(ground_truth, seed_kmean$cluster) # results in 0.7974953
