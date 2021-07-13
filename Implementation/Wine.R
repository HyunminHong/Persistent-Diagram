library(tidyverse)
library(nonet)
library(pdfCluster)
setwd("/Users/hyunminhong/Documents/Programmings/R_R studio/Bachelor Thesis/EMC-master/")
source("EMC.R")

wine <- read.csv("/Users/hyunminhong/Downloads/ToMATo/inputs/wine.txt", header = T, row.names = 1, sep = ",")

sort(unique(wine$quality)) # 3 4 5 6 7 8 (ground truth)

# the ground truth labels
ground_truth = wine$quality

wine <- scale(wine[,-12])

#write.table(wine, file = "wine_w_o_labels.txt", sep = " ",
#            row.names = FALSE, col.names = FALSE)
            


#################################################################
############ EMC ################################################
#################################################################

# Enhanced mode clustering
wine_EMC <- EMC(wine)


# SC-plot
plot(wine_EMC$SC.plot, type = "p", lwd = 2,
     xlim = c(0,20), ylim = c(0,800),
     xlab = "Index of ordered cluster",
     ylab = "Size of cluster",
     main = "SC-plot (Wine Quality)",
)
abline(h = wine_EMC$size.threshold, lwd = 2, col = "purple") # results that there are 4 clusters
# used denoising threshold is 21.97
legend("topright", expression((n*log(n)/20)^{frac(d,d+6)}), col="purple", lwd=8, cex=1)


# plot clusters
n_emc <- 4 # the number of clusters
palette_emc <- rainbow(n_emc) # generate n distinct colors to plot the clusters
palette_emc = palette_emc[wine_EMC$labels] # assign each point to the color where colors represent the clusters

plot(wine[,2:3], pch = 16, cex = 0.5, col = palette_emc, 
     xlab = "", ylab = "", xaxt="n", yaxt="n", main = "2d Projection Wine Quality Clustering (EMC)") # plot clusters (2d projection)

pca_emc = prcomp(wine)

autoplot(pca_emc, colour = palette_emc)

palette <- rainbow(length(unique(ground_truth)))
palette[1] <- "blue"
palette[2] <- "lightskyblue"
palette[3] <- "red"
palette[4] <- "orange"
palette[5] <- "lawngreen" 
palette[6] <- "purple"



plot(wine_EMC, pch=19, cex=0.3,
     xlab="", ylab="", main="Color by Wine Quality", 
     xaxt="n", yaxt="n", txt.pos=2, col=palette[ground_truth])

sort(unique(wine$quality))

legend("topleft", c("3","4","5","6","7","8"), col=palette[c(1,2,5,4,6,3)], pch=rep(19,6), cex=1)


# compute the adjusted Rand index
adj.rand.index(ground_truth, wine_EMC$labels) # results in 0.07330196


# confusion matrix
confusion_mat_emc <- table(ground_truth, wine_EMC$labels)
row.names(confusion_mat_emc) <- c(3,4,5,6,7,8)
colnames(confusion_mat_emc) <- c(1:nrow(wine_EMC$modes))
confusion_mat_emc

#################################################################
############ ToMATo #############################################
#################################################################

ToMATo_labels <- read.csv("~/Downloads/ToMATo/clusters.txt", header = F, sep = " ")
ToMATo_labels <- as.numeric(unlist(ToMATo_labels))

# plot clusters
n <- 7 # the number of clusters
palette_tom <- rainbow(n) # generate n distinct colors to plot the clusters
palette_tom = palette_tom[ToMATo_labels] # assign each point to the color where colors represent the clusters

plot(wine[,2:3], pch = 16, cex = 0.5, col = palette_tom, 
     xlab = "", ylab = "", xaxt="n", yaxt="n", main = "2d Projection Wine Quality Clustering (ToMATo)") # plot clusters (2d projection)

# compute the adjusted Rand index
adj.rand.index(ground_truth, ToMATo_labels) # results in 0.05554523

# confusion matrix
confusion_mat_tom <- table(ground_truth, ToMATo_labels)
row.names(confusion_mat_tom) <- c(3,4,5,6,7,8)
colnames(confusion_mat_tom) <- c(1:max(na.omit(ToMATo_labels)))
confusion_mat_tom

#################################################################
############ k-means ############################################
#################################################################

wine_kmean <- kmeans(wine, 6)

# compute the adjusted Rand index
adj.rand.index(ground_truth, wine_kmean$cluster) # results in 0.05159205

