setwd("/Users/hyunminhong/Documents/Programmings/R_R studio/Bachelor Thesis/EMC-master/")
source("EMC.R")
library(scatterplot3d)
library(pdfCluster)
library(ggfortify)
library(fossil)

# import the olive oil data used in Chen et al
library(freqparcoord)
data(oliveoils)

olive = scale(oliveoils[,3:10]) # extract numeric data 
                                # scale the data so that we can use the simplified Silverman's rule of thumb

# the ground truth labels 
ground_truth = as.numeric(oliveoils$Area)

#################################################################
############ EMC ################################################
#################################################################

# Enhanced mode clustering
olive_EMC = EMC(olive)


# SC-plot
plot(olive_EMC$SC.plot, type = "p", lwd = 2,
     xlim = c(0,20), ylim = c(0,250),
     xlab = "Index of ordered cluster",
     ylab = "Size of cluster",
     main = "SC-plot (Olive Oil)"
)
abline(h = olive_EMC$size.threshold, lwd = 2, col = "purple") # results that there are 7 clusters
legend("topright", expression((n*log(n)/20)^{frac(d,d+6)}), col="purple", lwd=8, cex=1)


# plot clusters
n_emc <- 7 # the number of clusters
palette_emc <- rainbow(n_emc) # generate n distinct colors to plot the clusters
palette_emc = palette_emc[olive_EMC$labels] # assign each point to the color where colors represent the clusters

plot(olive[,1:2], pch = 16, cex = 0.8, col = palette_emc, 
     xlab = "", ylab = "", xaxt="n", yaxt="n", main = "2d Projection Olive Oil Clustering (EMC)") # plot clusters (2d projection)

# 2d projection can be done by PCA
pca_emc = prcomp(olive)
autoplot(pca_emc, colour = palette_emc)

# 3d projection
scatterplot3d(olive[,1:3], pch = 16, type = "p",
              cex.symbols = 0.8, color = palette_emc,
              xlab = "X", ylab = "Y", zlab = "Z") # plot clusters 

palette <- rainbow(max(ground_truth))
palette[3] <- "brown"
palette[4] <- "forestgreen"

plot(olive_EMC, pch=20, cex=0.7,
     xlab="", ylab="", main="Color by Area", 
     xaxt="n", yaxt="n", txt.pos=4, col=palette[ground_truth])

legend("topright", levels(oliveoils[,1])[1:4], col=palette[1:4], pch=rep(19,9),cex=1)
legend("bottomright", levels(oliveoils[,1])[5:9], col=palette[5:9], pch=rep(19,9),cex=1)

# compute the adjusted Rand index
adj.rand.index(olive_EMC$labels, ground_truth) # results in 0.8260962

# confusion matrix
confusion_mat_emc <- table(ground_truth, olive_EMC$labels)
row.names(confusion_mat_emc) <- levels(oliveoils[,1])
colnames(confusion_mat_emc) <- c(1:nrow(olive_EMC$modes))
confusion_mat_emc

#################################################################
############ ToMATo #############################################
#################################################################

ToMATo_labels <- read.csv("/Users/hyunminhong/Desktop/Bachelor Thesis/Information_TDA/clusters_olive.txt", header = F, sep = " ")
ToMATo_labels <- as.numeric(unlist(ToMATo_labels))

# plot clusters
n <- 8 # the number of clusters
palette_tom <- rainbow(n) # generate n distinct colors to plot the clusters
palette_tom = palette_tom[ToMATo_labels] # assign each point to the color where colors represent the clusters

plot(olive[,1:2], pch = 16, cex = 0.8, col = palette_tom, 
     xlab = "", ylab = "", xaxt="n", yaxt="n", main = "2d Projection Olive Oil Clustering (ToMATo)") # plot clusters (2d projection)

scatterplot3d(olive[,1:3], pch = 16, type = "p",
              cex.symbols = 0.8, color = palette_tom,
              xlab = "X", ylab = "Y", zlab = "Z") # plot clusters 

# compute the adjusted Rand index
adj.rand.index(ToMATo_labels, ground_truth) # results in 0.8100825

# confusion matrix
confusion_mat_tom <- table(ground_truth, ToMATo_labels)
row.names(confusion_mat_tom) <- levels(oliveoils[,1])
colnames(confusion_mat_tom) <- c(1:max(na.omit(ToMATo_labels)))
confusion_mat_tom

table(ToMATo_labels, ground_truth)

#################################################################
############ k-means ############################################
#################################################################

olive_kmean <- kmeans(olive, 9)

# compute the adjusted Rand index
adj.rand.index(ground_truth, olive_kmean$cluster) # results in 0.7871385

