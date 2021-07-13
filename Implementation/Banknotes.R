library(nonet)
library(pdfCluster)
setwd("/Users/hyunminhong/Documents/Programmings/R_R studio/Bachelor Thesis/EMC-master/")
source("EMC.R")

data(banknote_authentication)

# write.table(banknotes, file = "banknotes_w_o_labels.txt", sep = " ", 
#            row.names = FALSE, col.names = FALSE)

# the ground truth labels
ground_truth = banknote_authentication$class

banknotes <- scale(banknote_authentication[,1:4])

#################################################################
############ EMC ################################################
#################################################################

# Enhanced mode clustering
banknotes_EMC <- EMC(banknotes)


# SC-plot
plot(banknotes_EMC$SC.plot, type = "p", lwd = 2,
     xlim = c(0,15), ylim = c(0,600),
     xlab = "Index of ordered cluster",
     ylab = "Size of cluster",
     main = "SC-plot (Bank Authentication)",
)
abline(h = banknotes_EMC$size.threshold+10, lwd = 2, col = "purple") # results that there are 5 clusters
                                                                     # used denoising threshold is 21.97
legend("topright", expression((n*log(n)/20)^{frac(d,d+6)}+10), col="purple", lwd=8, cex=1)


# plot clusters
n_emc <- 5 # the number of clusters
palette_emc <- rainbow(n_emc) # generate n distinct colors to plot the clusters
palette_emc = palette_emc[banknotes_EMC$labels] # assign each point to the color where colors represent the clusters

plot(banknotes[,1:2], pch = 16, cex = 0.8, col = palette_emc, 
     xlab = "", ylab = "", xaxt="n", yaxt="n", main = "2d Projection Bank Authentication Clustering (EMC)") # plot clusters (2d projection)

palette <- rainbow(max(ground_truth))

plot(banknotes_EMC, pch=20, cex=0.7,
     xlab="", ylab="", main="Color by Real/Forgery", 
     xaxt="n", yaxt="n", txt.pos=4, col=palette[ground_truth])

legend("topleft", c("Real", "Forgery"), col=palette, pch=rep(19,2), cex=1)


# compute the adjusted Rand index
adj.rand.index(ground_truth, banknotes_EMC$labels) # results in 0.563353


# confusion matrix
confusion_mat_emc <- table(ground_truth, banknotes_EMC$labels)
row.names(confusion_mat_emc) <- c("Real", "Forgery")
colnames(confusion_mat_emc) <- c(1:nrow(banknotes_EMC$modes))
confusion_mat_emc

#################################################################
############ ToMATo #############################################
#################################################################

ToMATo_labels <- read.csv("~/Downloads/ToMATo/clusters.txt", header = F, sep = " ")
ToMATo_labels <- as.numeric(unlist(ToMATo_labels))

# plot clusters
n <- 4 # the number of clusters
palette_tom <- rainbow(n) # generate n distinct colors to plot the clusters
palette_tom = palette_tom[ToMATo_labels] # assign each point to the color where colors represent the clusters

plot(banknotes[,1:2], pch = 16, cex = 0.8, col = palette_tom, 
     xlab = "", ylab = "", xaxt="n", yaxt="n", main = "2d Projection Bank Authentication Clustering (ToMATo)") # plot clusters (2d projection)

# compute the adjusted Rand index
adj.rand.index(ToMATo_labels, ground_truth) # results in 0.5714921

# confusion matrix
confusion_mat_tom <- table(ground_truth, ToMATo_labels)
row.names(confusion_mat_tom) <- c("Real", "Forgery")
colnames(confusion_mat_tom) <- c(1:max(na.omit(ToMATo_labels)))
confusion_mat_tom

table(ToMATo_labels, ground_truth)

#################################################################
############ k-means ############################################
#################################################################

bank_kmean <- kmeans(banknotes, 2)

# compute the adjusted Rand index
adj.rand.index(ground_truth, bank_kmean$cluster) # results in 0.01321583




