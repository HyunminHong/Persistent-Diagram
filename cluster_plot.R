library(scatterplot3d)

setwd("~/Downloads/ToMATo/")
clusters <- read.csv("clusters.txt", header = F, sep = " ")
clusters = as.numeric(unlist(clusters))

spiral <- read.csv("~/Downloads/ToMATo/inputs/spiral_w_density.txt", header = F, sep = " ")
spiral <- spiral[, c("V4", "V7", "V10")]

scatterplot3d(spiral$V4, spiral$V7, spiral$V10, pch = 16, type = "p",
              cex.symbols = 0.1, color = adjustcolor("blue", alpha.f = 0.5),
              xlim = c(0,2000), ylim = c(0,2000),
              xlab = "X", ylab = "Y", zlab = "Z") # plot spiral data with noise

plot(spiral$V4, spiral$V7, pch = 16, cex = 0.1,
     col = adjustcolor("blue", alpha.f = 0.5),
     xlab = "X", ylab = "Y") # 2d projection

cols = c(adjustcolor("blue", alpha.f = 0.5), adjustcolor("red", alpha.f = 0.5))
cols = cols[clusters]

plot(spiral[,1:2], pch = 16, cex = 0.1, col = cols, 
     xlab = "X", ylab = "Y") # plot clusters (2d projection)

scatterplot3d(spiral[,1:3], pch = 16, type = "p",
              cex.symbols = 0.1, color = cols,
              xlim = c(0,2000), ylim = c(0,2000),
              xlab = "X", ylab = "Y", zlab = "Z") # plot clusters 
