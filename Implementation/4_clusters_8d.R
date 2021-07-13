setwd("/Users/hyunminhong/Documents/Programmings/R_R studio/Bachelor Thesis/EMC-master/")

#' import the functions that will be used
source("EMC.R")

#' 8 dimensional four clusters dataset
#' @param N Sample size per each cluster
#' @param dis Randomness around clusters
#' @param d.add Added dimensions.
#' @return A four clusters dataset with 2+d.add dimensions.
#' @export
four_cluster = function(N=200, dis=0.1, d.add=6){
    ## setting clusters
    C0.1 = c(0,0)
    C0.2 = c(0.5,0)
    C0.3 = c(0,0.5)
    C0.4 = c(0.5,0.5)
    C0 = cbind(rbind(C0.1,C0.2,C0.3,C0.4), matrix(0,nrow=4, ncol=d.add))
    
    ## settting total dimensions
    d= d.add+2
    
    ## adding cluster points
    C = NULL
    for(i in 1:4){
        C = rbind(C, rmvnorm(N,C0[i,], sigma = diag(rep(dis^2,d))))
    }
    return(C)
}

## generate the data from a 4-Gaussian mixture in d = 8
set.seed(4)
fourClusters <- four_cluster()
plot(fourClusters, main = "4 Gaussian mixture", xlab = "X1", ylab = "X2")

## Example. Enhanced mode clustering with 4-Gaussian clusters in d = 8
start_time <- Sys.time()
fourClusters_emc <- EMC(fourClusters)
end_time <- Sys.time()

end_time - start_time

plot(fourClusters_emc$SC.plot, type = "p", lwd = 2,
     xlim = c(0,20), ylim = c(0,200),
     xlab = "Index of ordered cluster",
     ylab = "Size of cluster",
)
abline(h = fourClusters_emc$size.threshold, lwd = 2, col = "purple")
legend("topright", expression((n*log(n)/20)^{frac(d,d+6)}), col="purple", lwd=8, cex=1)
fourClusters_emc$size.threshold