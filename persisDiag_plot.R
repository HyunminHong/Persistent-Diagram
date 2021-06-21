setwd("~/Downloads/ToMATo/")

library(grDevices)

diagram <- read.csv("diagram.txt", header = F, sep = " ")

local_minmax = function(diagram) {
    ## min_birth = interval min; max_birth = interval max
    # corresponds to birth level
    unique_val = unique(diagram[,1])
    min_birth = min(unique_val)
    max_birth = max(unique_val)
    
    ## min_death = non-infinity min of range; max_death = range max
    # corresponds to death level
    fin_entry = which(is.finite(diagram[,2]))
    min_death = min(diagram[fin_entry, 2])
    max_death = max(diagram[fin_entry,2])
    
    return(c(min_death, max_death, min_birth, max_birth))
}

replace_infinity = function(diagram, inf_delta = 0.618) {
    # infinite row entry in birth
    inf_birth = which(is.infinite(diagram[,1]))
    # infinite row entry in death
    inf_death = which(is.infinite(diagram[,2]))
    
    # find local min and max
    min_death = local_minmax(diagram)[1]
    max_death = local_minmax(diagram)[2]
    min_birth = local_minmax(diagram)[3]
    max_birth = local_minmax(diagram)[4]
    
    # replace infinity values with more readable value
    delta = (min_death - max_birth) * inf_delta
    
    # if all entries of birth are finite, return the original
    diagram[inf_death, 2] = delta
    
    return(diagram)
}

display_diagram = function(diagram, tau = NaN, inf_delta = 0.618, sp = 0.1, alpha = 0) {
    # convert infinity values to plotable values
    diagram = replace_infinity(diagram, inf_delta)
    
    # find local min and max
    min_death = local_minmax(diagram)[1]
    max_death = local_minmax(diagram)[2]
    min_birth = local_minmax(diagram)[3]
    max_birth = local_minmax(diagram)[4]
    
    # increasing sp yields wider plot and vice versa
    step_size = abs((min_death - max_birth) * sp)
    
    plot(diagram[,1], diagram[,2], pch = 19, cex = 0.6, col = "blue",
         xlim = c(min_birth - step_size, max_birth + step_size), 
         ylim = c(min_death, max_death + step_size),
         xlab = "birth level", ylab = "death level", 
         main = "Persistence Diagram")
    
    abline(a = 0, b = 1, lty = 1, lwd = 0.3)
    abline(h=0, v=0, lty = 2, lwd = 0.3)
    abline(h = min(diagram[,2]), lty = 8, lwd = 1, col = adjustcolor("red", alpha.f = 0.5))
    polygon(x = c(min(diagram[,2]) - 10*step_size, max(diagram[,2]) + 10*step_size, min(diagram[,2]) - 10*step_size),
            y = c(min(diagram[,2]) - 10*step_size, max(diagram[,2]) + 10*step_size, max(diagram[,2]) + 10*step_size),
            col = "grey", 
            border = adjustcolor("black", alpha.f = 0.5))
    
    if (!is.nan(tau)) {
        clip(x1 = tau, x2 = max(diagram[,1]), y1 = 0, y2 = max_death + step_size + alpha)
        abline(a = -tau, b = 1, lty = 8, lwd = 1)
        clip(x1 = min(diagram)*10, x2 = max(diagram), y1 = min(diagram)*10, y2 = 0)
        abline(v = tau, lty = 8, lwd = 1)
    }
}
# display_diagram(diagram, tau = 1.4, alpha = 0.2) # alpha adjusts the range of the diagonal line
