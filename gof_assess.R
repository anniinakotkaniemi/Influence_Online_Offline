# Function for running GOF on list of results
assess_gof <- function(models, networks, nsim = 5000, seed = 70492, verbose = T){
  gofits <- list()
  for(i in 1:length(models)){
    if(verbose){cat(i,'...', sep = '')}
    nw <<- networks[[i+1]]
    nw_past <<- networks[[i]]
    gofits[[i]] <- list(simulate(models[[i]], 
                        nsim = nsim, output = 'stats', seed = seed), 
                        summary(models[[i]]$formula))
    rm(nw, nw_past, pos = ".GlobalEnv")
  }
  gofits
}

# Simple plotting function for GOF
plot_gofits <- function(gofits, labels = NULL){ 
  vars <- names(gofits[[1]][[2]])
  labs <- if(is.null(labels)){vars} else {labels}
  nsim <- nrow(gofits[[1]][[1]])
  for(i in vars){
    fitstats <- matrix(unlist(lapply(gofits, function(x){
      if(i %in% names(x[[2]])){
        c(x[[2]][i], x[[1]][,i])
      } else {rep(NA, nsim + 1)}
    })), ncol = length(gofits))
    par(mar = c(2, 2, 1.5, 0.5)) 
    boxplot(fitstats[-1,], range = 0, main = labs[which(vars == i)])
    points(fitstats[1,], col = 2, pch = 19, cex = 0.5)
    lines(fitstats[1,], col = 2)
  }
}
plotlabels <- c("Edges", "Reputational influence", "Government influence", 
                "Homophily: Governmental", "Homophily: Scientific", "Homophily: Business", "Homophily: Civil society",
                "Past edge", "Reciprocity", "GWESP", "GWI Degree", "Popularity effect")
gofits <- assess_gof(m1, nwlist, nsim = 10000)
plot_gofits(gofits, plotlabels)

# Print degeneracy statistics
pdf("output/pdf/gof_plots.pdf", width = 8, height = 11)
#jpeg("output/jpeg/gof_plots.jpg", width = 8, height = 11, res = 300)
par(mfrow = c(4, 3))
plot_gofits(gofits)
dev.off()

# Function for M1 indegree gof statistics 
# Function for extracting idegree-gof
assess_ideg <- function(models, networks, nsim, seed= 310523){
  gof_ideg <- list()
  for (i in 1:length(models)){
    nw <<- networks[[i+1]]
    nw_past <<- networks[[i]]
    gof_ideg[[i]] <- gof(models[[i]], GOF = models~idegree-model
                              ,control= control.gof.ergm(nsim = nsim, seed = seed))
    rm(nw, nw_past, pos = ".GlobalEnv")
  }
  gof_ideg
}
m1_ideg <- assess_ideg(m1, nwlist, 100)
# Test to omit the model statistic term with one network
nw_past <- nwlist[[1]]
goftest <- gof(m1[[1]], GOF = m1~idegree- model)
plot(goftest)  
?gof

