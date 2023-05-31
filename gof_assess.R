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
plot_gofits <- function(gofits){
  vars <- names(gofits[[1]][[2]])
  nsim <- nrow(gofits[[1]][[1]])
  for(i in vars){
    fitstats <- matrix(unlist(lapply(gofits, function(x){
      if(i %in% names(x[[2]])){
        c(x[[2]][i], x[[1]][,i])
      } else {rep(NA, nsim + 1)}
    })), ncol = length(gofits))
    par(mar = c(4, 4, 4, 4)) 
    boxplot(fitstats[-1,], range = 0)
    points(fitstats[1,], col = 2, pch = 19, cex = 0.5)
    lines(fitstats[1,], col = 2)
  }
}

gofits <- assess_gof(m1, nwlist, nsim = 10000)
plot_gofits(gofits)


