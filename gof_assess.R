# Function for running GOF on list of results
assessGof <- function(models, networks, nsim = 5000, seed = 70492, verbose = T){
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
plotGof <- function(gofits, labels = NULL){ 
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
gofits <- assessGof(m1, nwlist, nsim = 10000)
# Labels to plots
plotlabels <- c("Edges", "Reputational influence", "Government influence", 
                "Homophily: Government", "Homophily: Science", "Homophily: Business", "Homophily: Civil society",
                "Past edge", "Reciprocity", "GWESP", "GWI Degree", "Popularity effect")
plotGof(gofits, plotlabels)

# Print gof degeneracy statistics pdf
pdf("output/pdf/gof_plots.pdf", width = 7.5, height = 10)
par(mfrow = c(4, 3))
plotGof(gofits, plotlabels)
dev.off()
# Print jpeg
jpeg("output/jpeg/gof_plots.jpg", width = 7.5, height = 10, res = 300, units ="in")
par(mfrow = c(4, 3))
plotGof(gofits, plotlabels)
dev.off()

# Function for M1 indegree gof statistics 
# Function for extracting indegree-gof
assessIndeg <- function(models, networks, nsim, seed= 310523){
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
indeg <- assessIndeg(m1, nwlist, 100)

# Plot indeg gof to fit to two pages
# Setting headers does not work
# First page
pdf("output/pdf/indeg1.pdf", width = 8, height = 11)
par(mar = c(2, 2, 0.5, 0.2), mfrow=c(6,4))
for (i in 1:24) {
  plot(indeg[[(i)]], main=NA
     #  , main = paste("Period", i)
       )
 # mtext(paste("Period", i), side = 3, line = -1, outer = TRUE, cex = 1.5)
}
dev.off()
# Second page
pdf("output/pdf/indeg2.pdf", width = 8, height = 11)
par(mar = c(2, 2, 0.5, 0.2), mfrow=c(6,4))
for (i in 25:47) {
  plot(indeg[[(i)]], main=NA
       #  , main = paste("Period", i)
  )
  # mtext(paste("Period", i), side = 3, line = -1, outer = TRUE, cex = 1.5)
}
dev.off()


#################
### AIC and BIC Plotting

# Extract AIC values from the ERGM outputs
aic_m0 <- sapply(m0, function(model) AIC(model))
aic_m1 <- sapply(m1, function(model) AIC(model))
# Extract BIC values from the ERGM outputs
bic_m0 <- sapply(m0, function(model) BIC(model))
bic_m1 <- sapply(m1, function(model) BIC(model))

## AIC Plotting
AICdata <- cbind(c(2:48), aic_m1, aic_m0)
AICdata <- as.data.frame(AICdata)
colnames(AICdata) <- c("Month", "Model 1", "Model 0")
AICmelt <- reshape2::melt(AICdata, id.var='Month')
colnames(AICmelt) <- c("Month", "variable", "AIC")

p7<- ggplot(data = AICmelt, aes(x = Month, y = AIC, col=variable)) +ggtitle('AIC')+theme_bw() +
  scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm) + labs(x = "") +
  geom_line(lwd=1, aes(linetype=variable))+ scale_color_manual(values = c("Model 1" = "#EE6677", "Model 0" = "#4477AA")) 
p7<-p7+theme(legend.position='none', axis.text=element_text(size=16),axis.title=element_text(size=19),
             plot.title=element_text(size=21, face='bold'))
p7
## BIC Plotting
BICdata <- cbind(c(2:48), bic_m1, bic_m0)
BICdata <- as.data.frame(BICdata)
colnames(BICdata) <- c("Month", "Model 1", "Model 0")
BICmelt <- reshape2::melt(BICdata, id.var='Month')
colnames(BICmelt) <- c("Month", "variable", "BIC")

p8<- ggplot(data = BICmelt, aes(x = Month, y = BIC, col=variable)) +ggtitle('BIC')+theme_bw() +
  scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm) + labs(x = "") +
  geom_line(lwd=1, aes(linetype=variable))+ scale_color_manual(values = c("Model 1" = "#EE6677", "Model 0" = "#4477AA")) 
p8<-p8+theme(axis.title=element_text(size=19),axis.text=element_text(size=16), 
             plot.title=element_text(size=21, face='bold'), legend.title=element_blank(),
             legend.position = c(0.85, 0.90), legend.text=element_text(size=14))
p8

## Save as pdf
pdf("output/pdf/aicbic.pdf", width = 14, height = 7)
plots_ALL <- grid.arrange(p7, p8,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2)))
dev.off()

rm(aic_m1, aic_m0, AICmelt, AICdata, bic_m1, bic_m0, BICmelt, BICdata, p7, p8)
