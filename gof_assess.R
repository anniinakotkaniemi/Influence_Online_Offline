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

# Test to exclude the model statistic with one network
nw_past <- nwlist[[1]]
goftest <- gof(m1[[1]], GOF = ~idegree -model)
plot(goftest)  
?gof

# Plot (gives both idegree and model statistic for all periods in m1)
for (i in 1:length(m1_ideg)) {
  ideg_plot <- plot(m1_ideg[[i]], main = paste("Period", i))
}

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

p1<- ggplot(data = AICmelt, aes(x = Month, y = AIC, col=variable)) +ggtitle('AIC')+theme_bw() +
  scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm) +
  geom_line(lwd=1, aes(linetype=variable))+ scale_color_manual(values = c("Model 1" = "#EE6677", "Model 0" = "#4477AA")) 
p1<-p1+theme(legend.position='none', axis.text=element_text(size=12),axis.title=element_text(size=19),
             plot.title=element_text(size=21, face='bold'))
p1
## BIC Plotting
BICdata <- cbind(c(2:48), bic_m1, bic_m0)
BICdata <- as.data.frame(BICdata)
colnames(BICdata) <- c("Month", "Model 1", "Model 0")
BICmelt <- reshape2::melt(BICdata, id.var='Month')
colnames(BICmelt) <- c("Month", "variable", "BIC")

custom_labels <- function(x) {
  paste(c("Jan-18", "July-18",
          "Jan-19", "July-19",
          "Jan-20", "July-20",
          "Jan-21", "July-21"), x)
}
p2<- ggplot(data = BICmelt, aes(x = Month, y = BIC, col=variable)) +ggtitle('BIC')+theme_bw() +
  scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm) +
  geom_line(lwd=1, aes(linetype=variable))+ scale_color_manual(values = c("Model 1" = "#EE6677", "Model 0" = "#4477AA")) 
p2<-p2+theme(axis.title=element_text(size=19),axis.text=element_text(size=12), 
             plot.title=element_text(size=21, face='bold'), legend.title=element_blank(),
             legend.position = c(0.85, 0.90), legend.text=element_text(size=14))
p2

## Save as pdf
pdf("output/pdf/aicbic.pdf", width = 14, height = 7)
p1 <- p1 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
p2 <- p2 + scale_x_continuous(breaks=seq(1, 48, 6), labels = labelsm)
plots_ALL <- grid.arrange(p1, p2,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2)))
dev.off()
## Save as jpeg
jpeg("output/jpeg/aicbic.jpg", width = 3500, height = 1700, res=300)
plots_ALL <- grid.arrange(p1, p2,
                          ncol=2,
                          layout_matrix = rbind(c(1, 2)))
dev.off()


