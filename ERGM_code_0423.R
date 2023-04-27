#################################################
## ---------------- v.9 ERGMs --------------- ### 
################################################# 

# Plog mcmc_diagnostics and goodness-of-fit
mcmc_gof_plots <- function(model_list, filename){
  pdf(filename, width = 8.5, height = 11)
  for(i in 1:length(model_list)){mcmc.diagnostics(model_list[[i]]); title(main = paste('Period', (i + 1)))}
  for(i in 1:length(model_list)){plot(gof(model_list[[i]])); title(main = paste('Period', (i + 1)))}
  dev.off()
}

periods <- m2$`23`
nw_past <- nwlist$`22`
pdf("output/v9/mcmc.m4.41.pdf", width = 12, height = 8.5)
par( mfrow= c(3,4) )
mcmc.diagnostics(m4[[41]])
#plot(gof(periods, control = control.gof.ergm(seed = 160222)))
dev.off()

?ergm

## ---------------- M0 Baseline without gwi---------------- ##
m0 <- list()
for(nw_i in 2:48){
  nw <- nwlist[[nw_i]]
  nw_past <- nwlist[[nw_i-1]]
  m <- ergm(nw ~ edges 
            + nodeicov('inf')
            + nodeicov('pol') 
            + nodematch('OT_BIG', diff=T) 
            + edgecov(nw_past)
            ,control = control.ergm(seed = 160222))
  m0[[(nw_i)]] <- m
}
m0 <- m0[c(2:48)]
names(m0) <- c(2:48)
#mcmc_gof_plots(m0, "output/v8/mcmc_gof_m0.pdf")
save(m0, file="output/v9/m0.RData") 

## ---------------- M1 with gwi ---------------- ##
m1 <- list()
for(nw_i in 2:48){
  nw <- nwlist[[nw_i]]
  nw_past <- nwlist[[nw_i-1]]
  m <- ergm(nw ~ edges 
            + nodeicov('inf')
            + nodeicov('pol') 
            + nodematch('OT_BIG', diff=T) 
            + edgecov(nw_past)
            + mutual
            + gwesp(decay=0, fixed=T) 
            + gwidegree(decay=0, fixed=T)
            ,control = control.ergm(seed = 160222))
  m1[[(nw_i)]] <- m
}
m1 <- m1[c(2:48)]
names(m1) <- c(2:48)
#mcmc_gof_plots(m1, "output/v8/mcmc_gof_m1.pdf")
save(m1, file="output/v9/m1.RData")
?control.ergm

## ---------------- M2 alldeg ---------------- ##
m2 <- list()
for(nw_i in 2:48){
  nw <- nwlist[[nw_i]]
  nw_past <- nwlist[[nw_i-1]]
  m <- ergm(nw ~ edges 
            + nodeicov('inf')
            + nodeicov('pol') 
            + nodematch('OT_BIG', diff=T) 
            + edgecov(nw_past)
            + mutual
            + gwesp(decay=0, fixed=T) 
            + gwidegree(decay=0, fixed=T)
            + nodeicov('all_deg')
            ,control = control.ergm(seed = 160222))
  m2[[(nw_i)]] <- m
}
m2 <- m2[c(2:48)]
names(m2) <- c(2:48)
#mcmc_gof_plots(m2, "output/v8/mcmc_gof_m2.pdf")
save(m2, file="output/v9/m2.RData")

## ---------------- M3 infdeg ---------------- ##
m3 <- list()
for(nw_i in 2:48){
  nw <- nwlist[[nw_i]]
  nw_past <- nwlist[[nw_i-1]]
  m <- ergm(nw ~ edges 
            + nodeicov('inf')
            + nodeicov('pol') 
            + nodematch('OT_BIG', diff=T) 
            + edgecov(nw_past)
            + mutual
            + gwesp(decay=0, fixed=T) 
            + gwidegree(decay=0, fixed=T)
            + nodeicov('all_deg')
            + nodeicov('inf_deg')
            ,control = control.ergm(seed = 160222))
  m3[[(nw_i)]] <- m
}
m3 <- m3[c(2:48)]
names(m3) <- c(2:48)
#mcmc_gof_plots(m3, "output/v9/mcmc_gof_m3.pdf")
save(m3, file="output/v9/m3.RData")

## ---------------- M4 poldeg ---------------- ##
m4 <- list()
for(nw_i in 2:48){
  nw <- nwlist[[nw_i]]
  nw_past <- nwlist[[nw_i-1]]
  m <- ergm(nw ~ edges 
            + nodeicov('inf')
            + nodeicov('pol') 
            + nodematch('OT_BIG', diff=T) 
            + edgecov(nw_past)
            + mutual
            + gwesp(decay=0, fixed=T) 
            + gwidegree(decay=0, fixed=T)
            + nodeicov('all_deg')
            + nodeicov('pol_deg')
            ,control = control.ergm(seed = 160222))
  m4[[(nw_i)]] <- m
}
m4 <- m4[c(2:48)]
names(m4) <- c(2:48)
#mcmc_gof_plots(m4, "output/v8/mcmc_gof_m4.pdf")
save(m4, file="output/v9/m4.RData")

m4 <- c(m4.1, m4)

