############################################
## ---------------- ERGMs --------------- ##
############################################
library(statnet)
library(dplyr)
library(devtools)
library(lubridate)

# Upload the networks
load("~/output/nwlist.RData")

# Fit the ERGMs
## ---------------- M0 Baseline---------------- ##
m0 <- list()
for(nw_i in 2:48){
  nw <- nwlist[[nw_i]]
  nw_past <- nwlist[[nw_i-1]]
  m <- ergm(nw ~ edges 
            + nodeicov('inf')
            + nodeicov('pol') 
            + nodematch('OT_BIG', diff=T) 
            + edgecov(nw_past)
            ,control = control.ergm(seed = 160222, MCMC.samplesize = 10000))
  m0[[(nw_i)]] <- m
}
m0 <- m0[c(2:48)]
names(m0) <- c(2:48)
save(m0, file="output/final/m0.RData") 

## ---------------- M1  ---------------- ##
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
            + nodeicov('all_deg')
            ,control = control.ergm(seed = 160222, MCMC.samplesize = 10000))
  m1[[(nw_i)]] <- m
}
m1 <- m1[c(2:48)]
names(m1) <- c(2:48)
save(m1, file="output/final/m1.RData")

## ---------------- M2 ---------------- ##
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
            + nodeicov('inf_deg')
            ,control = control.ergm(seed = 160222, MCMC.samplesize = 10000))
  m2[[(nw_i)]] <- m
}
m2 <- m2[c(2:48)]
names(m2) <- c(2:48)
save(m2, file="output/final/m2.RData")

## ---------------- M3 ---------------- ##
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
            + nodeicov('pol_deg')
            ,control = control.ergm(seed = 160222, MCMC.samplesize = 10000))
  m3[[(nw_i)]] <- m
}
m3 <- m3[c(2:48)]
names(m3) <- c(2:48)
save(m3, file="output/final/m3.RData")



