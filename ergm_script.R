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
save(m1, file="output/v9/m1.RData")

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
save(m4, file="output/v9/m4.RData")

