rm(list=ls())
load('~/Documents/Research/JAPE/ConcurrentData/ReinforcerAnalysis/full_reinforcers.RData')
source('~/Documents/Research/JAPE/MoreCode/plotting_functions.R')

try(dev.off())
x11(width=10,height=4)
layout(matrix(1:12,ncol=4))

single_reinf <- 0
for(schdl in sort(unique(full_reinforcers$local_VI))[1:11]){
  # schdl <- sort(unique(full_reinforcers$local_VI))[1]
  sch_dat <- subset(full_reinforcers,local_VI==schdl)
  sch_baits <- NULL
  plot(NULL,xlim=c(0,32),ylim=c(0,.06))
  for(bb in unique(sch_dat$bird)){
    # bb <- 'p736'
    bb_baits <- NULL
    bb_dat <- subset(sch_dat,bird==bb)
    for(ss in unique(bb_dat$session)){
      # ss <- 's158'
      ss_dat <- subset(bb_dat,session==ss)
      for(ll in unique(ss_dat$lever)){
        # ll <- 'left'
        ll_dat <- subset(ss_dat,lever==ll)
        if(nrow(ll_dat)>1){
          bait_times <- ll_dat$scheduled_since[2:nrow(ll_dat)]-ll_dat$time[1:(nrow(ll_dat)-1)]
          bb_baits <- append(bb_baits,bait_times)
        }
        else{
          single_reinf <- single_reinf+1
        }
        # print(c(length(unique(ss_dat$lever)),bb,ss))
      }
    }
    sch_baits <- append(sch_baits,bb_baits)
    hist(bb_baits,breaks=seq(0,5000,1),add=T,freq=F)
  }
  hist(sch_baits,breaks=seq(0,5000,1),add=T,freq=F,col='#0055dd44')
  text(30,0.05,length(sch_baits),col='red',cex=1.5,adj=1)
  text(30,0.04,schdl,col='red',cex=1.5,adj=1)
}
