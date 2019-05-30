rm(list=ls())
sesum <- read.csv('~/Documents/Research/JAPE/ConcurrentData/session_summary.csv')
x11(width = 13,height = 1.5)
par(mar=c(3,1,1,1))
plot(NULL,xlim=c(1,180),ylim=c(.5,6.5),axes=F)
axis(1,at=seq(0,180,15))
cnt_bb <- 0
for(bb in unique(sesum$bird)){
  cnt_bb <- cnt_bb+1
  cnt_ss <- 0
  for(ss in unique(sesum$session)){
    cnt_ss <- cnt_ss+1
    color <- '#0000dd88'
    if(subset(sesum,bird==bb&session==ss)$dynamic_env==T){
      color <- '#dd000088'
    }
    points(cnt_ss,cnt_bb,pch=21,bg=color)
  }
}
