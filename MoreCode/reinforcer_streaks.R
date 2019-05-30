rm(list=ls())
load('~/Documents/Research/JAPE/ConcurrentData/ReinforcerAnalysis/full_reinforcers.RData')
load('~/Documents/Research/JAPE/ConcurrentData/ResponseAnalysis/full_responses.RData')
sesum <- read.csv('~/Documents/Research/JAPE/ConcurrentData/session_summary.csv')

birds <- sort(unique(full_reinforcers$bird))

# ss <- 's157'
bb <- birds[1]

# try(dev.off())
# x11(width = 11,height = 8)
pdf(file='~/Documents/Research/JAPE/DatArt/roads.pdf',width = 12,height = 6)
col_lever <- c('#0088cc','#dd4400')
par(bg='#000000',fg='#aaaaaa',col.axis='#999999',
    mar=rep(2,4),oma=c(3,4,2,0))
layout(matrix(1:12,ncol=6))
for(bb in birds){
  for(lv in sort(unique(full_reinforcers$lever))){
    plot(NULL,xlim=c(0,5),ylim=c(-80,80))
    
    for(ss in unique(sesum$session)[1:180]){
      rf <- subset(full_reinforcers,bird==bb&session==ss)
      rs <- subset(full_responses,bird==bb&session==ss)
      zero_coords <- c(0,
                       sum(rs$lever[rs$time<=rf$time[1]]=='left')-
                         sum(rs$lever[rs$time<=rf$time[1]]=='right'))
      points(zero_coords[1],zero_coords[2],col='#00dd11')
      prev_coords <- zero_coords
      for(rr in 1:(nrow(rf)-1)){
        curr_coords <- c(rf$streak_length[rr]+rnorm(1,sd=0),
                         sum(rs$lever[(rs$time>rf$time[rr])&(rs$time<=rf$time[(rr+1)])]=='left')-
                           sum(rs$lever[(rs$time>rf$time[rr])&(rs$time<=rf$time[(rr+1)])]=='right'))
        if(rf$lever[rr]==lv){
          lines(c(prev_coords[1],curr_coords[1]),
                c(prev_coords[2],curr_coords[2]),
                col=paste(col_lever[(rf$lever[rr]=='left')+1],'10',sep=''))
          if(rf$streak_length[rr+1]==1){
            prev_coords <- zero_coords
          }
          else{
            prev_coords <- curr_coords
          }
        }
      }
      print(c(bb,ss,lv))
    }
    abline(h=0,lty='dashed',col='#000000')
  }
}
dev.off()
