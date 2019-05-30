# rm(list=ls())
# load('~/Documents/Research/JAPE/ConcurrentData/ReinforcerAnalysis/full_reinforcers.RData')
# load('~/Documents/Research/JAPE/ConcurrentData/ResponseAnalysis/full_responses.RData')
# # source('~/Documents/Research/JAPE/MoreCode/plotting_functions.R')
# # sesum <- read.csv('~/Documents/Research/JAPE/ConcurrentData/session_summary.csv')
# 
# bb <- unique(full_reinforcers$bird)[4]
# lvr <- 'left'

plot_pulses <- function(bb,lvr){
  plot(NULL,xlim=c(0,5),ylim=c(-50,50),axes=F)
  
  for(ss in unique(full_reinforcers$session)){
    # ss <- unique(full_reinforcers$session)[100]
    rf <- subset(full_reinforcers,bird==bb&session==ss)
    rs <- subset(full_responses,bird==bb&session==ss)
    
    row_streaks <- 1
    col_streaks <- 1
    n_streaks <- sum(rf$streak_length[1:(nrow(rf)-1)]==1)
    lever <- vector(mode='character',length = n_streaks)
    left_m_right <- array(dim=c(n_streaks,
                                100))
    colnames(left_m_right) <- paste('strk',0:99,sep='')
    for(row in 1:(nrow(rf)-1)){
      left_m_right[row_streaks,col_streaks] <- (rf$cum_resp_left[(row+1)]-rf$cum_resp_left[row])-(rf$cum_resp_right[(row+1)]-rf$cum_resp_right[row])
      
      if(rf$streak_length[row+1]==1){
        lever[row_streaks] <- as.character(rf$lever[row])
        row_streaks <- row_streaks+1
        col_streaks <- 2
      }
      else{
        col_streaks <- col_streaks+1
      }
      
    }
    
    lever_strk <- left_m_right[which(lever==lvr),]
    for(lr in 1:nrow(lever_strk)){
      lines(0:(ncol(lever_strk)-1),c(lever_strk[1,1],lever_strk[lr,2:100]),
            col=paste(brd_cl[which(unique(sessum$bird)==bb)],'04',sep=''))
    }
    
  }
  abline(h=0,lty='dashed',col='#ee0000')
  axis(1,cex.axis=.7,tck=-.02)
  axis(2,cex.axis=.7,tck=-.02,padj=-.5)
}