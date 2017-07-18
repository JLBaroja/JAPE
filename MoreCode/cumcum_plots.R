rm(list=ls())
crf <- read.csv('cum_responses_full.csv',stringsAsFactors = F)

birds <- sort(unique(crf$bird))

x11(width=8,height=8)
par(mar=rep(2,4))
plot(NULL,xlim=c(0,60000),ylim=c(0,60000))
abline(0,1,lty='dashed')

# bb <- birds[4]
for(bb in birds){
  ld <- subset(crf,bird==bb)
  se <- ld[which(ld$event=='session_end'),]

  par(plt=c(0.1,.9,0.1,.9),new=T)
  plot(0,col='magenta',pch='4',cex=2,
       xlim=c(0,max(c(max(ld$cum_resp_left),max(ld$cum_resp_right)))),
       ylim=c(0,max(c(max(ld$cum_resp_left),max(ld$cum_resp_right)))))
  col_sess <- rep('#0000ee77',nrow(se))
  col_sess[which(se$session=='s15')] <- '#0000eeee'
  lwd_sess <- rep(0.5,nrow(se))
  lwd_sess[which(se$session=='s15')] <- 2
  points(se$cum_resp_right,se$cum_resp_left,
         pch=1,cex=1.5,lwd=lwd_sess,col=col_sess)
  points(se$cum_resp_right,se$cum_resp_left,
         pch=3,cex=3,lwd=lwd_sess,col=col_sess)
  points(ld$cum_resp_right,ld$cum_resp_left,
         lwd=2.5,type='l')
  text(se$cum_resp_right,se$cum_resp_left,
       se$session,cex=1,font=1,adj=c(2),srt=-45,col='#888888')
}
