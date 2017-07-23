# rm(list=ls())
# crf <- read.csv('cum_responses_full.csv',stringsAsFactors = F)
# crf <- cum_responses_full
# birds <- sort(unique(crf$bird))
birds <- c('p004',
           'p054',
           'p138',
           'p510',
           'p530',
           'p736')

x11(width=8,height=8)
# par(mar=rep(2,4))
# plot(NULL,xlim=c(0,60000),ylim=c(0,60000))
# abline(0,1,lty='dashed')

# bb <- birds[4]
for(bb in birds){
  # ld <- subset(crf,bird==bb)
  ld <- read.csv(paste('cum_responses_',bb,'.csv',sep=''))
  se <- ld[which(ld$event=='session_end'),]
  
  par(plt=c(0.1,.9,0.1,.9),new=T)
  plot(NULL,type='n',
       axes=F,
       # xlim=c(0,1.1*max(c(max(ld$cum_resp_left),max(ld$cum_resp_right)))),
       # ylim=c(0,1.1*max(c(max(ld$cum_resp_left),max(ld$cum_resp_right)))))
       xlim=c(0,70000),ylim=c(0,70000));abline(0,1,lty='dashed')
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
  points(tail(ld$cum_resp_right,1),
         tail(ld$cum_resp_left,1),
         lwd=2.5,cex=2)
  text(se$cum_resp_right,se$cum_resp_left,
       se$session,cex=1,font=1,adj=c(2),srt=-45,col='#888888')
  text(tail(ld$cum_resp_right,1),
       tail(ld$cum_resp_left,1),
       bb,cex=1.5,font=1,srt=45,adj=-.5,col='#000000')
}
axis(1);axis(2)


bb <- 'p530'
ld <- read.csv(paste('cum_responses_',bb,'.csv',sep=''))
sessions <- unique(ld$session)
plot(NULL,type='n',
     axes=F,
     # xlim=c(0,1.1*max(c(max(ld$cum_resp_left),max(ld$cum_resp_right)))),
     # ylim=c(0,1.1*max(c(max(ld$cum_resp_left),max(ld$cum_resp_right)))))
     xlim=c(0,200),ylim=c(0,200));abline(0,1,lty='dashed')
for(ss in sessions){
  # ss <- sessions[16]
  sd <- subset(ld,session==ss)
  tail(sd)
  tm_brks <- seq(0,max(sd$session_time_sec),1)
  cm_rsp_left <- NULL
  cm_rsp_right <- NULL
  for(tt in tm_brks){
    cm_rsp_left <- append(cm_rsp_left,sum(sd$event=='response_left_key'&sd$session_time_sec<=tt))
    cm_rsp_right <- append(cm_rsp_right,sum(sd$event=='response_right_key'&sd$session_time_sec<=tt))
  }
  points(cm_rsp_right,cm_rsp_left,type='l',lwd=2)
  text(tail(cm_rsp_right,1),tail(cm_rsp_left,1),ss,srt=45,cex=2,col='#00000044')
}
