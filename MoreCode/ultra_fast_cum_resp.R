
setwd('~/Documents/Research/JAPE/ConcurrentData/')
time_vector <- seq(0,3600,1)
layout(matrix(1:6,ncol=3))
for(bird in c('p004','p138','p054','p510','p530','p736')){
  # bird <- 'p510'
  sessions <- paste('s',50:52,sep='')
  if(bird=='p054'){sessions <- paste('s',49:51,sep='')}
  # if(bird=='p054'){sessions <- paste('s',44:48,sep='')}
  cum_resp_right <- 0
  cum_resp_left <- 0
  sess_cords_lft <- NULL
  sess_cords_rght <- NULL
  for(ss in sessions){
    sd <- read.csv(paste(bird,ss,'.csv',sep=''))
    cr_left <- NULL
    cr_right <- NULL
    for(tv in 1:length(time_vector)){
      cr_left[tv] <- sum(sd$event=='response_left_key'&sd$session_time_sec<=time_vector[tv])
      cr_right[tv] <- sum(sd$event=='response_right_key'&sd$session_time_sec<=time_vector[tv])
      print(c(bird,ss,time_vector[tv]))
    }
    cum_resp_left <- append(cum_resp_left,cr_left+max(cum_resp_left))
    cum_resp_right <- append(cum_resp_right,cr_right+max(cum_resp_right))
    sess_cords_lft <- append(sess_cords_lft,max(cum_resp_left))
    sess_cords_rght <- append(sess_cords_rght,max(cum_resp_right))
  }
  plot(cum_resp_left,cum_resp_right,
       xlim=c(0,max(max(cum_resp_left),max(cum_resp_right))),
       ylim=c(0,max(max(cum_resp_left),max(cum_resp_right))),
       type='l',main=paste(bird,sessions));abline(0,1,lty='dashed')
  points(sess_cords_lft,sess_cords_rght,pch=4,col='red')
}


setwd('~/Documents/Research/JAPE/ConcurrentData/Raw MED files/')
dir()
for(bb in c('p004','p138','p054','p510','p530','p736')){
  # bb <- 'p004'
  sss <- paste('s',10:51,sep='') 
  for(ss in sss){
    file <- paste(bb,'_',ss,'_japede',sep='')
    rl <- readLines(file)
    print(c(bb,ss,rl[14]))
  }
}

