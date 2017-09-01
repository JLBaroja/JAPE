rm(list=ls())
setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')
# setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')

birds <- c('p004','p054','p138','p510','p530','p736')
# sessions <- 1:70

bb <- birds[5]
time_definition <- 1

# ss <- 25
sss <- 1:65
cum_list <- vector(mode='list')
c_ss <- 0
for(ss in sss){
  c_ss <- c_ss+1
  data_file <- paste(bb,'s',sprintf('%02d',ss),'.csv',sep='')
  if(data_file%in%dir()){
    df <- read.csv(data_file)}
  time <- seq(0,max(df$session_time_sec),by=time_definition)
  cum_resp_right <- NULL
  cum_resp_left <- NULL
  cum_reinf_right <- NULL
  cum_reinf_left <- NULL
  for(tt in time){
    cum_resp_right <- append(cum_resp_right,sum(df$session_time_sec<=tt&df$event=='response_right_key'))
    cum_resp_left <- append(cum_resp_left,sum(df$session_time_sec<=tt&df$event=='response_left_key'))
    cum_reinf_right <- append(cum_reinf_right,sum(df$session_time_sec<=tt&df$event=='feeder_on_right'))
    cum_reinf_left <- append(cum_reinf_left,sum(df$session_time_sec<=tt&df$event=='feeder_on_left'))
  }
  cum_list[[c_ss]] <- data.frame(ss,
                                 time,
                                 cum_resp_left,
                                 cum_resp_right,
                                 cum_reinf_left,
                                 cum_reinf_right)
  print(paste(bb,ss))
}

dev.off()
x11(width=10,height=10)
plot(NULL,xlim=c(0,250000),ylim=c(0,250000))
abline(0,1,lty='dashed')
total_resp_right <- 0
total_resp_left <- 0
for(ll in 1:length(cum_list)){
  resp_right <- cum_list[[ll]]$cum_resp_right+total_resp_right
  resp_left <- cum_list[[ll]]$cum_resp_left+total_resp_left
  points(resp_right,
         resp_left,type='l')
  total_resp_right <- max(resp_right)
  total_resp_left <- max(resp_left)
  points(total_resp_right,total_resp_left,pch=4,col='red')
}

     # xlim=c(0,max(c(max(cum_resp_left),max(cum_resp_right)))),
     # ylim=c(0,max(c(max(cum_resp_left),max(cum_resp_right)))))
