rm(list=ls())
# setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')
setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')

birds <- c('p004','p054','p138','p510','p530','p736')
# sessions <- 1:70

bb <- birds[4]
ss <- 25
time_definition <- 1
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
data.frame(time,cum_resp_left,cum_resp_right,cum_reinf_left,cum_reinf_right)
plot(cum_resp_right,cum_resp_left,
     type='l',
     xlim=c(0,max(c(max(cum_resp_left),max(cum_resp_right)))),
     ylim=c(0,max(c(max(cum_resp_left),max(cum_resp_right)))))
