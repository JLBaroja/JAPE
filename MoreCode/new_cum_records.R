rm(list=ls())
setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')

birds <- c('p004','p054','p138','p510','p530','p736')
# sessions <- 1:70

bb <- birds[4]
ss <- 25
time_definition <- 1
data_file <- paste(bb,'s',sprintf('%02d',ss),'.csv',sep='')
if(data_file%in%dir()){
  df <- read.csv(data_file)}
times <- seq(0,max(df$session_time_sec),by=time_definition)
cum_resp_right <- NULL
cum_resp_left <- NULL
for(tt in times){
  cum_resp_right <- append(cum_resp_right,
                           sum(df$session_time_sec<=tt&df$event=='response_right_key'))
  cum_resp_left <- append(cum_resp_left,
                          sum(df$session_time_sec<=tt&df$event=='response_left_key'))
}
data.frame(times,cum_resp_left,cum_resp_right)

