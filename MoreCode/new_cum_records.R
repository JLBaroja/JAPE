rm(list=ls())
# setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')

birds <- c('p004','p054','p138','p510','p530','p736')
# sessions <- 1:70

time_definition <- 1

# bb <- birds[5]
# ss <- 25

sss <- 1:74
for(bb in birds){
  cum_list <- vector(mode='list')
  c_ss <- 0
  setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')
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
      cum_resp_right <- append(cum_resp_right,
                               sum(df$session_time_sec<=tt&df$event=='response_right_key'))
      cum_resp_left <- append(cum_resp_left,
                              sum(df$session_time_sec<=tt&df$event=='response_left_key'))
      cum_reinf_right <- append(cum_reinf_right,
                                sum(df$session_time_sec<=tt&df$event=='feeder_on_right'))
      cum_reinf_left <- append(cum_reinf_left,
                               sum(df$session_time_sec<=tt&df$event=='feeder_on_left'))
    }
    cum_list[[c_ss]] <- data.frame(bird=bb,
                                   session=ss,
                                   time,
                                   cum_resp_left,
                                   cum_resp_right,
                                   cum_reinf_left,
                                   cum_reinf_right,
                                   med_file=unique(df$med_notation_file))
    print(paste(bb,ss,data_file))
  }
  setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
  save(cum_list,file=paste(bb,'cum_resp_reinf_list.RData',sep='_'))
}





rm(list=ls())
dev.off()
x11(width=20,height=4)


setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
birds <- c('p004','p054','p138','p510','p530','p736')

layout(matrix(1:6,ncol=6))
# bb <- birds[1]
for(bb in birds){
plot(NULL,xlim=c(0,10000),ylim=c(0,10000))
abline(0,1,lty='dashed')
  load(paste(bb,'_cum_resp_reinf_list.RData',sep=''))
  
  total_resp_right <- 0
  total_resp_left <- 0
  for(ll in 1:length(cum_list)){
    # resp_right <- cum_list[[ll]]$cum_resp_right+total_resp_right
    # resp_left <- cum_list[[ll]]$cum_resp_left+total_resp_left
    resp_right <- cum_list[[ll]]$cum_reinf_right+total_resp_right
    resp_left <- cum_list[[ll]]$cum_reinf_left+total_resp_left
    points(resp_right,
           resp_left,type='l')
    total_resp_right <- max(resp_right)
    total_resp_left <- max(resp_left)
    pt_col <- 'red'
    if(unique(cum_list[[ll]]$med_file)=='japede_L_30_30_30_R_90_90_90'){
      pt_col <- 'blue'
    }
    else if(unique(cum_list[[ll]]$med_file)=='japede_L_90_90_90_R_30_30_30'){
      pt_col <- 'lightblue'
    }
    points(total_resp_right,total_resp_left,pch=4,col=pt_col)
  }
  
}

# xlim=c(0,max(c(max(cum_resp_left),max(cum_resp_right)))),
# ylim=c(0,max(c(max(cum_resp_left),max(cum_resp_right)))))



















