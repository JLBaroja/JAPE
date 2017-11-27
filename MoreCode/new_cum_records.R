# rm(list=ls())
# # setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')
# 
# birds <- c('p004','p054','p138','p510','p530','p736')
# # sessions <- 1:70
# 
# time_definition <- 1
# 
# # bb <- birds[5]
# # ss <- 25
# 
# sss <- 1:85
# for(bb in birds){
#   cum_list <- vector(mode='list')
#   c_ss <- 0
#   setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')
#   # setwd('~/Documents/Research/jape/ConcurrentData/CSV files/')
#   for(ss in sss){
#     c_ss <- c_ss+1
#     data_file <- paste(bb,'s',sprintf('%02d',ss),'.csv',sep='')
#     if(data_file%in%dir()){
#       
#       df <- read.csv(data_file)
#       time <- seq(0,max(df$session_time_sec),by=time_definition)
#       cum_resp_right <- NULL
#       cum_resp_left <- NULL
#       cum_reinf_right <- NULL
#       cum_reinf_left <- NULL
#       for(tt in time){
#         cum_resp_right <- append(cum_resp_right,
#                                  sum(df$session_time_sec<=tt&df$event=='response_right_key'))
#         cum_resp_left <- append(cum_resp_left,
#                                 sum(df$session_time_sec<=tt&df$event=='response_left_key'))
#         cum_reinf_right <- append(cum_reinf_right,
#                                   sum(df$session_time_sec<=tt&df$event=='feeder_on_right'))
#         cum_reinf_left <- append(cum_reinf_left,
#                                  sum(df$session_time_sec<=tt&df$event=='feeder_on_left'))
#       }
#       
#       cum_list[[c_ss]] <- data.frame(bird=bb,
#                                      session=ss,
#                                      time,
#                                      cum_resp_left,
#                                      cum_resp_right,
#                                      cum_reinf_left,
#                                      cum_reinf_right,
#                                      first_cp_left=unique(df$first_cp_left),
#                                      second_cp_left=unique(df$second_cp_left),
#                                      first_cp_right=unique(df$first_cp_right),
#                                      second_cp_right=unique(df$second_cp_right),
#                                      med_data_file=unique(df$data_file),
#                                      csv_data_file=data_file,
#                                      med_notation_file=unique(df$med_notation_file))
#       
#       names(cum_list)[c_ss] <- data_file
#       print(paste(bb,ss,data_file))
#       
#     }
#   }
#   # setwd('~/Documents/Research/jape/ConcurrentData/RData files/')
#   setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
#   save(cum_list,file=paste(bb,'cum_resp_reinf_list.RData',sep='_'))
# }
# 
# 
# 
# 
# 
# # Updating cum_lists
# rm(list=ls())
# 
# get_cum_counts <- function(data_file,time_definition=1){
#   # time_definition <- 1
#   ss <- as.numeric(strsplit(strsplit(data_file,split='s')[[1]][2],split='.c')[[1]])
#   df <- read.csv(data_file)
#   time <- seq(0,max(df$session_time_sec),by=time_definition)
#   cum_resp_right <- NULL
#   cum_resp_left <- NULL
#   cum_reinf_right <- NULL
#   cum_reinf_left <- NULL
#   for(tt in time){
#     cum_resp_right <- append(cum_resp_right,
#                              sum(df$session_time_sec<=tt&df$event=='response_right_key'))
#     cum_resp_left <- append(cum_resp_left,
#                             sum(df$session_time_sec<=tt&df$event=='response_left_key'))
#     cum_reinf_right <- append(cum_reinf_right,
#                               sum(df$session_time_sec<=tt&df$event=='feeder_on_right'))
#     cum_reinf_left <- append(cum_reinf_left,
#                              sum(df$session_time_sec<=tt&df$event=='feeder_on_left'))
#   }
#   cum_df <- data.frame(bird=bb,
#                        session=ss,
#                        time,
#                        cum_resp_left,
#                        cum_resp_right,
#                        cum_reinf_left,
#                        cum_reinf_right,
#                        first_cp_left=unique(df$first_cp_left),
#                        second_cp_left=unique(df$second_cp_left),
#                        first_cp_right=unique(df$first_cp_right),
#                        second_cp_right=unique(df$second_cp_right),
#                        med_data_file=unique(df$data_file),
#                        csv_data_file=data_file,
#                        med_notation_file=unique(df$med_notation_file))
#   
#   return(cum_df)
# }
# 
# birds <- c('p004','p054','p138','p510','p530','p736')
# # bb <- birds[4]
# for(bb in birds){
#   # setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
#   setwd('~/Documents/Research/JAPE/ConcurrentData/RData files/')
#   load(paste(bb,'_cum_resp_reinf_list.RData',sep=''))
#   # setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')
#   setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')
#   bird_archives <- dir()[grep(bb,dir())]
#   files_to_add <- bird_archives[!bird_archives%in%names(cum_list)]
#   # data_file <- files_to_add[1]
#   c_ss <- length(cum_list)
#   for(data_file in files_to_add){
#     c_ss <- c_ss+1
#     cum_list[[c_ss]] <- get_cum_counts(data_file)
#     names(cum_list)[c_ss] <- data_file
#     print(paste(bb,data_file))
#   }
#   # setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
#   setwd('~/Documents/Research/JAPE/ConcurrentData/RData files/')
#   save(cum_list,file=paste(bb,'cum_resp_reinf_list.RData',sep='_'))
# }

# rm(list=ls())
# for(bb in birds){
#   load(paste(bb,'cum_resp_reinf_list.RData',sep='_'))
#   for(dd in 80:77){
#     cum_list <- cum_list[-dd]
#   }
#   save(cum_list,file=paste(bb,'cum_resp_reinf_list.RData',sep='_'))
# }

# test_list <- cum_list
# tl <- test_list[-80]



rm(list=ls())
dev.off()
x11(width=10,height=8)
setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
# setwd('~/Documents/Research/jape/ConcurrentData/RData files/')
birds <- c('p004','p054','p138','p510','p530','p736')
layout(matrix(1:6,ncol=3))
# bb <- birds[1]
for(bb in birds){
  # plot(NULL,xlim=c(0,300000),ylim=c(0,300000))
  plot(NULL,xlim=c(0,100000),ylim=c(0,100000))
  abline(0,1,lty='dashed')
  mtext(bb,3)
  load(paste(bb,'_cum_resp_reinf_list.RData',sep=''))
  
  total_resp_right <- 0
  total_resp_left <- 0
  
  # for(ll in 1:length(cum_list)){
  for(ss in 110:length(cum_list)){
    session_name <- paste(bb,'s',sprintf('%02d',ss),'.csv',sep='')
    ll <- which(names(cum_list)==session_name)
    # for(ll in 1:15){
    # for(ll in c(1:15,60:76)){
    # if(!unique(cum_list[[ll]]$med_file%in%c('japede_L_30_30_30_R_90_90_90',
    # 'japede_L_90_90_90_R_30_30_30'))){
    # resp_right <- cum_list[[ll]]$cum_resp_right#+total_resp_right
    # resp_left <- cum_list[[ll]]$cum_resp_left#+total_resp_left
    resp_right <- cum_list[[ll]]$cum_resp_right+total_resp_right
    resp_left <- cum_list[[ll]]$cum_resp_left+total_resp_left
    # resp_right <- cum_list[[ll]]$cum_reinf_right#+total_resp_right
    # resp_left <- cum_list[[ll]]$cum_reinf_left#+total_resp_left
    # line_color <- hsv(h=1,v=1-(ll/76),s=0)
    total_resp_right <- max(resp_right)
    total_resp_left <- max(resp_left)
    pt_col <- 'red'
    if(unique(cum_list[[ll]]$med_notation_file)=='japede_L_30_30_30_R_90_90_90'){
      pt_col <- 'blue'
    }
    else if(unique(cum_list[[ll]]$med_notation_file)=='japede_L_90_90_90_R_30_30_30'){
      pt_col <- 'orange'
    }
    else if(unique(cum_list[[ll]]$med_notation_file)=='japede_L_45_45_45_R_45_45_45'){
      pt_col <- 'green'
    }
    pt_cex <- 1
    line_transp <- '22'
    lwd=1
    if(ss >= 116){pt_cex=1.5;line_transp <- 'ff';lwd=1.5}
    line_color <- paste('#000000',line_transp,sep='')
    points(resp_right,
           resp_left,type='l',col=line_color,lwd=lwd)
    points(total_resp_right,total_resp_left,pch=4,col=pt_col,cex=pt_cex,lwd=lwd)
  }
  # }
}

