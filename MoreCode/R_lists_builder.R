rm(list=ls())
# setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')

birds <- c('p004','p054','p138','p510','p530','p736')
# sessions <- 1:70

time_definition <- 1

# bb <- birds[5]
# ss <- 25

sss <- 1:85
for(bb in birds){
  cum_list <- vector(mode='list')
  c_ss <- 0
  setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')
  # setwd('~/Documents/Research/jape/ConcurrentData/CSV files/')
  for(ss in sss){
    c_ss <- c_ss+1
    data_file <- paste(bb,'s',sprintf('%02d',ss),'.csv',sep='')
    if(data_file%in%dir()){
      
      df <- read.csv(data_file)
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
                                     first_cp_left=unique(df$first_cp_left),
                                     second_cp_left=unique(df$second_cp_left),
                                     first_cp_right=unique(df$first_cp_right),
                                     second_cp_right=unique(df$second_cp_right),
                                     med_data_file=unique(df$data_file),
                                     csv_data_file=data_file,
                                     med_notation_file=unique(df$med_notation_file))
      
      names(cum_list)[c_ss] <- data_file
      print(paste(bb,ss,data_file))
      
    }
  }
  # setwd('~/Documents/Research/jape/ConcurrentData/RData files/')
  setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
  save(cum_list,file=paste(bb,'cum_resp_reinf_list.RData',sep='_'))
}





# Updating cum_lists
rm(list=ls())

get_cum_counts <- function(data_file,time_definition=1){
  # time_definition <- 1
  ss <- as.numeric(strsplit(strsplit(data_file,split='s')[[1]][2],split='.c')[[1]])
  df <- read.csv(data_file)
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
  cum_df <- data.frame(bird=bb,
                       session=ss,
                       time,
                       cum_resp_left,
                       cum_resp_right,
                       cum_reinf_left,
                       cum_reinf_right,
                       first_cp_left=unique(df$first_cp_left),
                       second_cp_left=unique(df$second_cp_left),
                       first_cp_right=unique(df$first_cp_right),
                       second_cp_right=unique(df$second_cp_right),
                       med_data_file=unique(df$data_file),
                       csv_data_file=data_file,
                       med_notation_file=unique(df$med_notation_file))
  
  return(cum_df)
}

birds <- c('p004','p054','p138','p510','p530','p736')
# bb <- birds[4]
for(bb in birds){
  setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
  # setwd('~/Documents/Research/JAPE/ConcurrentData/RData files/')
  load(paste(bb,'_cum_resp_reinf_list.RData',sep=''))
  setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')
  # setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')
  bird_archives <- dir()[grep(bb,dir())]
  files_to_add <- bird_archives[!bird_archives%in%names(cum_list)]
  # data_file <- files_to_add[1]
  c_ss <- length(cum_list)
  for(data_file in files_to_add){
    c_ss <- c_ss+1
    cum_list[[c_ss]] <- get_cum_counts(data_file)
    names(cum_list)[c_ss] <- data_file
    print(paste(bb,data_file))
  }
  setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
  # setwd('~/Documents/Research/JAPE/ConcurrentData/RData files/')
  save(cum_list,file=paste(bb,'cum_resp_reinf_list.RData',sep='_'))
}

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







# Building events lists
rm(list=ls())
birds <- c('p004','p054','p138','p510','p530','p736')
sss <- 1:136
for(bb in birds){
  events_list <- vector(mode='list')
  c_ss <- 0
  setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')
  # setwd('~/Documents/Research/jape/ConcurrentData/CSV files/')
  for(ss in sss){
    c_ss <- c_ss+1
    data_file <- paste(bb,'s',sprintf('%02d',ss),'.csv',sep='')
    if(data_file%in%dir()){
      df <- read.csv(data_file)
      events_list[[c_ss]] <- subset(df,event%in%c('session_start',
                                                  'session_end',
                                                  'response_left_key',
                                                  'response_right_key',
                                                  'feeder_on_left',
                                                  'feeder_on_right',
                                                  'reinforcer_scheduled_right',
                                                  'reinforcer_shceduled_left'))
      names(events_list)[c_ss] <- data_file
      print(paste(bb,ss,data_file,dim(events_list[[c_ss]])[1]))
    }
  }
  # setwd('~/Documents/Research/jape/ConcurrentData/RData files/')
  setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
  save(events_list,file=paste(bb,'events_list.RData',sep='_'))
}




# # Building general list
# rm(list=ls())
# japede_events <- vector(mode='list')
# japede_cum_events <- vector(mode='list')
# setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
# birds <- c('p004','p054','p138','p510','p530','p736')
# c_bb <- 0
# # bb <- birds[4]
# for(bb in birds){
#   c_bb <- c_bb+1
#   load(paste(bb,'cum_resp_reinf_list.RData',sep='_'))
#   load(paste(bb,'events_list.RData',sep='_'))
#   japede_events[[c_bb]] <- events_list
#   japede_cum_events[[c_bb]] <- cum_list
#   names(japede_events)[c_bb] <- bb
#   names(japede_cum_events)[c_bb] <- bb
# }
