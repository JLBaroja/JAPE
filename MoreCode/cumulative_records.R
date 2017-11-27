rm(list=ls())
setwd('~/Documents/Luis/JAPE/ConcurrentData/RData files/')
# setwd('~/Documents/Research/JAPE/ConcurrentData/RData files/')

x11(width=15,height=6)
par(mar=c(3,3,1,1))
layout(matrix(1:12,ncol=6))
for(bb in c('p004','p054','p138','p510','p530','p736')){

  # bb <- 'p530'
  sessions <- 130:149
  add_sessions <- TRUE
  load(paste(bb,'cum_resp_reinf_list.RData',sep='_'))
  load(paste(bb,'events_list.RData',sep='_'))

  total_resp_right <- 0
  total_resp_left <- 0
  total_reinf_right <- 0
  total_reinf_left <- 0
  total_time <- 0
  resp_right <- NULL
  resp_left <- NULL
  reinf_right <- NULL
  reinf_left <- NULL
  added_time <- NULL
  fcp_resp_right <- NULL
  fcp_resp_left <- NULL
  fcp_reinf_right <- NULL
  fcp_reinf_left <- NULL
  snds_resp_right <- NULL
  snds_resp_left <- NULL
  snds_reinf_right <- NULL
  snds_reinf_left <- NULL
  true_change <- NULL

  # ss <- sessions[5]
  for(ss in sessions){
    s_cum <- which(names(cum_list)==paste(bb,'s',sprintf('%02d',ss),'.csv',sep=''))
    s_evnt <- which(names(events_list)==paste(bb,'s',sprintf('%02d',ss),'.csv',sep=''))
    cum_data <- (cum_list[[s_cum]])
    event_data <- (events_list[[s_evnt]])
    print(c(as.character(unique(event_data$session)),
            as.character(unique(event_data$date)),
            as.character(unique(event_data$bird)),
            as.character(unique(event_data$med_notation_file))))
    time_first_cp <- unique(event_data$first_cp_left)
    broken_schedules_labs <- strsplit(as.character(unique(event_data$med_notation_file)),split='_')[[1]]
    left_bfr_1 <- as.numeric(broken_schedules_labs[3])
    left_aftr_1 <- as.numeric(broken_schedules_labs[4])
    left_bfr_2 <-  as.numeric(broken_schedules_labs[5])
    right_bfr_1 <- as.numeric(broken_schedules_labs[7])
    right_aftr_1 <- as.numeric(broken_schedules_labs[8])
    right_bfr_2 <- as.numeric(broken_schedules_labs[9])
    true_change <- append(true_change,left_bfr_1!=left_aftr_1)

    #
    #

    resp_right <- append(resp_right,cum_data$cum_resp_right+total_resp_right)
    resp_left <- append(resp_left,cum_data$cum_resp_left+total_resp_left)
    reinf_right <- append(reinf_right,cum_data$cum_reinf_right+total_reinf_right)
    reinf_left <- append(reinf_left,cum_data$cum_reinf_left+total_reinf_left)
    added_time <- append(added_time,cum_data$time+total_time)

    fcp_resp_right <- append(fcp_resp_right,resp_right[added_time==time_first_cp+total_time])
    fcp_resp_left <- append(fcp_resp_left,resp_left[added_time==time_first_cp+total_time])
    fcp_reinf_right <- append(fcp_reinf_right,reinf_right[added_time==time_first_cp+total_time])
    fcp_reinf_left <- append(fcp_reinf_left,reinf_left[added_time==time_first_cp+total_time])
    snds_resp_right <- append(snds_resp_right,max(resp_right))
    snds_resp_left <- append(snds_resp_left,max(resp_left))
    snds_reinf_right <- append(snds_reinf_right,max(reinf_right))
    snds_reinf_left <- append(snds_reinf_left,max(reinf_left))

    total_resp_right <- max(resp_right)*add_sessions
    total_resp_left <- max(resp_left)*add_sessions
    total_reinf_right <- max(reinf_right)*add_sessions
    total_reinf_left <- max(reinf_left)*add_sessions
    total_time <- max(added_time)*add_sessions
  }


  for(set in 1:2){
    if(set==1){
      var_x <- resp_right
      var_y <- resp_left
      markers_x <- fcp_resp_right[true_change]
      markers_y <- fcp_resp_left[true_change]
      marks_x <- snds_resp_right
      marks_y <- snds_resp_left}
    if(set==2){
      var_x <- reinf_right
      var_y <- reinf_left
      markers_x <- fcp_reinf_right[true_change]
      markers_y <- fcp_reinf_left[true_change]
      marks_x <- snds_reinf_right
      marks_y <- snds_reinf_left}
    # dev.off()
    # x11(width=8,height=8)
    # par(mar=c(3,3,1,1))
    plot(var_x,var_y,type='l',
         xlim=c(0,max(c(max(var_x),max(var_y)))),
         ylim=c(0,max(c(max(var_x),max(var_y)))),main=bb)
    abline(0,1,lty='dashed')
    points(markers_x,markers_y)
    points(marks_x,marks_y,pch=4,col='#ee000077')
  }
}


#
# ss <- 1
# cum_list[[which(names(cum_list)==)]]






















# rm(list=ls()[!ls()%in%c('japede_events','japede_cum_events','birds')])

get_cum_counts <- function(bb,sessions){
  
  # bb <- 'p530'
  # sessions <- 1
  # sessions <- 131:136
  add_sessions <- TRUE
  
  cum_list <- japede_cum_events[[which(names(japede_cum_events)==bb)]]
  events_list <- japede_events[[which(names(japede_events)==bb)]]
  
  total_resp_right <- 0
  total_resp_left <- 0
  total_reinf_right <- 0
  total_reinf_left <- 0
  total_time <- 0
  resp_right <- NULL
  resp_left <- NULL
  reinf_right <- NULL
  reinf_left <- NULL
  added_time <- NULL
  fcp_resp_right <- NULL
  fcp_resp_left <- NULL
  fcp_reinf_right <- NULL
  fcp_reinf_left <- NULL
  snds_resp_right <- NULL
  snds_resp_left <- NULL
  snds_reinf_right <- NULL
  snds_reinf_left <- NULL
  true_change <- NULL
  change_point <- NULL
  session_labels <- NULL
  operating_left <- NULL
  operating_right <- NULL
  session_end <- NULL
  # ss <- sessions[5]
  for(ss in sessions){
    s_cum <- which(names(cum_list)==paste(bb,'s',sprintf('%02d',ss),'.csv',sep=''))
    s_evnt <- which(names(events_list)==paste(bb,'s',sprintf('%02d',ss),'.csv',sep=''))
    cum_data <- tail(cum_list[[s_cum]],nrow(cum_list[[s_cum]])-1) # These assume time definition = 1 sec
    event_data <- tail(events_list[[s_evnt]],nrow(events_list[[s_evnt]])-1) # These assume time definition = 1 sec
    print(c(as.character(unique(event_data$session)),
            as.character(unique(event_data$date)),
            as.character(unique(event_data$bird)),
            as.character(unique(event_data$med_notation_file))))
    
    time_first_cp <- unique(event_data$first_cp_left)
    broken_schedules_labs <- strsplit(as.character(unique(event_data$med_notation_file)),split='_')[[1]]
    left_bfr_1 <- as.numeric(broken_schedules_labs[3])
    left_aftr_1 <- as.numeric(broken_schedules_labs[4])
    left_bfr_2 <-  as.numeric(broken_schedules_labs[5])
    right_bfr_1 <- as.numeric(broken_schedules_labs[7])
    right_aftr_1 <- as.numeric(broken_schedules_labs[8])
    right_bfr_2 <- as.numeric(broken_schedules_labs[9])
    true_change <- append(true_change,rep(left_bfr_1!=left_aftr_1,nrow(cum_data)))
    change_point <- append(change_point,cum_data$time==cum_data$first_cp_left&left_bfr_1!=left_aftr_1)
    session_labels <- append(session_labels,rep(unique(cum_data$session),nrow(cum_data)))
    
    session_end <- append(session_end,c(rep(F,nrow(cum_data)-1),T))
    
    operating_left <- append(operating_left,
                             c(rep(left_bfr_1,sum(cum_data$time<cum_data$first_cp_left)),
                               rep(left_aftr_1,sum(cum_data$time>=cum_data$first_cp_left&cum_data$time<cum_data$second_cp_left)),
                               rep(left_bfr_2,sum(cum_data$time>=cum_data$second_cp_left))))
    
    operating_right <- append(operating_right,
                              c(rep(right_bfr_1,sum(cum_data$time<cum_data$first_cp_right)),
                                rep(right_aftr_1,sum(cum_data$time>=cum_data$first_cp_right&cum_data$time<cum_data$second_cp_right)),
                                rep(right_bfr_2,sum(cum_data$time>=cum_data$second_cp_right))))
    
    resp_right <- append(resp_right,cum_data$cum_resp_right+total_resp_right)
    resp_left <- append(resp_left,cum_data$cum_resp_left+total_resp_left)
    reinf_right <- append(reinf_right,cum_data$cum_reinf_right+total_reinf_right)
    reinf_left <- append(reinf_left,cum_data$cum_reinf_left+total_reinf_left)
    added_time <- append(added_time,cum_data$time+total_time)
    
    fcp_resp_right <- append(fcp_resp_right,resp_right[added_time==time_first_cp+total_time])
    fcp_resp_left <- append(fcp_resp_left,resp_left[added_time==time_first_cp+total_time])
    fcp_reinf_right <- append(fcp_reinf_right,reinf_right[added_time==time_first_cp+total_time])
    fcp_reinf_left <- append(fcp_reinf_left,reinf_left[added_time==time_first_cp+total_time])
    snds_resp_right <- append(snds_resp_right,max(resp_right))
    snds_resp_left <- append(snds_resp_left,max(resp_left))
    snds_reinf_right <- append(snds_reinf_right,max(reinf_right))
    snds_reinf_left <- append(snds_reinf_left,max(reinf_left))
    
    total_resp_right <- max(resp_right)*add_sessions
    total_resp_left <- max(resp_left)*add_sessions
    total_reinf_right <- max(reinf_right)*add_sessions
    total_reinf_left <- max(reinf_left)*add_sessions
    total_time <- max(added_time)*add_sessions
  }
  
  return(data.frame(bird=bb,
                    session=session_labels,
                    cum_resp_left=resp_left,
                    cum_resp_right=resp_right,
                    cum_reinf_left=reinf_left,
                    cum_reinf_right=reinf_right,
                    change_point=change_point,
                    change_in_session=true_change,
                    vi_left=operating_left,
                    vi_right=operating_right,
                    session_end=session_end,
                    added_time=added_time
  ))
  
}