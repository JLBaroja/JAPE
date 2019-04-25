rm(list=ls())
# source('~/Documents/Luis/JAPE/MoreCode/plotting_functions.R')
source('~/Documents/Research/JAPE/MoreCode/plotting_functions.R')
sesssum <- read.csv('~/Documents/Research/JAPE/ConcurrentData/session_summary.csv')

get_responses <- function(bb,ss){
  setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')
  # bb <- 'p736'
  # ss <- 's04'
  # for(ss in unique(sesssum$session)){
  archive <- paste(bb,ss,'.csv',sep='')
  ss_dat <- read.csv(archive)
  #   print(c(ss,sum(ss_dat$event=='response_left_key'),sum(ss_dat$event=='response_right_key')))
  # }
  
  # try(dev.off())
  # x11(width=18,height=5)
  # plot_session(ss_dat,time_zoom = c(0,115))
  
  responses <- subset(ss_dat,event%in%c('response_left_key','response_right_key'))
  reinforcers <- subset(ss_dat,event%in%c('feeder_on_left','feeder_on_right'))
  
  responses <- data.frame(lever=responses$event,
                          time=responses$session_time_sec)     
  responses$lever <- c('right','left')[(responses$lever=='response_left_key')+1]
  
  reinforcers <- data.frame(lever=reinforcers$event,
                            time=reinforcers$session_time_sec)
  reinforcers$lever <- c('right','left')[(reinforcers$lever=='feeder_on_left')+1]
  
  
  cum_resp_left <- NA
  cum_resp_right <- NA
  cum_reinf_left <- NA
  cum_reinf_right <- NA
  
  for(i in 1:nrow(responses)){
    cum_resp_left[i] <- sum(responses$lever=='left'&responses$time<=responses$time[i])
    cum_resp_right[i] <- sum(responses$lever=='right'&responses$time<=responses$time[i])
    cum_reinf_left[i] <- sum(reinforcers$lever=='left'&reinforcers$time<=responses$time[i])
    cum_reinf_right[i] <- sum(reinforcers$lever=='right'&reinforcers$time<=responses$time[i])
  }
  
  responses$irt_local <- NA
  left_rows <- which(responses$lever=='left')
  right_rows <- which(responses$lever=='right')
  responses$irt_local[left_rows] <- c(0,
                                      responses$time[left_rows[2:length(left_rows)]]-responses$time[left_rows[1:(length(left_rows)-1)]])
  responses$irt_local[right_rows] <- c(0,
                                       responses$time[right_rows[2:length(right_rows)]]-responses$time[right_rows[1:(length(right_rows)-1)]])
  responses$irt_global <- c(0,
                            responses$time[2:nrow(responses)]-responses$time[1:(nrow(responses)-1)])
  
  responses$reinforced <- responses$time%in%reinforcers$time
  first_in_visit <- c(T,
                      responses$lever[2:nrow(responses)]!=responses$lever[1:(nrow(responses)-1)])
  last_in_visit <- c(responses$lever[1:(nrow(responses)-1)]!=responses$lever[2:nrow(responses)],
                     T)
  
  responses$session <- ss
  responses$bird <- bb
  responses$cum_resp_left <- cum_resp_left
  responses$cum_resp_right <- cum_resp_right
  responses$cum_reinf_left <- cum_reinf_left
  responses$cum_reinf_right <- cum_reinf_right
  responses$first_in_visit <- first_in_visit
  responses$last_in_visit <- last_in_visit
  # responses$reinforced <- reinforced
  # responses$irt_global <- irt_global
  # head(responses,15)
  
  sess_info <- sesssum[sesssum$session==ss&sesssum$bird==bb,]
  
  responses$local_VI[responses$lever=='left'] <- sess_info$VIleft1
  responses$alter_VI[responses$lever=='right'] <- sess_info$VIleft1
  responses$local_VI[responses$lever=='right'] <- sess_info$VIright1
  responses$alter_VI[responses$lever=='left'] <- sess_info$VIright1
  responses$local_VI[responses$lever=='left'&responses$time>sess_info$CPleft] <- sess_info$VIleft2
  responses$alter_VI[responses$lever=='right'&responses$time>sess_info$CPleft] <- sess_info$VIleft2
  responses$local_VI[responses$lever=='right'&responses$time>sess_info$CPleft] <- sess_info$VIright2
  responses$alter_VI[responses$lever=='left'&responses$time>sess_info$CPleft] <- sess_info$VIright2
  
  return(responses)
  
}


get_bird_responses <- function(bb){
  bird_responses <- NULL
  for(ss in unique(sesssum$session)){
    bird_responses <- rbind(bird_responses,get_responses(bb,ss))
    print(c(bb,ss))
  }
  return(bird_responses)  
}


responses_p004 <- get_bird_responses('p004')
responses_p054 <- get_bird_responses('p054')
responses_p138 <- get_bird_responses('p138')
responses_p510 <- get_bird_responses('p510')
responses_p530 <- get_bird_responses('p530')
responses_p736 <- get_bird_responses('p736')
setwd('~/Documents/Research/JAPE/ConcurrentData/ResponseAnalysis/')
write.csv(responses_p004,file = 'responses_p004.csv',row.names = F)
write.csv(responses_p054,file = 'responses_p054.csv',row.names = F)
write.csv(responses_p138,file = 'responses_p138.csv',row.names = F)
write.csv(responses_p510,file = 'responses_p510.csv',row.names = F)
write.csv(responses_p530,file = 'responses_p530.csv',row.names = F)
write.csv(responses_p736,file = 'responses_p736.csv',row.names = F)

rm(list=ls())
responses_p004 <- read.csv('responses_p004.csv')
responses_p054 <- read.csv('responses_p054.csv')
responses_p138 <- read.csv('responses_p138.csv')
responses_p510 <- read.csv('responses_p510.csv')
responses_p530 <- read.csv('responses_p530.csv')
responses_p736 <- read.csv('responses_p736.csv')

full_responses <- rbind(responses_p004,
                        responses_p054,
                        responses_p138,
                        responses_p510,
                        responses_p530,
                        responses_p736)
write.csv(full_responses,file='full_responses.csv',row.names=F)
save(full_responses,file='full_responses.RData')


