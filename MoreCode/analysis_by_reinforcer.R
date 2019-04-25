rm(list=ls())
# source('~/Documents/Luis/JAPE/MoreCode/plotting_functions.R')
source('~/Documents/Research/JAPE/MoreCode/plotting_functions.R')
sesssum <- read.csv('~/Documents/Research/JAPE/ConcurrentData/session_summary.csv')
# visits <- read.csv('~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/full_visits.csv')


get_reinforcers <- function(bb,ss){
  setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')
  
  # bb <- 'p138'
  # ss <- 's159'
  
  # for(bb in unique(sesssum$bird)){
  #   for(ss in unique(sesssum$session)){
  
  archive <- paste(bb,ss,'.csv',sep='')
  ss_dat <- read.csv(archive)
  
  # try(dev.off())
  # x11(width=12,height=5)
  # plot_session(bb,ss,time_zoom = c(0,3600))
  
  responses <- subset(ss_dat,event%in%c('response_left_key','response_right_key'))
  reinforcers <- subset(ss_dat,event%in%c('feeder_on_left','feeder_on_right'))
  baits <- subset(ss_dat,event%in%c('reinforcer_scheduled_left','reinforcer_scheduled_right'))
  
  responses <- data.frame(lever=responses$event,
                          time=responses$session_time_sec)     
  responses$lever <- c('right','left')[(responses$lever=='response_left_key')+1]
  
  reinforcers <- data.frame(lever=reinforcers$event,
                            time=reinforcers$session_time_sec)
  reinforcers$lever <- c('right','left')[(reinforcers$lever=='feeder_on_left')+1]
  
  baits <- data.frame(lever=baits$event,
                      time=baits$session_time_sec)
  baits$lever <- c('right','left')[(baits$lever=='reinforcer_scheduled_left')+1]
  
  baits_left <- which(baits$lever=='left')
  baits_right <- which(baits$lever=='right')
  
  baits_2_keep <- sort(
    union(baits_left[1:sum(reinforcers$lever=='left')],
          baits_right[1:sum(reinforcers$lever=='right')])
  )
  
  baits <- baits[baits_2_keep,]
  
  # print(c(bb,ss,
  #         sum(reinforcers$lever=='left')!=sum(baits$lever=='left'),
  #         sum(reinforcers$lever=='right')!=sum(baits$lever=='right'),
  #         nrow(reinforcers)!=nrow(baits)))
  #   }
  # }
  
  
  reinforcers <- rbind(c(NA,0),reinforcers)
  reinforcers$lever[1] <- 'none'
  baits <- rbind(c(NA,0),baits)
  baits$lever[1] <- 'none'
  
  
  reinforcers$scheduled_since <- NA
  reinforcers$scheduled_since[reinforcers$lever=='left'] <- baits$time[baits$lever=='left']
  reinforcers$scheduled_since[reinforcers$lever=='right'] <- baits$time[baits$lever=='right']
  
  
  
  
  cost_local <- NA
  cost_alter <- NA
  streak_len <- 0
  cum_resp_left <- NA
  cum_resp_right <- NA
  cum_reinf_left <- NA
  cum_reinf_right <- NA
  for(i in 2:nrow(reinforcers)){
    # Costs in local and alter keys
    local_key <- reinforcers$lever[i]
    alter_key <- setdiff(c('left','right'),local_key)
    cost_local[i] <- sum(responses$time>reinforcers$time[(i-1)]&responses$time<=reinforcers$time[i]&
                           responses$lever==local_key)
    cost_alter[i] <- sum(responses$time>reinforcers$time[(i-1)]&responses$time<=reinforcers$time[i]&
                           responses$lever==alter_key)
    
    # Streaks
    if(reinforcers$lever[i]==reinforcers$lever[(i-1)]){
      streak_len[i] <- streak_len[(i-1)]+1
    }
    else{
      streak_len[i] <- 1
    }
    
    # Cumulative responses
    cum_resp_left[i] <- sum(responses$time<=reinforcers$time[i]&responses$lever=='left')
    cum_resp_right[i] <- sum(responses$time<=reinforcers$time[i]&responses$lever=='right')
    
    # Cumulative rewards
    cum_reinf_left[i] <- sum(reinforcers$time<=reinforcers$time[i]&reinforcers$lever=='left')
    cum_reinf_right[i] <- sum(reinforcers$time<=reinforcers$time[i]&reinforcers$lever=='right')
  }
  
  
  reinforcers$session <- ss
  reinforcers$bird <- bb
  reinforcers$cost_local <- cost_local
  reinforcers$cost_alter <- cost_alter
  reinforcers$streak_length <- streak_len
  reinforcers$cum_resp_left <- cum_resp_left
  reinforcers$cum_resp_right <- cum_resp_right
  reinforcers$cum_reinf_left <- cum_reinf_left
  reinforcers$cum_reinf_right <- cum_reinf_right
  
  sess_info <- sesssum[sesssum$session==ss&sesssum$bird==bb,]
  
  reinforcers$local_VI[reinforcers$lever=='left'] <- sess_info$VIleft1
  reinforcers$alter_VI[reinforcers$lever=='right'] <- sess_info$VIleft1
  reinforcers$local_VI[reinforcers$lever=='right'] <- sess_info$VIright1
  reinforcers$alter_VI[reinforcers$lever=='left'] <- sess_info$VIright1
  reinforcers$local_VI[reinforcers$lever=='left'&reinforcers$time>sess_info$CPleft] <- sess_info$VIleft2
  reinforcers$alter_VI[reinforcers$lever=='right'&reinforcers$time>sess_info$CPleft] <- sess_info$VIleft2
  reinforcers$local_VI[reinforcers$lever=='right'&reinforcers$time>sess_info$CPleft] <- sess_info$VIright2
  reinforcers$alter_VI[reinforcers$lever=='left'&reinforcers$time>sess_info$CPleft] <- sess_info$VIright2

  reinforcers <- reinforcers[-1,] # Erases first row, used only for above extraction to run
  rownames(reinforcers) <- as.character(reinforcers$cum_reinf_left+reinforcers$cum_reinf_right)
  return(reinforcers)
  
}


get_bird_reinforcers <- function(bb){
  bird_reinforcers <- NULL
  for(ss in unique(sesssum$session)){
    bird_reinforcers <- rbind(bird_reinforcers,get_reinforcers(bb,ss))
    print(c(bb,ss))
  }
  return(bird_reinforcers)  
}

reinforcers_p004 <- get_bird_reinforcers('p004')
reinforcers_p054 <- get_bird_reinforcers('p054')
reinforcers_p138 <- get_bird_reinforcers('p138')
reinforcers_p510 <- get_bird_reinforcers('p510')
reinforcers_p530 <- get_bird_reinforcers('p530')
reinforcers_p736 <- get_bird_reinforcers('p736')
setwd('~/Documents/Research/JAPE/ConcurrentData/ReinforcerAnalysis/')
write.csv(reinforcers_p004,file = 'reinforcers_p004.csv',row.names = F)
write.csv(reinforcers_p054,file = 'reinforcers_p054.csv',row.names = F)
write.csv(reinforcers_p138,file = 'reinforcers_p138.csv',row.names = F)
write.csv(reinforcers_p510,file = 'reinforcers_p510.csv',row.names = F)
write.csv(reinforcers_p530,file = 'reinforcers_p530.csv',row.names = F)
write.csv(reinforcers_p736,file = 'reinforcers_p736.csv',row.names = F)



rm(list=ls())
reinforcers_p004 <- read.csv('reinforcers_p004.csv')
reinforcers_p054 <- read.csv('reinforcers_p054.csv')
reinforcers_p138 <- read.csv('reinforcers_p138.csv')
reinforcers_p510 <- read.csv('reinforcers_p510.csv')
reinforcers_p530 <- read.csv('reinforcers_p530.csv')
reinforcers_p736 <- read.csv('reinforcers_p736.csv')

full_reinforcers <- rbind(reinforcers_p004,
                          reinforcers_p054,
                          reinforcers_p138,
                          reinforcers_p510,
                          reinforcers_p530,
                          reinforcers_p736)
write.csv(full_reinforcers,file='full_reinforcers.csv',row.names=F)
save(full_reinforcers,file='full_reinforcers.RData')

# 
# rm(list=ls())
# reinforcers <- read.csv('~/Documents/Research/JAPE/ConcurrentData/ReinforcerAnalysis/full_reinforcers.csv')
# 
# plot(reinforcers$streak_length,col=as.numeric(as.factor(reinforcers$lever)))


# plot(reinforcers$streak_length[2:(nrow(reinforcers)-2)],
#      log2(reinforcers$cost_local/reinforcers$cost_alter)[3:(nrow(reinforcers)-1)],
#      type='p')
# plot(reinforcers$cum_resp_right,reinforcers$cum_resp_left)
# plot(reinforcers$cum_reinf_right,reinforcers$cum_reinf_left)
# 
# plot(reinforcers$cum_reinf_left,reinforcers$cum_resp_left)
# plot(reinforcers$cum_reinf_right,reinforcers$cum_resp_right)
# 
# try(dev.off())
# plot(log2(reinforcers$cost_alter/reinforcers$cost_local)[reinforcers$lever=='left'])
# lines(log2(reinforcers$cost_alter/reinforcers$cost_local)[reinforcers$lever=='right'])
# 
# summary(reinforcers)
