rm(list=ls())
# source('~/Documents/Luis/JAPE/MoreCode/plotting_functions.R')
source('~/Documents/Research/JAPE/MoreCode/plotting_functions.R')
sesssum <- read.csv('~/Documents/Research/JAPE/ConcurrentData/session_summary.csv')

get_visits <- function(bb,ss){
  setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')
  # bb <- 'p530'
  # ss <- 's177'
  archive <- paste(bb,ss,'.csv',sep='')
  ss_dat <- read.csv(archive)
  
  # try(dev.off())
  # x11(width=18,height=5)
  # plot_session(ss_dat)
  
  responses <- subset(ss_dat,event%in%c('response_left_key','response_right_key'))
  reinforcers <- subset(ss_dat,event%in%c('feeder_on_left','feeder_on_right'))
  
  responses <- data.frame(lever=responses$event,
                          time=responses$session_time_sec)     
  responses$lever <- c('right','left')[(responses$lever=='response_left_key')+1]
  
  reinforcers <- data.frame(lever=reinforcers$event,
                            time=reinforcers$session_time_sec)
  reinforcers$lever <- c('right','left')[(reinforcers$lever=='feeder_on_left')+1]
  
  n_responses <- NULL
  n_reinforcers <- NULL
  lever <- NULL
  time_start <- NULL
  time_end <- NULL
  
  
  responding_in <- responses$lever[1]
  time_start <- append(time_start,responses$time[1])
  counting_responses <- 1
  # rr <- 2
  for(rr in 2:nrow(responses)){
    if(responses$lever[rr]==responding_in){
      # visit continues
      counting_responses <- counting_responses+1
    }
    else{
      # visit ends
      n_responses <- append(n_responses,counting_responses)
      lever <- append(lever,responding_in)
      time_end <- append(time_end,responses$time[(rr-1)])
      
      n_reinforcers <- append(n_reinforcers,
                              sum(reinforcers$time<=tail(time_end,1)&reinforcers$time>=tail(time_start,1)))
      
      time_start <- append(time_start,responses$time[(rr)])
      counting_responses <- 1
    }
    responding_in <- responses$lever[rr]
  }
  
  visits <- data.frame(session=ss,
                       bird=bb,
                       n_responses,
                       n_reinforcers,
                       lever,
                       time_start=time_start[1:(length(time_start)-1)],
                       time_end,
                       local_VI=NA,
                       alter_VI=NA)
  
  sess_info <- sesssum[sesssum$session==ss&sesssum$bird==bb,]
  
  visits$local_VI[visits$lever=='left'] <- sess_info$VIleft1
  visits$alter_VI[visits$lever=='right'] <- sess_info$VIleft1
  visits$local_VI[visits$lever=='right'] <- sess_info$VIright1
  visits$alter_VI[visits$lever=='left'] <- sess_info$VIright1
  visits$local_VI[visits$lever=='left'&visits$time_start>sess_info$CPleft] <- sess_info$VIleft2
  visits$alter_VI[visits$lever=='right'&visits$time_start>sess_info$CPleft] <- sess_info$VIleft2
  visits$local_VI[visits$lever=='right'&visits$time_start>sess_info$CPleft] <- sess_info$VIright2
  visits$alter_VI[visits$lever=='left'&visits$time_start>sess_info$CPleft] <- sess_info$VIright2
  
  return(visits)
  
}


get_bird_visits <- function(bb){
  bird_visits <- NULL
  for(ss in unique(sesssum$session)){
    bird_visits <- rbind(bird_visits,get_visits(bb,ss))
    print(c(bb,ss))
  }
  return(bird_visits)  
}

visits_p004 <- get_bird_visits('p004')
visits_p054 <- get_bird_visits('p054')
visits_p138 <- get_bird_visits('p138')
visits_p510 <- get_bird_visits('p510')
visits_p530 <- get_bird_visits('p530')
visits_p736 <- get_bird_visits('p736')
write.csv(visits_p004,file = '~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p004.csv',row.names = F)
write.csv(visits_p054,file = '~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p054.csv',row.names = F)
write.csv(visits_p138,file = '~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p138.csv',row.names = F)
write.csv(visits_p510,file = '~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p510.csv',row.names = F)
write.csv(visits_p530,file = '~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p530.csv',row.names = F)
write.csv(visits_p736,file = '~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p736.csv',row.names = F)

rm(list=ls())
visits_p004 <- read.csv('~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p004.csv')
visits_p054 <- read.csv('~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p054.csv')
visits_p138 <- read.csv('~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p138.csv')
visits_p510 <- read.csv('~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p510.csv')
visits_p530 <- read.csv('~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p530.csv')
visits_p736 <- read.csv('~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/visits_p736.csv')


full_visits <- rbind(visits_p004,
                     visits_p054,
                     visits_p138,
                     visits_p510,
                     visits_p530,
                     visits_p736)
full_visits$duration <- full_visits$time_end-full_visits$time_start
full_visits$resp_rate <- full_visits$n_responses/full_visits$duration
full_visits$reinf_rate <- full_visits$n_reinforcers/full_visits$duration

write.csv(full_visits,file='~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/full_visits.csv',row.names = F)
save(full_visits,file='full_visits.RData')

# full_visits <- rbind(full_visits,get_visits('p054','s16'))
# dim(full_visits)

# 
# head(responses,25)
# 
# dev.off()
# x11(width=10,height=10)
# par(bg='#000000',fg='#bbbbbb',col.axis='#bbbbbb')
# plot(data.frame(visits,visits$time_end-visits$time_start),col=as.numeric(visits$lever)+1)





# 
# length <- visits$time_end-visits$time_start
# 
# hist(length[length!=0],xlim=c(0,10),breaks=300)
# 
# 
# 
# 
# 
# 
# comparison <- c(responses$lever,'string')!=c('string',responses$lever)
# starts <- comparison[-length(comparison)]
# ends <- comparison[-1]








