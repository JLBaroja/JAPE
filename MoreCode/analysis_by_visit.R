rm(list=ls())
source('~/Documents/Luis/JAPE/MoreCode/plotting_functions.R')

setwd('~/Documents/Luis/JAPE/ConcurrentData/CSV files/')
bb <- 'p530'
ss <- 's177'
archive <- paste(bb,ss,'.csv',sep='')
ss_dat <- read.csv(archive)

dev.off()
x11(width=18,height=5)
plot_session(ss_dat,time_zoom = c(0,100))


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

visits <- data.frame(n_responses,
                     n_reinforcers,
                     lever,
                     time_start=time_start[1:(length(time_start)-1)],
                     time_end)

head(visits)
head(responses,25)

dev.off()
x11(width=10,height=10)
par(bg='#000000',fg='#bbbbbb',col.axis='#bbbbbb')
plot(data.frame(visits,visits$time_end-visits$time_start),col=as.numeric(visits$lever)+1)















comparison <- c(responses$lever,'string')!=c('string',responses$lever)
starts <- comparison[-length(comparison)]
ends <- comparison[-1]








