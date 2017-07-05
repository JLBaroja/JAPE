# Extracts data and builds different human-friendly datasets.
# Name of script, and of main .RData object, honors Dorothy Johnson Vaughan 1910-2008

rm(list=ls())
concurrent_data_folder <- '~/Documents/Luis/JAPE/ConcurrentData'
setwd(concurrent_data_folder)
japede_full <- read.csv('concurrent_full.csv',stringsAsFactors = F)

birdz <- sort(unique(japede_full$bird))
sessionz <- sort(unique(japede_full$session))

layout(matrix(1:(6*7),ncol=6))
par(mar=rep(1,4),mgp=c(2,0,0),tck=0.02)

brd <- birdz[3]
# for(brd in birdz){
b_data <- subset(japede_full,bird==brd)
sssn <- sessionz[3]
# for(sssn in sessionz){
s_data <- subset(b_data,session==sssn)


rnf_left <- s_data$session_time_sec[which(s_data$event=='feeder_on_left')]
rnf_right <- s_data$session_time_sec[which(s_data$event=='feeder_on_right')]
all_rnf <- sort(c(rnf_left,rnf_right))
schdld_left <- s_data$session_time_sec[which(s_data$event=='reinforcer_scheduled_left')]
schdld_right <- s_data$session_time_sec[which(s_data$event=='reinforcer_scheduled_right')]
schdld_left <- schdld_left[1:length(rnf_left)]
schdld_right <- schdld_right[1:length(rnf_right)]

tss_left <- rnf_left-schdld_left
tss_right <- rnf_right-schdld_right

rsp_left <- s_data$session_time_sec[which(s_data$event=='response_left_key')]
rsp_right <- s_data$session_time_sec[which(s_data$event=='response_right_key')]


key <- NULL
n_rnf_left <- rep(NA,length(all_rnf))
n_rnf_right <- rep(NA,length(all_rnf))
time_since_scheduled <- NULL

key[which(all_rnf%in%rnf_left)] <- 'left'
key[which(all_rnf%in%rnf_right)] <- 'right'
# n_rnf_left[which(all_rnf%in%rnf_left)] <- 1:length(rnf_left)
# n_rnf_right[which(all_rnf%in%rnf_right)] <- 1:length(rnf_right)
n_rnf_left[which(key=='left')] <- 1:length(rnf_left)
n_rnf_right[which(key=='right')] <- 1:length(rnf_right)
time_since_scheduled[which(key=='left')] <- tss_left 
time_since_scheduled[which(key=='right')] <- tss_right




# print(c(length(schdld_left),
#         length(rnf_left),
#         length(schdld_right),
#         length(rnf_right)))



plot_session(s_data,time_zoom = 'start',reinf_details = T)
plot_session(s_data,time_zoom = 'end',reinf_details = T)

# plot(NULL,
#      ylim=c(0,500),
#      xlim=c(0,max(s_data$session_time_sec)),
#      axes=F)
# points(schdld_left,tss_left,pch=16,col='#0000aaaa',cex=1,type='l',lwd=2)
# points(schdld_right,tss_right,pch=16,col='#aa0000aa',cex=1,type='l',lwd=2)


# reinf_in_session <- seq(1,sum(c(sum(s_data$event=='feeder_on_left'),sum(s_data$event=='feeder_on_right'))))
#
reinforcers <- data.frame(bird=brd,
                          session=sssn,
                          n_rnf_left,
                          n_rnf_right,
                          key,
                          moment_of_delivery=all_rnf,
                          time_since_scheduled
                          );reinforcers

# }}



