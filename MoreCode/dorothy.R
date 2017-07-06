# Extracts data and builds different human-friendly datasets.
# Name of script, and of main .RData object, honors Dorothy Johnson Vaughan 1910-2008

# rm(list=ls())
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
all_rsp <- sort(c(rsp_left,rsp_right))
key_rsp <- NULL
key_rsp[which(all_rsp%in%rsp_left)] <- 'left'
key_rsp[which(all_rsp%in%rsp_right)] <- 'right'
join_left <- rep(F,length(all_rsp))
join_right <- rep(F,length(all_rsp))
leave_left <- rep(F,length(all_rsp))
leave_right <- rep(F,length(all_rsp))
for(rs in 2:length(all_rsp)){
  if(key_rsp[rs]=='left'&key_rsp[(rs-1)]=='right'){join_left[rs] <- T}
  if(key_rsp[rs]=='left'&key_rsp[(rs+1)]=='right'){leave_left[rs] <- T}
  if(key_rsp[rs]=='right'&key_rsp[(rs-1)]=='left'){join_right[rs] <- T}
  if(key_rsp[rs]=='right'&key_rsp[(rs+1)]=='left'){leave_right[rs] <- T}
}
rnfrcd_left <- rep(F,length(all_rsp))
rnfrcd_right <- rep(F,length(all_rsp))
rnfrcd_left[which(all_rsp%in%rnf_left)] <- T
rnfrcd_right[which(all_rsp%in%rnf_right)] <- T




rr <- all_rnf[83]
rsp_row <- which(all_rsp==rr)
if(key_rsp[rsp_row]=='left'){
  next_leave_row <- which(leave_left&all_rsp>=rr)[1]
  next_join_row <- which(join_right&all_rsp>=rr)[1]
  reinforcer_count <- rnfrcd_left[rsp_row:next_leave_row]}
if(key_rsp[rsp_row]=='right'){
  next_leave_row <- which(leave_right&all_rsp>=rr)[1]
  next_join_row <- which(joint_left&all_rsp>=rr)[1]
  reinforcer_count <- rnfrcd_right[rsp_row:next_leave_row]}

rnf_bfr_leave <- sum(reinforcer_count)
rsp_bfr_leave <- length(rsp_row:next_leave_row)
tm_bfr_leave <- all_rsp[next_leave_row]-all_rsp[rsp_row]
tm_bfr_join <- all_rsp[next_join_row]-all_rsp[rsp_row]

responses[rsp_row:next_join_row,]


tsls_left <- NA
if(length(rnf_left)>=2){
for(rr in 2:length(rnf_left)){tsls_left[rr] <- rnf_left[rr]-rnf_left[(rr-1)]}}
tsls_right <- NA
for(rr in 2:length(rnf_right)){tsls_right[rr] <- rnf_right[rr]-rnf_right[(rr-1)]}
tsls_any <- NA
for(rr in 2:length(all_rnf)){tsls_any[rr] <- all_rnf[rr]-all_rnf[(rr-1)]}

key_rnf <- NULL
n_rnf_left <- rep(NA,length(all_rnf))
n_rnf_right <- rep(NA,length(all_rnf))
time_since_scheduled <- NULL
time_since_last_in_same <- NULL

key_rnf[which(all_rnf%in%rnf_left)] <- 'left'
key_rnf[which(all_rnf%in%rnf_right)] <- 'right'
# n_rnf_left[which(all_rnf%in%rnf_left)] <- 1:length(rnf_left)
# n_rnf_right[which(all_rnf%in%rnf_right)] <- 1:length(rnf_right)
n_rnf_left[which(key_rnf=='left')] <- 1:length(rnf_left)
n_rnf_right[which(key_rnf=='right')] <- 1:length(rnf_right)
time_since_scheduled[which(key_rnf=='left')] <- tss_left 
time_since_scheduled[which(key_rnf=='right')] <- tss_right
time_since_last_in_same[which(key_rnf=='left')] <- tsls_left 
time_since_last_in_same[which(key_rnf=='right')] <- tsls_right


# print(c(length(schdld_left),
#         length(rnf_left),
#         length(schdld_right),
#         length(rnf_right)))


layout(1:2)
plot_session(s_data,time_zoom = 'start',reinf_details = T)
# plot_session(s_data,time_zoom = c(70,90),reinf_details = T)
plot_session(s_data,time_zoom = c(2200,2600),reinf_details = T)

# plot(NULL,
#      ylim=c(0,200),
#      xlim=c(0,max(s_data$session_time_sec)),
#      axes=F)
# points(schdld_left,tss_left,pch=16,col='#0000aaaa',cex=1,type='l',lwd=2)
# points(schdld_right,tss_right,pch=16,col='#aa0000aa',cex=1,type='l',lwd=2)
# points(rnf_left,tsls_left,pch=16,col='#0000aaaa',cex=1,type='l',lwd=2)
# points(rnf_right,tsls_right,pch=16,col='#aa0000aa',cex=1,type='l',lwd=2)
# points(all_rnf,tsls_any,pch=16,col='#aaaaaa',cex=1,type='l',lwd=1)
# points(rnf_left,tsls_any[which(key_rnf=='left')],pch=16,col='#0000aaaa',cex=1,type='l',lwd=2)
# points(rnf_right,tsls_any[which(key_rnf=='right')],pch=16,col='#aa0000aa',cex=1,type='l',lwd=2)
# hist(tsls_left,xlim=c(0,600),breaks=seq(0,600,10))
# hist(tsls_right,xlim=c(0,600),breaks=seq(0,600,10))


# reinf_in_session <- seq(1,sum(c(sum(s_data$event=='feeder_on_left'),sum(s_data$event=='feeder_on_right'))))
#
responses <- data.frame(bird=brd,
                        session=sssn,
                        all_rsp,
                        key_rsp,
                        join_left,
                        join_right,
                        leave_left,
                        leave_right,
                        rnfrcd_left,
                        rnfrcd_right
                        );head(responses,20);tail(responses,60)

reinforcers <- data.frame(bird=brd,
                          session=sssn,
                          n_rnf_left,
                          n_rnf_right,
                          key_rnf,
                          moment_of_delivery=all_rnf,
                          time_since_scheduled,
                          time_since_last_in_same
                          );head(reinforcers,7);tail(reinforcers,15)

# }}



