

# rm(list=ls())
# data_folder <- '~/Documents/Luis/JAPE/AutoShapingData/'
# # data_folder <- '~/Documents/Research/JAPE/AutoShapingData/'
# 
# setwd(data_folder)
# realtime <- read.csv('autoshaping_data.csv')
# 
# add_event <- function(h_event,
#                       y_coord,
#                       ...) {
#   ix_event <- which(rt$event==h_event)
#   points(rt$session_time[ix_event],rep(y_coord,length(ix_event)),...)
# }
# 
# interval_event <- function(event_name,
#                            y_coord,
#                            bw=.15,
#                            lab_on='_on',
#                            lab_off='_off',
#                            color='#0055ff66') {
# 
#   # event_name <- 'max_light'
#   # y_coord <- .5
#   # band_width <- .1
# 
#   name_on <- paste(event_name,lab_on,sep='')
#   name_off <- paste(event_name,lab_off,sep='')
#   times_on <<- rt$session_time[which(rt$event==name_on)]
#   times_off <<- rt$session_time[which(rt$event==name_off)]
# 
#   if(length(times_on)>0){
#     for(tt in 1:length(times_on)){
#       polygon(x=c(rep(times_on[tt],2),rep(times_off[tt],2)),
#               y=c(y_coord+c(+bw,-bw,-bw,+bw)/2),col=color)
# 
#     }
#   }
#   add_event(name_on,y_coord=y_coord+bw*.8,pch=25,bg=color)
#   add_event(name_off,y_coord=y_coord-bw*.8,pch=24,bg=color)
# 
#   text(-max(rt$session_time)*.01,y_coord,event_name,adj=1,cex=1,font=2)
# }
# 
# 
# # birds <- unique(realtime$subject)
# birds <- c('p051','p510','p054','p530',
#            'p056','p736','p004','p138')
# sessions <- rep(c('s01','s02','s03','s04','s05','s06','s07','s08'),each=length(birds))
# data_files <- paste(birds,sessions,'atsh',sep='_')
# 
# # dev.off()
# # x11(width = 20, height = 10)
# layout(matrix(1:8,ncol=1))
# for(df in data_files){
#   rt <- subset(realtime,data_file==df)
#   if(nrow(rt)>0){
#     y_lims <- c(-2,1)
#     par(bg='#cccccc',col.axis='#555555',fg='#333333',
#         mar=c(1,0.5,1,1))
#     plot(0,type='n',
#          xlim=c(-max(realtime$session_time)*.06,
#                 max(realtime$session_time)*1.075),
#          ylim=c(-2,.6),
#          ann=F,axes=F)
#     mtext(df,4,line=-4,font=2)
#     mtext(paste('Box:',unique(rt$box)),4,line=-2,font=2)
#     axis(1,at=0:ceiling(max(rt$session_time)),cex.axis=.6)
# 
#     interval_event('central_light',0,color = '#33cc0066')
#     add_event('resp_central_key',0,pch=4,cex=1.5,col='#ee0000')
#     interval_event('left_light',0.4,color = '#33cc0066')
#     add_event('resp_left_key',0.4,pch=4,cex=1.5,col='#ee0000')
#     interval_event('right_light',-0.4,color = '#33cc0066')
#     add_event('resp_right_key',-0.4,pch=4,cex=1.5,col='#ee0000')
#     interval_event('feeder',-.85,color = '#ee770066')
# 
#     z_labels <- c('(to left light)',
#                   '(to right light)',
#                   '(to feeder)',
#                   '(to next trial)')
#     for(zz in 1:4){
#       y_crd <- seq(-1.2,-1.9,length.out = 4)[zz]
#       ev_nm <- paste('z00',zz,sep='')
#       add_event(ev_nm,
#                 y_coord = y_crd,
#                 pch=20+zz,bg='#9922ee88',cex=1.5)
#       text(-max(rt$session_time)*.01,y_crd,
#            paste(z_labels[zz],ev_nm),adj=1,cex=1)
#     }
#   }
#   else{
#     plot(0,type='n',axes=F,ann=F)
#     text(1,0,'NO DATA',font=2,cex=1.75,adj=c(0.5,-1))
#     text(1,0,df,font=2,cex=1.5,adj=c(0.5,2))
#   }
#   # segments(x0=0:ceiling(max(rt$session_time)),
#   #          x1=0:ceiling(max(rt$session_time)),
#   #          y0=rep(y_lims[1],ceiling(tail(rt$session_time,1))),
#   #          y1=rep(y_lims[2],ceiling(tail(rt$session_time,1))),lwd=.5,col='#0088ee22')
# 
# }




















## Exploring Concurrent Output
# rm(list=ls())
# concurrent_data_folder <- '~/Documents/Luis/JAPE/ConcurrentData'
concurrent_data_folder <- '~/Documents/Research/JAPE/ConcurrentData'
setwd(concurrent_data_folder)
ct_full <- read.csv('concurrent_full.csv',stringsAsFactors = F)

  birds <- sort(unique(ct_full$bird))
# bb <- birds[2]
cumulative_responses_bird <- function(bb){
  ld <- subset(ct_full,bird==bb&event%in%c('session_start',
                                           'reinforcer_scheduled_left',
                                           'reinforcer_scheduled_right',
                                           'feeder_on_left',
                                           'response_left_key',
                                           'feeder_on_right',
                                           'response_right_key',
                                           'session_end'))
  print(c(bb,dim(ld)))
  df=ld[order(ld$session),]
  dresp=subset(df,event%in%c('response_left_key','response_right_key',
                             'feeder_on_left','feeder_on_right',
                             'session_end'))
  for(ss in unique(dresp$session)){
    sd <- subset(dresp,session==ss)
    if(ss=='s01'){
      cum_sec <- sd$session_time_sec
    }
    else{
      cum_sec <- append(cum_sec,sd$session_time_sec+max(cum_sec))
    }
  }
  dresp$cum_sec=cum_sec
  
  cum_resp_left <- NULL
  cum_resp_right <- NULL
  cum_reinf_left <- NULL
  cum_reinf_right <- NULL
  c_tt <- 0
  for(tt in dresp$cum_sec){
    c_tt <- c_tt+1
    cum_resp_left[c_tt] <- sum(dresp$cum_sec<=tt&dresp$event=='response_left_key')
    cum_resp_right[c_tt] <- sum(dresp$cum_sec<=tt&dresp$event=='response_right_key')
    cum_reinf_left[c_tt] <- sum(dresp$cum_sec<=tt&dresp$event=='feeder_on_left')
    cum_reinf_right[c_tt] <- sum(dresp$cum_sec<=tt&dresp$event=='feeder_on_right')
    print(paste(round(tt/(60*60),3),'hours processed,','bird',bb,collapse=' '))
  }
  dresp$cum_resp_left <- cum_resp_left
  dresp$cum_resp_right <- cum_resp_right
  dresp$cum_reinf_left <- cum_reinf_left
  dresp$cum_reinf_right <- cum_reinf_right
  
  return(dresp)
}

for(bb in birds[1]){
  cum_responses_bird <- cumulative_responses_bird(bb)
  write.csv(cum_responses_bird,paste('cum_responses_',bb,'.csv',sep=''),row.names=F)
}



# 
# cumulative_stuff <- function(brd,sssn){
#   
#   # brd <- 'p004'
#   # sssn <- 's01'
#   
#   ld <- subset(ct_full,bird==brd&session==sssn)
#   
#   time <- seq(0,ceiling(tail(ld$session_time_sec,1)),1)
#   cum_reinf_left <- NA
#   cum_reinf_right <- NA
#   cum_resp_left <- NA
#   cum_resp_right <- NA
#   c_tt <- 0
#   for(tt in time){
#     c_tt <- c_tt+1
#     # Cum-Cum Responses
#     cum_resp_left[c_tt] <- sum(ld$event=='response_left_key'&ld$session_time_sec<=tt)
#     cum_resp_right[c_tt] <- sum(ld$event=='response_right_key'&ld$session_time_sec<=tt)
#     
#     # Cum-Cum Reinforcers
#     cum_reinf_left[c_tt] <- sum(ld$event=='feeder_on_left'&ld$session_time_sec<=tt)
#     cum_reinf_right[c_tt] <- sum(ld$event=='feeder_on_right'&ld$session_time_sec<=tt)
#   }
#   
#   return(list(time_definition=time,
#               cum_resp_left=cum_resp_left,
#               cum_resp_right=cum_resp_right,
#               cum_reinf_left=cum_reinf_left,
#               cum_reinf_right=cum_reinf_right))
# }
# 
# 
# 
# 

# 
# 
# birds <- sort(unique(ct_full$bird))
# sessions <- sort(unique(ct_full$session))
# 
# brds_info <- vector('list',length(birds))
# # bird <- birds[1]
# for(bb in 1:length(birds)){
#   sssns_info <- vector('list',length(sessions))
#   # session <- sessions[1]
#   for(ss in 1:length(sessions)){
#     cs <- cumulative_stuff(birds[bb],sessions[ss])
#     sssns_info[[ss]] <- cs
#     print(c(birds[bb],sessions[ss]))
#   }
#   brds_info[[bb]] <- sssns_info
# }

# 
# dev.off()
# x11(width=20,height=7)
# layout(matrix(1:12,ncol=6))
# par(mar=rep(2,4),oma=rep(3,4),bg='#000000',fg='#aaaaaa',col.axis='#aaaaaa')
# colors <- c(heat.colors(length(sessions)-2),c('#00dd00','#00dddd'))
# for(bb in 1:length(birds)){
#   
#   # print(c(bb,
#   #         max(c(max(cum_reinf_left),max(cum_reinf_right))),
#   #         max(c(max(cum_resp_left),max(cum_resp_right)))))
#   
#   # plot(NULL,ylim=c(0,4000),xlim=c(0,4000),type='l')
#   plot(NULL,ylim=c(0,1000),xlim=c(0,1000),type='l')
#   abline(0,1,lty='dashed')
#   abline(0,3,lty='solid',col='#888888')
#   mtext(birds[bb],3,cex=1.5,line=1)
#   for(ss in 1:length(sessions)){
#     sd <- brds_info[[bb]][[ss]]
#     lines(sd$cum_resp_right,sd$cum_resp_left,col=colors[ss])
#   }
#   # text(2500,500,paste('# Left:',max(cum_resp_left)),adj=c(1,-1))
#   # text(2500,500,paste('# Right:',max(cum_resp_right)),adj=c(1,.5))
#   # text(2500,500,paste('# Total:',max(cum_resp_left)+max(cum_resp_right)),adj=c(1,2))
#   
#   plot(NULL,ylim=c(0,160),xlim=c(0,160),type='l')
#   abline(0,1,lty='dashed')
#   abline(0,3,lty='solid',col='#888888')
#   for(ss in 1:length(sessions)){
#     sd <- brds_info[[bb]][[ss]]
#     lines(sd$cum_reinf_right,sd$cum_reinf_left,col=colors[ss])
#   }
#   
#   # text(150,25,paste('# Left:',max(cum_reinf_left)),adj=c(1,-1))
#   # text(150,25,paste('# Right:',max(cum_reinf_right)),adj=c(1,.5))
#   # text(150,25,paste('# Total:',max(cum_reinf_left)+max(cum_reinf_right)),adj=c(1,2))
# }
# 
# 

# 
# 
# 
# bb <- birds[3]
# b_data <- subset(ct_full,bird==bb)
# # ss <- sessions[1]
# dev.off()
# x11(width=20,height=10)
# layout(1:length(sessions))
# par(mar=rep(2,4),oma=c(0,0,3,0))
# for(ss in sessions){
#   s_data <- subset(b_data,session==ss)
#   plot_session(s_data)
# }
# mtext(bb,3,outer=T,cex=1.5)
# 
# 
