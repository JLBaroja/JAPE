

plot_session <- function(s_data,
                         time_zoom = NULL,
                         reinf_details=F){
  if(length(time_zoom)!=2){
    if(class(time_zoom)=='NULL'){time_zoom=c(0,max(s_data$session_time_sec))}
    else if(time_zoom=='end'){time_zoom=c(max(s_data$session_time_sec)-200,
                                          max(s_data$session_time_sec))}
    else if(time_zoom=='start'){time_zoom=c(0,200)}
  }
  # time_zoom <- c(0,max(s_data$session_time_sec))
  par(bg='#000000',col.axis='#666666',fg='#666666')
  plot(NULL,ylim=c(-1,1),xlim=time_zoom,axes=F)
  axis(1)
  segments(
    x0=s_data$session_time_sec[s_data$event=='response_left_key'],
    x1=s_data$session_time_sec[s_data$event=='response_left_key'],
    y0=rep(0.2-.1,sum(s_data$event=='response_left_key')),
    y1=rep(0.2+.1,sum(s_data$event=='response_left_key')),
    lwd=0.5,col='#bbbbff')
  segments(
    x0=s_data$session_time_sec[s_data$event=='response_right_key'],
    x1=s_data$session_time_sec[s_data$event=='response_right_key'],
    y0=rep(-0.2-.1,sum(s_data$event=='response_right_key')),
    y1=rep(-0.2+.1,sum(s_data$event=='response_right_key')),
    lwd=0.5,col='#bbbbff')
  points(s_data$session_time_sec[s_data$event=='reinforcer_scheduled_left'],
         rep(0.4,sum(s_data$event=='reinforcer_scheduled_left')),pch=4,lwd=1,col='#dd8800')
  points(s_data$session_time_sec[s_data$event=='reinforcer_scheduled_right'],
         rep(-0.4,sum(s_data$event=='reinforcer_scheduled_right')),pch=4,lwd=1,col='#dd8800')
  points(s_data$session_time_sec[s_data$event=='feeder_on_left'],
         rep(0.5,sum(s_data$event=='feeder_on_left')),pch=4,cex=1,lwd=1,col='#dd2200')
  points(s_data$session_time_sec[s_data$event=='feeder_on_right'],
         rep(-0.5,sum(s_data$event=='feeder_on_right')),pch=4,cex=1,lwd=1,col='#dd2200')
  if(reinf_details){
    text(s_data$session_time_sec[s_data$event=='feeder_on_left'],
         rep(0.5,sum(s_data$event=='feeder_on_left')),
         paste(s_data$session_time_sec[s_data$event=='feeder_on_left']),
         pch=4,cex=1,lwd=2,col='#dd2200',srt=90,adj=-.25)
    text(s_data$session_time_sec[s_data$event=='feeder_on_right'],
         rep(-0.5,sum(s_data$event=='feeder_on_right')),
         paste(s_data$session_time_sec[s_data$event=='feeder_on_right']),
         pch=4,cex=1,lwd=2,col='#dd2200',srt=90,adj=1.25)
    
  }
}


plot_all_sessions <- function(tz=NULL){
  birds <- sort(unique(japede_full$bird))
  sessions <- sort(unique(japede_full$session))
  
  # dev.off()
  x11(width=20,height=8.5)
  layout(matrix(1:(length(birds)*length(sessions)),ncol=length(birds)))
  par(mar=rep(1,4),mgp=c(2,0,0),tck=0.02,oma=c(0,0,1,0))
  for(bbb in birds){
    for(sss in sessions){
      l_data <- subset(japede_full,bird==bbb&session==sss)
      plot_session(l_data,time_zoom = tz)
      print(c(bbb,sss))
      print(head(l_data,3))
      if(sss==sessions[1]){
        mtext(bbb,3,col='#eeee00ff')
      }
    }
  }
}
