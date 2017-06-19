rm(list=ls())
data_folder <- '~/Documents/Luis/JAPE/AutoShapingData/'
# data_folder <- '~/Documents/Research/JAPE/AutoShapingData/'

setwd(data_folder)
realtime <- read.csv('autoshaping_data.csv')

add_event <- function(h_event,
                      y_coord,
                      ...) {
  ix_event <- which(rt$event==h_event)
  points(rt$session_time[ix_event],rep(y_coord,length(ix_event)),...)
}

interval_event <- function(event_name,
                           y_coord,
                           bw=.15,
                           lab_on='_on',
                           lab_off='_off',
                           color='#0055ff66') {
  
  # event_name <- 'max_light'
  # y_coord <- .5
  # band_width <- .1
  
  name_on <- paste(event_name,lab_on,sep='')
  name_off <- paste(event_name,lab_off,sep='')
  times_on <<- rt$session_time[which(rt$event==name_on)]
  times_off <<- rt$session_time[which(rt$event==name_off)]
  
  if(length(times_on)>0){
    for(tt in 1:length(times_on)){
      polygon(x=c(rep(times_on[tt],2),rep(times_off[tt],2)),
              y=c(y_coord+c(+bw,-bw,-bw,+bw)/2),col=color)
      
    }
  }
  add_event(name_on,y_coord=y_coord+bw*.8,pch=25,bg=color) 
  add_event(name_off,y_coord=y_coord-bw*.8,pch=24,bg=color) 
  
  text(-max(rt$session_time)*.01,y_coord,event_name,adj=1,cex=1,font=2)
}


# birds <- unique(realtime$subject)
birds <- c('p051','p510','p054','p530',
           'p056','p736','p004','p138')
sessions <- rep(c('s01','s02'),each=length(birds))
data_files <- paste(birds,sessions,'atsh',sep='_')

# dev.off()
# x11(width = 20, height = 10)
layout(matrix(1:8,ncol=2))
for(df in data_files){
  rt <- subset(realtime,data_file==df)
  if(nrow(rt)>0){
    y_lims <- c(-2,1)
    par(bg='#cccccc',col.axis='#555555',fg='#333333',
        mar=c(1,0.5,1,1))
    plot(0,type='n',
         xlim=c(-max(realtime$session_time)*.06,
                max(realtime$session_time)*1.075),
         ylim=c(-2,.6),
         ann=F,axes=F)
    mtext(df,4,line=-4,font=2)
    mtext(paste('Box:',unique(rt$box)),4,line=-2,font=2)
    axis(1,at=0:ceiling(max(rt$session_time)),cex.axis=.6)
    
    interval_event('central_light',0,color = '#33cc0066')  
    add_event('resp_central_key',0,pch=4,cex=1.5,col='#ee0000')
    interval_event('left_light',0.4,color = '#33cc0066')  
    add_event('resp_left_key',0.4,pch=4,cex=1.5,col='#ee0000')
    interval_event('right_light',-0.4,color = '#33cc0066')  
    add_event('resp_right_key',-0.4,pch=4,cex=1.5,col='#ee0000')
    interval_event('feeder',-.85,color = '#ee770066')  
    
    z_labels <- c('(to left light)',
                  '(to right light)',
                  '(to feeder)',
                  '(to next trial)')
    for(zz in 1:4){
      y_crd <- seq(-1.2,-1.9,length.out = 4)[zz]
      ev_nm <- paste('z00',zz,sep='')
      add_event(ev_nm,
                y_coord = y_crd,
                pch=20+zz,bg='#9922ee88',cex=1.5)
      text(-max(rt$session_time)*.01,y_crd,
           paste(z_labels[zz],ev_nm),adj=1,cex=1)
    }
  }
  else{
    plot(0,type='n',axes=F,ann=F)
    text(1,0,'NO DATA',font=2,cex=1.75,adj=c(0.5,-1))
    text(1,0,df,font=2,cex=1.5,adj=c(0.5,2))
  }
  # segments(x0=0:ceiling(max(rt$session_time)),
  #          x1=0:ceiling(max(rt$session_time)),
  #          y0=rep(y_lims[1],ceiling(tail(rt$session_time,1))),
  #          y1=rep(y_lims[2],ceiling(tail(rt$session_time,1))),lwd=.5,col='#0088ee22')
  
}

