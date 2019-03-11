
rm(list=ls())

plt_session <- function(bb,ss,
                        start=NULL,
                        end=NULL){
  # bb <- 'p004'
  # ss <- 3
  setwd('~/Documents/Research/JAPE/ConcurrentData/CSV files/')
  data_file <- paste(bb,'s',sprintf('%02d',ss),'.csv',sep='')
  bsdat <<- read.csv(data_file)
  if(class(start)=='NULL'){
    start <- bsdat$session_time_sec[1]}
  if(class(end)=='NULL'){
    end <- tail(bsdat$session_time_sec,1)}
  
  try(dev.off())
  x11(width = 8,height=8)
  layout(rbind(rep(1,3),rep(2,3),rep(3,3),c(4:6)))
  par(bg='#000000',fg='#337733',col.axis='#00bb00',
      mar=rep(0,4),oma=rep(4,4))
  
  
  add_segments <- function(vnt,at_y,
                           width=.2,
                           color='#dddddd'){
    # vnt <- 'feeder_on_left'
    # at_y <- 0.5
    times <- bsdat$session_time_sec[bsdat$event==vnt]
    segments(x0=times,x1=times,
             y0=rep(at_y,length(times))-width/2,
             y1=rep(at_y,length(times))+width/2,
             col=color)
    # points(times,rep(at_y,length(times)),col='#ee9922')
  }
  
  plot(NULL,xlim=c(start-(end-start)*.1,end),ylim=c(-1,1))
  text(start,.9,data_file,cex=2,col='#44dd22',adj=0)
  text(end,.9,unique(bsdat$med_notation_file),cex=1.2,col='#44dd22',adj=1)
  add_segments('feeder_on_left',0.25,.2,'#dd0000')
  add_segments('feeder_on_right',-0.25,.2,'#dd0000')
  add_segments('response_left_key',0.5,.1,'#dd8800')
  add_segments('response_right_key',-0.5,.1,'#dd8800')
  
  # cumulative_record_old_fashion <- function(vnt='response',color='#cccccc'){
  #   vnt='response'
  #   color='#cccccc'
  #   plot(NULL,xlim=c(max))
  # }
  
  # cumulative_record_old_fashion()
  
  cumulative_record <- function(vnt='response',
                                time_definition=1,
                                color='#cccccc',
                                old_fashioned=F,
                                n_resp_cut=200){
    if(vnt=='response'){
      vnt_left_lab <- paste(vnt,'_left_key',sep='')
      vnt_right_lab <- paste(vnt,'_right_key',sep='')
    }
    if(vnt=='feeder'){
      vnt_left_lab <- paste(vnt,'_on_left',sep='')
      vnt_right_lab <- paste(vnt,'_on_right',sep='')
    }
    if(!old_fashioned){
      cum_left <- NULL
      cum_right <- NULL
      times <- seq(start,end,time_definition)
      for(tt in times){
        cum_left <- append(cum_left,sum(bsdat$event==vnt_left_lab&bsdat$session_time_sec<=tt))
        cum_right <- append(cum_right,sum(bsdat$event==vnt_right_lab&bsdat$session_time_sec<=tt))
      }
      plot(NULL,
           xlim=c(min(c(min(cum_left),min(cum_right))),max(c(max(cum_left),max(cum_right)))),
           ylim=c(min(c(min(cum_left),min(cum_right))),max(c(max(cum_left),max(cum_right)))))
      abline(0,1,lty='dashed',col='#10a943aa')
      qrtrs <- seq(0,3600,900)
      points(cum_right[times%in%qrtrs],cum_left[times%in%qrtrs],
             pch=21,cex=2,col='#22ddee',bg='#1133ee')
      qrtrs <- seq(0,3600,300)[!seq(0,3600,300)%in%qrtrs]
      points(cum_right[times%in%qrtrs],cum_left[times%in%qrtrs],
             pch=21,cex=1,col='#22ddee',bg='#1133ee')
      lines(cum_right,cum_left,col=color)
    }
    
    if(old_fashioned){
      times_left <- bsdat$session_time_sec[bsdat$event==vnt_left_lab]
      times_right <- bsdat$session_time_sec[bsdat$event==vnt_right_lab]
      cum_left <- cumsum(bsdat$event==vnt_left_lab)[bsdat$event==vnt_left_lab]
      cum_right <- cumsum(bsdat$event==vnt_right_lab)[bsdat$event==vnt_right_lab]
      ref_cut_left <- which(cum_left%in%seq(n_resp_cut,by=n_resp_cut,length.out = length(cum_left))) # Still too big!
      ref_cut_right <- which(cum_right%in%seq(n_resp_cut,by=n_resp_cut,length.out = length(cum_right))) # Still too big!
      plot(NULL,xlim=c(start-(end-start)*.1,end),ylim=c(0,n_resp_cut))
      r_rr <- 1
      c_rr <- 0
      for(rr in ref_cut_left){
        cum_left[r_rr:rr] <- cum_left[r_rr:rr]-c_rr*n_resp_cut
        lines(times_left[r_rr:rr],
              cum_left[r_rr:rr],col=color)
        r_rr <- rr+1
        c_rr <- c_rr+1
      }
      r_rr <- 1
      c_rr <- 0
      for(rr in ref_cut_right){
        cum_right[r_rr:rr] <- cum_right[r_rr:rr]-c_rr*n_resp_cut
        lines(times_right[r_rr:rr],
              cum_right[r_rr:rr],col='orange')
        r_rr <- rr+1
        c_rr <- c_rr+1
      }
      # lines(times_left,cum_left,col=color)
      # lines(times_right,cum_right,col='orange')
    }
  }
  
  cumulative_record(old_fashioned = T,vnt = 'feeder',n_resp_cut = 15)
  cumulative_record(old_fashioned = T)
  
  par(mar=rep(2,4))
  cumulative_record()
  cumulative_record('feeder')
}

# plt_session('p054',145)
plt_session('p510',177)
# plt_session('p530',177,start = 900,end=2700)

# Old-fashined cum records
# "Reinforcer impulses" after each one