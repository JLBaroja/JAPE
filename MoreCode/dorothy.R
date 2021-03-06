# Extracts data and builds different human-friendly datasets.
# Name of script, and of main .RData object, honors Dorothy Johnson Vaughan 1910-2008


# rm(list=ls())
concurrent_data_folder <- '~/Documents/Luis/JAPE/ConcurrentData'
setwd(concurrent_data_folder)
japede_full <- read.csv('concurrent_full.csv',stringsAsFactors = F)

source_folder <- '~/Documents/Luis/JAPE/MoreCode/' 
setwd(source_folder)
source('plotting_functions.R')

birdz <- sort(unique(japede_full$bird))
sessionz <- sort(unique(japede_full$session))


dev.off()
x11(width=20,height=10)
layout(matrix(1:(length(birdz)*length(sessionz)),ncol=length(birdz)))
par(mar=rep(1,4),mgp=c(2,0,0),tck=0.02,oma=c(0,0,1,0),bg='#000000',col.axis='#333333',fg='#333333')

brd <- birdz[1]
for(brd in birdz){
  b_data <- subset(japede_full,bird==brd)
  sssn <- sessionz[4]
  for(sssn in sessionz){
    s_data <- subset(b_data,session==sssn)
    
    # 
    rnf_left <- s_data$session_time_sec[which(s_data$event=='feeder_on_left')]
    rnf_right <- s_data$session_time_sec[which(s_data$event=='feeder_on_right')]
    # all_rnf <- sort(c(rnf_left,rnf_right))
    # schdld_left <- s_data$session_time_sec[which(s_data$event=='reinforcer_scheduled_left')]
    # schdld_right <- s_data$session_time_sec[which(s_data$event=='reinforcer_scheduled_right')]
    # schdld_left <- schdld_left[1:length(rnf_left)]
    # schdld_right <- schdld_right[1:length(rnf_right)]
    # 
    # tss_left <- rnf_left-schdld_left
    # tss_right <- rnf_right-schdld_right
    # 
    # 
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
      if(key_rsp[rs]=='right'&key_rsp[(rs-1)]=='left'){join_right[rs] <- T}
      if(rs<length(all_rsp)){
        if(key_rsp[rs]=='left'&key_rsp[(rs+1)]=='right'){leave_left[rs] <- T}
        if(key_rsp[rs]=='right'&key_rsp[(rs+1)]=='left'){leave_right[rs] <- T}
      }
    }
    rnfrcd_left <- rep(F,length(all_rsp))
    rnfrcd_right <- rep(F,length(all_rsp))
    rnfrcd_left[which(all_rsp%in%rnf_left)] <- T
    rnfrcd_right[which(all_rsp%in%rnf_right)] <- T

    
    
    
    # rnf_bfr_leave <- NULL
    # rsp_bfr_leave <- NULL
    # tm_bfr_leave <- NULL
    # tm_bfr_join <- NULL
    # resp_rows <- NULL
    # c_rr <- 0
    # for(rr in all_rnf){
    #   c_rr <- c_rr+1
    #   # rr <- all_rnf[90]
    #   rsp_row <- which(all_rsp==rr)
    #   if(key_rsp[rsp_row]=='left'){
    #     next_leave_row <- which(leave_left&all_rsp>=rr)[1]
    #     # next_leave_row <- sum(c(which(leave_left&all_rsp>=rr)[1],
    #     #                         is.na(which(leave_left&all_rsp>=rr)[1])*length(all_rsp)),na.rm=T)
    #     next_join_row <- which(join_right&all_rsp>=rr)[1]
    #     reinforcer_target <- rnfrcd_left}
    #   if(key_rsp[rsp_row]=='right'){
    #     next_leave_row <- which(leave_right&all_rsp>=rr)[1]
    #     next_join_row <- which(join_left&all_rsp>=rr)[1]
    #     reinforcer_target <- rnfrcd_right}
    #   
    #   if(is.na(next_leave_row)){      
    #     rnf_bfr_leave[c_rr] <- NA
    #     rsp_bfr_leave[c_rr] <- NA
    #     tm_bfr_leave[c_rr] <- NA
    #     tm_bfr_join[c_rr] <- NA
    #   }
    #   else{      
    #     rnf_bfr_leave[c_rr] <- sum(reinforcer_target[rsp_row:next_leave_row])-1
    #     rsp_bfr_leave[c_rr] <- length(rsp_row:next_leave_row)-1
    #     tm_bfr_leave[c_rr] <- all_rsp[next_leave_row]-all_rsp[rsp_row]
    #     tm_bfr_join[c_rr] <- all_rsp[next_join_row]-all_rsp[rsp_row]
    #   }

      
      
      rnf_bfr_leave_rsp <- NULL
      rsp_bfr_leave_rsp <- NULL
      tm_bfr_leave_rsp <- NULL
      tm_bfr_join_rsp <- NULL
      # resp_rows <- NULL
      c_rr <- 0
      for(rr in all_rsp){
        c_rr <- c_rr+1
        # rr <- all_rnf[90]
        rsp_row <- which(all_rsp==rr)
        if(key_rsp[rsp_row]=='left'){
          next_leave_row <- which(leave_left&all_rsp>=rr)[1]
          # next_leave_row <- sum(c(which(leave_left&all_rsp>=rr)[1],
          #                         is.na(which(leave_left&all_rsp>=rr)[1])*length(all_rsp)),na.rm=T)
          next_join_row <- which(join_right&all_rsp>=rr)[1]
          reinforcer_target <- rnfrcd_left}
        if(key_rsp[rsp_row]=='right'){
          next_leave_row <- which(leave_right&all_rsp>=rr)[1]
          next_join_row <- which(join_left&all_rsp>=rr)[1]
          reinforcer_target <- rnfrcd_right}
        
        if(is.na(next_leave_row)){      
          rnf_bfr_leave_rsp[c_rr] <- NA
          rsp_bfr_leave_rsp[c_rr] <- NA
          tm_bfr_leave_rsp[c_rr] <- NA
          tm_bfr_join_rsp[c_rr] <- NA
        }
        else{      
          rnf_bfr_leave_rsp[c_rr] <- sum(reinforcer_target[(rsp_row+1):next_leave_row])#-1
          rsp_bfr_leave_rsp[c_rr] <- length(rsp_row:next_leave_row)-1
          tm_bfr_leave_rsp[c_rr] <- all_rsp[next_leave_row]-all_rsp[rsp_row]
          tm_bfr_join_rsp[c_rr] <- all_rsp[next_join_row]-all_rsp[rsp_row]
        }
        
      # print(c(
      #   # key_rsp[rsp_row],
      #   rr,
      #   rnf_bfr_leave[c_rr],
      #   rsp_bfr_leave[c_rr],
      #   tm_bfr_leave[c_rr],
      #   tm_bfr_join[c_rr]
      # ))
      
      # resp_rows[c_rr] <- rsp_row
    }
    
    # data.frame(
    #   key_rsp[resp_rows],
    #   all_rnf,#[1:(c_rr-1)],
    #   rnf_bfr_leave,
    #   rsp_bfr_leave,
    #   tm_bfr_leave,
    #   tm_bfr_join
    # )

    
        # responses[rsp_row:next_join_row,]
    # 
    # dev.off()
    # layout(1:2)
    # par(mar=rep(3,4))
    # plot_session(s_data,time_zoom = c(0,50),reinf_details = T)
    # plot_session(s_data,time_zoom = c(1335,1375),reinf_details = T)
    # # plot_session(s_data)
    
    
    # 
    # tsls_left <- NA
    # if(length(rnf_left)>=2){
    #   for(rr in 2:length(rnf_left)){tsls_left[rr] <- rnf_left[rr]-rnf_left[(rr-1)]}}
    # tsls_right <- NA
    # for(rr in 2:length(rnf_right)){tsls_right[rr] <- rnf_right[rr]-rnf_right[(rr-1)]}
    # tsls_any <- NA
    # for(rr in 2:length(all_rnf)){tsls_any[rr] <- all_rnf[rr]-all_rnf[(rr-1)]}
    # 
    # key_rnf <- NULL
    # n_rnf_left <- rep(NA,length(all_rnf))
    # n_rnf_right <- rep(NA,length(all_rnf))
    # time_since_scheduled <- NULL
    # time_since_last_in_same <- NULL
    # 
    # key_rnf[which(all_rnf%in%rnf_left)] <- 'left'
    # key_rnf[which(all_rnf%in%rnf_right)] <- 'right'
    # # n_rnf_left[which(all_rnf%in%rnf_left)] <- 1:length(rnf_left)
    # # n_rnf_right[which(all_rnf%in%rnf_right)] <- 1:length(rnf_right)
    # n_rnf_left[which(key_rnf=='left')] <- 1:length(rnf_left)
    # n_rnf_right[which(key_rnf=='right')] <- 1:length(rnf_right)
    # time_since_scheduled[which(key_rnf=='left')] <- tss_left 
    # time_since_scheduled[which(key_rnf=='right')] <- tss_right
    # time_since_last_in_same[which(key_rnf=='left')] <- tsls_left 
    # time_since_last_in_same[which(key_rnf=='right')] <- tsls_right
    # 
    
    
    plot(NULL,
         ylim=c(0,30),
         xlim=c(0,max(s_data$session_time_sec)),
         axes=F)
    # plot(NULL,
    #      ylim=c(0,60),
    #      xlim=c(0,60),
    #      axes=F)
    axis(1)
    axis(2)
    # points(schdld_left,tss_left,pch=16,col='#0000aaaa',cex=1,type='l',lwd=2)
    # points(schdld_right,tss_right,pch=16,col='#aa0000aa',cex=1,type='l',lwd=2)
    # points(rnf_left,tsls_left,pch=16,col='#0000aaaa',cex=1,type='l',lwd=2)
    # points(rnf_right,tsls_right,pch=16,col='#aa0000aa',cex=1,type='l',lwd=2)
    # points(rnf_left,rnf_bfr_leave[key_rnf=='left'],pch=16,col='#dd88dd',cex=1,type='l',lwd=1)
    # points(rnf_right,rnf_bfr_leave[key_rnf=='right'],pch=16,col='#dddd88',cex=1,type='l',lwd=1)
    # points(tm_bfr_leave[key_rnf=='left'],rsp_bfr_leave[key_rnf=='left'],pch=16,col='#dd88dd88',cex=.8,type='p',lwd=1)
    # points(tm_bfr_leave[key_rnf=='right'],rsp_bfr_leave[key_rnf=='right'],pch=16,col='#88dddd88',cex=.8,type='p',lwd=1)
    # points(all_rsp[key_rsp=='left'],rsp_bfr_leave_rsp[key_rsp=='left'],pch=16,col='#dd88dd88',cex=.8,type='l',lwd=1)
    # points(all_rsp[key_rsp=='right'],rsp_bfr_leave_rsp[key_rsp=='right'],pch=16,col='#8899ccbb',cex=.8,type='l',lwd=1)
    points(all_rsp[key_rsp=='left'],tm_bfr_leave_rsp[key_rsp=='left'],pch=16,col='#dd88dd88',cex=.8,type='l',lwd=1)
    points(all_rsp[key_rsp=='right'],tm_bfr_leave_rsp[key_rsp=='right'],pch=16,col='#88bbddaa',cex=.8,type='l',lwd=1)
    # points(all_rsp[key_rsp=='left'],rnf_bfr_leave_rsp[key_rsp=='left'],pch=16,col='#dd88dd88',cex=.8,type='o',lwd=1)
    # points(all_rsp[key_rsp=='right'],rnf_bfr_leave_rsp[key_rsp=='right'],pch=16,col='#88dddd88',cex=.8,type='o',lwd=1)
    # points(tm_bfr_leave_rsp[key_rsp=='left'],rsp_bfr_leave_rsp[key_rsp=='left'],pch=16,col='#dd88dd88',cex=.2,type='p',lwd=1)
    # points(tm_bfr_leave_rsp[key_rsp=='right'],rsp_bfr_leave_rsp[key_rsp=='right'],pch=16,col='#88dddd88',cex=.2,type='p',lwd=1)
    # points(rnf_left,rsp_bfr_leave[key_rnf=='left'],pch=16,col='#dd88dd88',cex=.8,type='o',lwd=1)
    # points(rnf_right,rsp_bfr_leave[key_rnf=='right'],pch=16,col='#88dddd88',cex=.8,type='o',lwd=1)
    # points(rnf_left,tm_bfr_join[key_rnf=='left'],pch=16,col='#dd88dd',cex=1,type='l',lwd=1)
    # points(rnf_right,tm_bfr_join[key_rnf=='right'],pch=16,col='#dddd88',cex=1,type='l',lwd=1)
    # points(rnf_left,tm_bfr_leave[key_rnf=='left'],pch=16,col='#dd88dd88',cex=.6,type='o',lwd=1)
    # points(rnf_right,tm_bfr_leave[key_rnf=='right'],pch=16,col='#88dddd88',cex=.6,type='o',lwd=1)
    # points(all_rnf,tsls_any,pch=16,col='#aaaaaa',cex=1,type='l',lwd=1)
    # points(rnf_left,tsls_any[which(key_rnf=='left')],pch=16,col='#0000aaaa',cex=1,type='l',lwd=2)
    # points(rnf_right,tsls_any[which(key_rnf=='right')],pch=16,col='#aa0000aa',cex=1,type='l',lwd=2)
    # hist(tsls_left,xlim=c(0,600),breaks=seq(0,600,10))
    # hist(tsls_right,xlim=c(0,600),breaks=seq(0,600,10))
    
    # if(sssn=='s01'){mtext(brd,3)}
    
    # reinf_in_session <- seq(1,sum(c(sum(s_data$event=='feeder_on_left'),sum(s_data$event=='feeder_on_right'))))
    #

    
    # responses <- data.frame(bird=brd,
    #                         session=sssn,
    #                         all_rsp,
    #                         key_rsp,
    #                         join_left,
    #                         join_right,
    #                         leave_left,
    #                         leave_right,
    #                         rnfrcd_left,
    #                         rnfrcd_right
    # );head(responses,2);tail(responses,2)
    # 
    # reinforcers <- data.frame(bird=brd,
    #                           session=sssn,
    #                           n_rnf_left,
    #                           n_rnf_right,
    #                           key_rnf,
    #                           moment_of_delivery=all_rnf,
    #                           time_since_scheduled,
    #                           time_since_last_in_same,
    #                           rnf_bfr_leave,
    #                           rsp_bfr_leave,
    #                           tm_bfr_leave,
    #                           tm_bfr_join
    # );head(reinforcers,2);tail(reinforcers,2)
    
  }
}



