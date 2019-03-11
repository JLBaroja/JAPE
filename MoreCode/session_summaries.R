# rm(list=ls())
# 
# session_summary <- function(bb,ss){
#   ssdt <- read.csv(paste(bb,'s',sprintf('%02d',ss),'.csv',sep=''))
#   session_info <- strsplit(as.character(unique(ssdt$med_notation_file)),split='_')[[1]]
#   L1 <- as.numeric(session_info[3])
#   L2 <- as.numeric(session_info[4])
#   R1 <- as.numeric(session_info[7])
#   R2 <- as.numeric(session_info[8])
#   print(c(bb,ss))#,unique(ssdt$first_cp_left)==unique(ssdt$first_cp_right)))
#   sess_summ <- data.frame(session=tolower(unique(ssdt$session)),
#                           bird=tolower(unique(ssdt$bird)),
#                           VIleft1=L1,VIleft2=L2,VIright1=R1,VIright2=R2,
#                           CPright=unique(ssdt$first_cp_right),
#                           CPleft=unique(ssdt$first_cp_left))
#   return(sess_summ)
# }
# 
# birds <- c('p004','p054','p138','p510','p530','p736')
# # bb <- birds[1]
# # ss <- 18
# 
# 
# sessions <- data.frame(NULL)
# for(bb in birds){
#   for(ss in 1:180){
#     sessions <- rbind(sessions,session_summary(bb,ss))
#   }
# }
# 
# sessions$dynamic_env <- sessions$VIleft1!=sessions$VIleft2
# 
# sessions$conditions_left <- paste(sessions$VIleft1,sessions$VIleft2,sep='_')
# sessions$conditions_right <- paste(sessions$VIright1,sessions$VIright2,sep='_')
# 
# sessions$env_1 <- paste(sessions$VIleft1,sessions$VIright1,sep='_')
# sessions$env_2 <- paste(sessions$VIleft2,sessions$VIright2,sep='_')
# 
# setwd('~/Documents/Research/JAPE/ConcurrentData/')
# write.csv(sessions,file='session_summary.csv',row.names = F)

rm(list=ls())
setwd('~/Documents/Research/JAPE/ConcurrentData/')
sessions <- read.csv('session_summary.csv',stringsAsFactors = F)

for(bb in unique(sessions$bird)){
  sbst <- subset(sessions,bird==bb)
  print(sum(sbst$dynamic_env))
}

summary(sessions$CPleft[sessions$dynamic_env]/60)

chng_ss <- subset(sessions,dynamic_env==T)
no_chng_ss <- subset(sessions,dynamic_env==F)
length(unique(chng_ss$VIright1))

summary(
  (60/chng_ss$VIleft1)/
    (60/chng_ss$VIright1)
)
summary(
  (60/chng_ss$VIleft2)/
    (60/chng_ss$VIright2)
)

for(sch in unique(chng_ss$VIleft1)){
  sbst <- subset(chng_ss,VIleft1==sch)
  # print(unique(sbst$VIleft1))
  print(length(unique(sbst$VIleft2)))
}
for(sch in unique(chng_ss$VIright1)){
  sbst <- subset(chng_ss,VIright1==sch)
  # print(unique(sbst$VIright1))
  print(length(unique(sbst$VIright2)))
}

for(sch in unique(chng_ss$env_1)){
  sbst <- subset(chng_ss,env_1==sch)
  # print(unique(sbst$env_1))
  print(length(unique(sbst$env_2)))
}
for(sch in unique(chng_ss$env_2)){
  sbst <- subset(chng_ss,env_2==sch)
  # print(unique(sbst$env_2))
  print(length(unique(sbst$env_1)))
}


for(bb in unique(chng_ss$bird)){
  chng_ss_bb <- subset(chng_ss,bird=bb)
  print(c(bb,length(unique(chng_ss_bb$env_1))))
  for(sch in unique(chng_ss_bb$env_1)){
    sbst <- subset(chng_ss_bb,env_1==sch)
    # print(unique(sbst$env_1))
    print(length(unique(sbst$env_2)))
  }
}

for(sch in unique(chng_ss_bb$env_2)){
  sbst <- subset(chng_ss_bb,env_2==sch)
  # print(unique(sbst$env_2))
  print(length(unique(sbst$env_1)))
}


envir_1 <- c('24_360', 
             '25_225', 
             '27_135', 
             '30_90',  
             '35_63', 
             '45_45', 
             '63_35',  
             '90_30', 
             '135_27', 
             '225_25', 
             '360_24',
             'Inf_Inf') 

bb <- 'p530'
try(dev.off())
x11(width=12,height=9)
layout(matrix(1:12,ncol=4))
for(env in envir_1){
  # env <- envir_1[3]
  target_sess <- subset(chng_ss,bird==bb&env_2==env)
  plot(NULL,xlim=c(-1000,1000),ylim=c(-1000,1000))
  for(ss in unique(target_sess$session)){
    c_cnts <- cum_counts(bb,ss)
    lines(c_cnts$rel_cum_list$rel_cum_resp_right,
          c_cnts$rel_cum_list$rel_cum_resp_left)
  }
  points(0,0,pch=21,bg='#ffffff88',cex=10)
}



birds <- c('p004','p054','p138','p510','p530','p736')
try(dev.off())
x11(width=12,height=9)
layout(matrix(1:12,ncol=4))
for(env in envir_1){
  plot(NULL,xlim=c(-1000,1000),ylim=c(-1000,1000))
  for(bb in birds){
    # env <- envir_1[3]
    target_sess <- subset(chng_ss,bird==bb&env_1==env)
    for(ss in unique(target_sess$session)){
      c_cnts <- cum_counts(bb,ss)
      lines(c_cnts$rel_cum_list$rel_cum_resp_right,
            c_cnts$rel_cum_list$rel_cum_resp_left)
    }
    points(0,0,pch=21,bg='#ffffff88',cex=10)
  }
}


source('~/Documents/Research/JAPE/Reports/ISCP 2018/cum_records.R')
# birds <- c('p004','p054','p138','p510','p530','p736')
# head(chng_ss)


# plot(NULL,xlim=c(-1,1),ylim=c(-1,1))
ww_b_chng <- NA
ww_a_chng <- NA
rr_b_chng <- NA
rr_a_chng <- NA
cm_cnts_ch_rsp_left <- array(dim=c(nrow(chng_ss),360)) 
cm_cnts_ch_rsp_right <- array(dim=c(nrow(chng_ss),360)) 
row_names <- NA
birds <- NA
for(rr in 1:nrow(chng_ss)){
  # for(rr in 1:3){
  mtch <- matching_ratios(chng_ss$bird[rr],chng_ss$session[rr])  
  cnts <- cum_counts(chng_ss$bird[rr],chng_ss$session[rr])  
  row_name <- paste(chng_ss[rr,c('env_1','env_2')],collapse='_to_')
  cm_cnts_ch_rsp_left[rr,] <- cnts$rel_cum_list$rel_cum_resp_left
  cm_cnts_ch_rsp_right[rr,] <- cnts$rel_cum_list$rel_cum_resp_right
  row_names[rr] <- row_name
  birds[rr] <- chng_ss$bird[rr]
  ww_b_chng[rr] <- mtch$reinf_ratio_b
  ww_a_chng[rr] <- mtch$reinf_ratio_a
  rr_b_chng[rr] <- mtch$resp_ratio_b
  rr_a_chng[rr] <- mtch$resp_ratio_a
  print(rr)
}

row.names(cm_cnts_ch_rsp_left) <- row_names
row.names(cm_cnts_ch_rsp_right) <- row_names
ch_plotting_stuff <- list(cm_cnts_ch_rsp_left=cm_cnts_ch_rsp_left,
                          cm_cnts_ch_rsp_right=cm_cnts_ch_rsp_right,
                          ww_b_chng=ww_b_chng,
                          ww_a_chng=ww_a_chng,
                          rr_b_chng=rr_b_chng,
                          rr_a_chng=rr_a_chng,
                          birds=birds)
save(ch_plotting_stuff,
     file='~/Documents/Research/JAPE/Reports/ISCP 2018/ch_plttng_stff.RData')





load(file='~/Documents/Research/JAPE/Reports/ISCP 2018/ch_plttng_stff.RData')
environmentz <- c('24_360', 
                  '25_225', 
                  '27_135', 
                  '30_90',  
                  '35_63', 
                  '45_45', 
                  '63_35',  
                  '90_30', 
                  '135_27', 
                  '225_25', 
                  '360_24',
                  'Inf_Inf')
cc_R_left <- ch_plotting_stuff$cm_cnts_ch_rsp_left
cc_R_right <- ch_plotting_stuff$cm_cnts_ch_rsp_right

envr <- environmentz[3]
starting_sessions <- F
# cum_records <- function(envr,
#                         starting_sessions=T){
#   
#   sch <- as.numeric(strsplit(envr,split='_')[[1]])
#   
#   if(starting_sessions){
#     trgt_rows <- which(startsWith(row.names(cc_R_left),envr))
#     plot_legend <- 'sessions that started in'
#   }
#   if(!starting_sessions){
#     trgt_rows <- which(endsWith(row.names(cc_R_left),envr))
#     plot_legend <- 'sessions that ended in'
#   }
#   # try(dev.off())
#   # x11(width=4,height=4)
#   resp_zoom <- 1000
#   plot(NULL,xlim=c(-resp_zoom,resp_zoom),ylim=c(-resp_zoom,resp_zoom))
#   for(tr in trgt_rows){
#     lines(cc_R_right[tr,],cc_R_left[tr,])
#   }
#   # abline(0,sch[2]/sch[1],col='#dd6600',lwd=2)
#   slope <- sch[2]/sch[1]
#   if(starting_sessions){
#     if(slope<1){
#       x_crds <- c(-resp_zoom,0)
#       y_crds <- c(-resp_zoom*slope,0)}
#     if(slope>=1){
#       y_crds <- c(-resp_zoom,0)
#       x_crds <- c(-resp_zoom/slope,0)
#     }
#   }
#   if(!starting_sessions){
#     if(slope<1){
#       x_crds <- c(resp_zoom,0)
#       y_crds <- c(resp_zoom*slope,0)}
#     if(slope>=1){
#       y_crds <- c(resp_zoom,0)
#       x_crds <- c(resp_zoom/slope,0)
#     }
#   }
#   lines(x_crds,y_crds,col='#dd6600',lwd=2)
#   mtext(paste('Left VI',sch[1],'- Right VI',sch[2]),font=2,cex=1.5,family='CM Roman')
#   mtext(plot_legend,line=2,cex=1,family='CM Roman')
# }
# text(x_crds[2],y_crds[2],'matching',adj=c(0,0),col='red',srt=tanh(slope))


as.numeric(chng_ss$bird)
try(dev.off())
x11(width=5,height=5)
plot(NULL,xlim=c(-5,5),ylim=c(-5,5));abline(0,1,lty='dashed')
# points(log2(ww_b_chng),log2(rr_b_chng),col=as.numeric(chng_ss$bird))
points(log2(ww_a_chng),log2(rr_a_chng),pch=16,col=as.numeric(chng_ss$bird))




# plot(NULL,xlim=c(-1,1),ylim=c(-1,1))
ww_b_nochng <- NA
ww_a_nochng <- NA
rr_b_nochng <- NA
rr_a_nochng <- NA
birds <- NA
for(rr in 1:nrow(no_chng_ss)){
  mtch <- matching_ratios(no_chng_ss$bird[rr],no_chng_ss$session[rr])  
  birds[rr] <- no_chng_ss$bird[rr]
  ww_b_nochng[rr] <- mtch$reinf_ratio_b
  ww_a_nochng[rr] <- mtch$reinf_ratio_a
  rr_b_nochng[rr] <- mtch$resp_ratio_b
  rr_a_nochng[rr] <- mtch$resp_ratio_a
  print(rr)
}

as.numeric(no_chng_ss$bird)
try(dev.off())
x11(width=5,height=5)
plot(NULL,xlim=c(-5,5),ylim=c(-5,5));abline(0,1,lty='dashed')
points(log2(ww_b_nochng),log2(rr_b_nochng),col=as.numeric(no_chng_ss$bird))
# points(log2(ww_a_nochng),log2(rr_a_nochng),pch=16,col=as.numeric(no_chng_ss$bird))









