
# bb <- 'p510'
# ss <- 3
cum_counts <- function(bb,ss){
  # ssdt <- read.csv(paste('~/Documents/Research/JAPE/ConcurrentData/CSV files/',bb,'s',sprintf('%02d',ss),'.csv',sep=''))
  ssdt <- read.csv(paste('~/Documents/Research/JAPE/ConcurrentData/CSV files/',bb,ss,'.csv',sep=''))
  
  chpt <- unique(ssdt$first_cp_left)
  
  definition <- 10
  breaks <- seq(0,max(ssdt$session_time_sec),definition)
  
  cum_resp_left <- NA
  cum_resp_right <- NA
  cum_reinf_left <- NA
  cum_reinf_right <- NA
  for(br in 2:length(breaks)){
    cum_resp_left[(br-1)] <- sum(ssdt$event[ssdt$session_time_sec<=breaks[br]]=='response_left_key')
    cum_resp_right[(br-1)] <- sum(ssdt$event[ssdt$session_time_sec<=breaks[br]]=='response_right_key')
    cum_reinf_left[(br-1)] <- sum(ssdt$event[ssdt$session_time_sec<=breaks[br]]=='feeder_on_left')
    cum_reinf_right[(br-1)] <- sum(ssdt$event[ssdt$session_time_sec<=breaks[br]]=='feeder_on_right')
  }
  rel_cum_resp_left <- cum_resp_left-cum_resp_left[which(breaks>=chpt)[1]]
  rel_cum_resp_right <- cum_resp_right-cum_resp_right[which(breaks>=chpt)[1]]
  rel_cum_reinf_left <- cum_reinf_left-cum_reinf_left[which(breaks>=chpt)[1]]
  rel_cum_reinf_right <- cum_reinf_right-cum_reinf_right[which(breaks>=chpt)[1]]
  
  cum_list <- list(cum_resp_left=cum_resp_left,
                   cum_resp_right=cum_resp_right,
                   cum_reinf_left=cum_reinf_left,
                   cum_reinf_right=cum_reinf_right)
  rel_cum_list <- list(rel_cum_resp_left=rel_cum_resp_left,
                       rel_cum_resp_right=rel_cum_resp_right,
                       rel_cum_reinf_left=rel_cum_reinf_left,
                       rel_cum_reinf_right=rel_cum_reinf_right)
  return(list(cum_list=cum_list,rel_cum_list=rel_cum_list))
}
# plot(rel_cum_resp_right,rel_cum_resp_left)

# cum_counts('p530',20)


matching_ratios <- function(bb,ss){
  # bb <- 'p510'
  # ss <- 's38'
  ssdt <- read.csv(paste('~/Documents/Research/JAPE/ConcurrentData/CSV files/',bb,ss,'.csv',sep=''))
  
  chpt <- unique(ssdt$first_cp_left)
  
  n_resp_left_before <- sum(ssdt$event=='response_left_key'&ssdt$session_time_sec<chpt)
  n_resp_left_after <- sum(ssdt$event=='response_left_key'&ssdt$session_time_sec>chpt)
  n_resp_right_before <- sum(ssdt$event=='response_right_key'&ssdt$session_time_sec<chpt)
  n_resp_right_after <- sum(ssdt$event=='response_right_key'&ssdt$session_time_sec>chpt)
  # resp_ratio_b <- n_resp_left_before/(n_resp_left_before+n_resp_right_before)
  # resp_ratio_a <- n_resp_left_after/(n_resp_left_after+n_resp_right_after)
  resp_ratio_b <- n_resp_left_before/n_resp_right_before
  resp_ratio_a <- n_resp_left_after/n_resp_right_after
  n_reinf_left_before <- sum(ssdt$event=='feeder_on_left'&ssdt$session_time_sec<chpt)
  n_reinf_left_after <- sum(ssdt$event=='feeder_on_left'&ssdt$session_time_sec>chpt)
  n_reinf_right_before <- sum(ssdt$event=='feeder_on_right'&ssdt$session_time_sec<chpt)
  n_reinf_right_after <- sum(ssdt$event=='feeder_on_right'&ssdt$session_time_sec>chpt)
  # reinf_ratio_b <- n_reinf_left_before/(n_reinf_left_before+n_reinf_right_before)
  # reinf_ratio_a <- n_reinf_left_after/(n_reinf_left_after+n_reinf_right_after)
  reinf_ratio_b <- n_reinf_left_before/n_reinf_right_before
  reinf_ratio_a <- n_reinf_left_after/n_reinf_right_after
  matching <- list(resp_ratio_b=resp_ratio_b,
                   resp_ratio_a=resp_ratio_a,
                   reinf_ratio_b=reinf_ratio_b,
                   reinf_ratio_a=reinf_ratio_a)
  return(matching)
}

# matching_ratios('p510','s45')



cum_records <- function(envr,
                        starting_sessions=T){
  
  sch <- as.numeric(strsplit(envr,split='_')[[1]])
  
  if(starting_sessions){
    trgt_rows <- which(startsWith(row.names(cc_R_left),envr))
    plot_legend <- 'sessions that started in'
    x_adj <- 1.2
  }
  if(!starting_sessions){
    trgt_rows <- which(endsWith(row.names(cc_R_left),envr))
    plot_legend <- 'sessions that ended in'
    x_adj <- -.2
  }
  # try(dev.off())
  # x11(width=4,height=4)
  resp_zoom <- 1000
  # par(mai=rep(.5,4))
  plot(NULL,xlim=c(-resp_zoom,resp_zoom),ylim=c(-resp_zoom,resp_zoom),
       axes=F,ann=F)
  for(tr in trgt_rows){
    lines(cc_R_right[tr,],cc_R_left[tr,])
  }
  points(0,0,pch=21,bg='#ffffff88',cex=3.5)
  # abline(0,sch[2]/sch[1],col='#dd6600',lwd=2)
  slope <- sch[2]/sch[1]
  if(starting_sessions){
    if(slope<1){
      x_crds <- c(-resp_zoom,0)
      y_crds <- c(-resp_zoom*slope,0)}
    if(slope>=1){
      y_crds <- c(-resp_zoom,0)
      x_crds <- c(-resp_zoom/slope,0)
    }
  }
  if(!starting_sessions){
    if(slope<1){
      x_crds <- c(resp_zoom,0)
      y_crds <- c(resp_zoom*slope,0)}
    if(slope>=1){
      y_crds <- c(resp_zoom,0)
      x_crds <- c(resp_zoom/slope,0)
    }
  }
  lines(x_crds,y_crds,col='#dd6600bb',lwd=3)
  
  text(0,0,'matching prediction',srt=atan(slope)*57.29578,adj=c(x_adj,-.5),col='#dd6600bb',cex=.7,family='CM Roman')
  mtext(paste('Left VI',sch[1],'- Right VI',sch[2]),font=2,cex=1.25,family='CM Roman',line=.5)
  mtext(plot_legend,line=1.5,cex=1,family='CM Roman',col='#333333')
  par(mgp=c(0,.5,0),tck=-.02)
  axis(1,at=c(-resp_zoom,0,resp_zoom),cex=.8,col='#444444',col.axis='#444444',family='CM Roman')
  axis(2,at=c(-resp_zoom,0,resp_zoom),las=1,cex=.8,col='#444444',col.axis='#444444',family='CM Roman')
}






plot_bayes <- function(which_plot,which_time=NULL,which_node=NULL){
  if(!is.null(which_time)){
    if(which_time=='after'){
      xx <- bayes$dat$xx_a
      yy <- bayes$dat$yy_a
      bb <- bayes$dat$bb_a
      b0 <- bayes$nds$b0_a
      b1 <- bayes$nds$b1_a
      title <- 'after the change'
      legend <- ''
    }
    if(which_time=='before'){
      xx <- bayes$dat$xx_b
      yy <- bayes$dat$yy_b
      bb <- bayes$dat$bb_b
      b0 <- bayes$nds$b0_b
      b1 <- bayes$nds$b1_b
      title <- 'before the change'
      legend <- 'each color represents one bird'
    }
  }
  
  if(which_plot=='scatter'){
    plot(NULL,xlim=c(-6,6),ylim=c(-6,6),axes=F,ann=F)
    indx <- sample(1:dim(b0)[1],50)
    for(b in 1:length(unique(bb))){
      for(ii in indx){
        abline(b0[ii,b],b1[ii,b],col=paste(brd_cl[b],'33',sep=''))
      }
    }
    segments(x0=c(0,-6),x1=c(0,0),y0=c(-6,0),y1=c(0,0),lty='dashed',col='#333333')
    abline(0,1,col='#dd6600bb',lwd=2)
    text(-5,-5,'matching',adj=c(0,-.3),cex=1.5,col='#dd6600bb',srt=45,family='CM Roman')
    points(xx,yy,pch=21,bg=paste(brd_cl[bb],'88',sep=''),cex=1.5)
    text(-5,5,legend,cex=1.25,adj=0,font=3,family='CM Roman')
    par(mgp=c(0,.6,0),tck=-.01)
    axis(2,col='#444444',col.axis='#444444',family='CM Roman')
    axis(1,col='#444444',col.axis='#444444',family='CM Roman')
    mtext(expression(paste('log(','rewards'['left'] / 'rewards'['right'],')')),1,line=2.75,family='CM Roman',cex=2)
    mtext(expression(paste('log(','responses'['left'] / 'responses'['right'],')')),2,line=1.5,family='CM Roman',cex=2)
    mtext(title,3,cex=2.5,family='CM Roman')
    # mtext('response ratio',1,family='CM Roman')
  }
  
  if(which_plot=='marginal'){
    if(which_node=='beta1'){
      distro <- b1
      max_d <- 20
      main_lab <- 'slopes'
      x_label <- expression(beta[1])}
    else if(which_node=='beta0'){
      distro <- b0
      max_d <- 8
      main_lab <- 'intercepts'
      x_label <- expression(beta[0])}
    else if(which_node=='delta_beta0'){
      distro <- bayes$nds$delta_beta_0
      max_d <- 6
      main_lab <- 'difference between intercepts'
      x_label <- expression(beta[0]['after']-beta[0]['before'])}
    else if(which_node=='delta_beta1'){
      distro <- bayes$nds$delta_beta_1
      max_d <- 14
      main_lab <- 'difference between slopes'
      x_label <- expression(beta[1]['after']-beta[1]['before'])}
    plot(NULL,xlim=c(-1.2,1.2),ylim=c(0,max_d),axes=F,ann=F)
    if(which_node=='delta_beta0'|which_node=='delta_beta1'){
      lines(c(0,0),c(0,max_d*.8),lty='dashed')
    }
    clip(-1,1,0,100)
    for(b in 1:length(unique(bb))){
      hist(distro[,b],breaks=seq(-1.5,1.5,.01),plot=F)->ht
      lines(ht$mids,ht$density,lwd=2,col=brd_cl[b])
    }
    # axis(2,cex=.8,col='#444444',col.axis='#444444',family='CM Roman')
    par(mgp=c(0,.25,0))
    axis(1,cex=.8,col='#444444',col.axis='#444444',family='CM Roman')
    mtext(x_label,1,line=1.5,family='CM Roman',cex=1.5)
    mtext(main_lab,3,line=-1,cex=1.8,family='CM Roman')
    if(which_plot=='marginal'&which_time=='before'&which_node=='beta0'){
      mtext('posterior density',2,line=0,cex=1,family='CM Roman')
    }
  }
  
  
  
}




