

load(file='~/Documents/Research/JAPE/Reports/ISCP 2018/ch_plttng_stff.RData')
xxx <- log2(ch_plotting_stuff$ww_b_chng)
yyy <- log2(ch_plotting_stuff$rr_b_chng)
birds <- unique(ch_plotting_stuff$birds)
bbb <- NA
for(rr in 1:length(ch_plotting_stuff$birds)){
  bbb[rr] <- which(birds==ch_plotting_stuff$birds[rr])
}

bb_b <- bbb[!(is.na(xxx)|is.na(yyy)|xxx=='Inf'|yyy=='Inf'|xxx=='-Inf'|yyy=='-Inf')]
yy_b <- yyy[!(is.na(xxx)|is.na(yyy)|xxx=='Inf'|yyy=='Inf'|xxx=='-Inf'|yyy=='-Inf')]
xx_b <- xxx[!(is.na(xxx)|is.na(yyy)|xxx=='Inf'|yyy=='Inf'|xxx=='-Inf'|yyy=='-Inf')]

xxx <- log2(ch_plotting_stuff$ww_a_chng)
yyy <- log2(ch_plotting_stuff$rr_a_chng)
birds <- unique(ch_plotting_stuff$birds)
bbb <- NA
for(rr in 1:length(ch_plotting_stuff$birds)){
  bbb[rr] <- which(birds==ch_plotting_stuff$birds[rr])
}

bb_a <- bbb[!(is.na(xxx)|is.na(yyy)|xxx=='Inf'|yyy=='Inf'|xxx=='-Inf'|yyy=='-Inf')]
yy_a <- yyy[!(is.na(xxx)|is.na(yyy)|xxx=='Inf'|yyy=='Inf'|xxx=='-Inf'|yyy=='-Inf')]
xx_a <- xxx[!(is.na(xxx)|is.na(yyy)|xxx=='Inf'|yyy=='Inf'|xxx=='-Inf'|yyy=='-Inf')]

library('R2jags')
# n_bb_b <- length(unique(bb_b))
n_bb <- length(unique(bb_a))
n_dat_b <- length(xx_b)
n_dat_a <- length(xx_a)
data_jags <- list('xx_a',
                  'xx_b',
                  'yy_a',
                  'yy_b',
                  'bb_a',
                  'bb_b',
                  'n_bb',
                  # 'n_bb_b',
                  'n_dat_a',
                  'n_dat_b')
write('
model{
  
  for(b in 1:n_bb){
    mu_beta_0[b] ~ dnorm(0,1)
    delta_beta_0[b] ~ dnorm(0,1)
    delta_beta_0_pr[b] ~ dnorm(0,1)
    b0_a[b] <- mu_beta_0[b]+delta_beta_0[b]/2
    b0_b[b] <- mu_beta_0[b]-delta_beta_0[b]/2
    mu_beta_1[b] ~ dnorm(0,1)
    delta_beta_1[b] ~ dnorm(0,1)
    delta_beta_1_pr[b] ~ dnorm(0,1)
    b1_a[b] <- mu_beta_1[b]+delta_beta_1[b]/2
    b1_b[b] <- mu_beta_1[b]-delta_beta_1[b]/2
    err[b]~dnorm(0,10)T(0,)
  }
  
  for(da in 1:n_dat_a){
    yy_a[da]~dnorm(xx_a[da]*b1_a[bb_a[da]]+b0_a[bb_a[da]],1/err[bb_a[da]]^2)
  }
  for(db in 1:n_dat_b){
    yy_b[db]~dnorm(xx_b[db]*b1_b[bb_b[db]]+b0_b[bb_b[db]],1/err[bb_b[db]]^2)
  }
  
}
','model.bug')

nodes <- c(
  'mu_beta_0',
  'delta_beta_0',
  'delta_beta_0_pr',
  'b0_a',
  'b0_b',
  'mu_beta_1',
  'delta_beta_1',
  'delta_beta_1_pr',
  'b1_a',
  'b1_b',
  'err')
posteriores <-jags( 
  data=data_jags, 
  # inits=iniciales, 
  parameters.to.save=nodes, 
  model.file='model.bug',
  n.chains=3, 
  n.iter=5000, 
  n.burnin=1000, 
  n.thin=4, 
  DIC=T) 
nds <- posteriores$BUGSoutput$sims.list
dat <- list(yy_a=yy_a,
            yy_b=yy_b,
            xx_a=xx_a,
            xx_b=xx_b,
            bb_a=bb_a,
            bb_b=bb_b)
bayes <- list(nds=nds,dat=dat)
save(bayes,file='~/Documents/Research/JAPE/Reports/ISCP 2018/jabm.RData')


load(file='~/Documents/Research/JAPE/Reports/ISCP 2018/jabm.RData')
brd_cl <- c(rgb(6,150,104,alpha=NULL,maxColorValue = 255),
            rgb(25,79,70,alpha=NULL,maxColorValue = 255),
            rgb(108,153,153,alpha=NULL,maxColorValue = 255),
            rgb(28,69,133,alpha=NULL,maxColorValue = 255),
            rgb(50,149,233,alpha=NULL,maxColorValue = 255),
            rgb(103,120,245,alpha=NULL,maxColorValue = 255))


plot_bayes <- function(which_plot,which_time=NULL,which_node=NULL){
  if(!is.null(which_time)){
    xx <- bayes$dat$xx_b
    yy <- bayes$dat$yy_b
    bb <- bayes$dat$bb_b
    b0 <- bayes$nds$b0_b
    b1 <- bayes$nds$b1_b
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
    points(xx,yy,pch=21,bg=paste(brd_cl[bb],'88',sep=''),cex=1.5)
    par(mgp=c(0,.5,0),tck=-.02)
    axis(2,cex=.8,col='#444444',col.axis='#444444',family='CM Roman')
    axis(1,cex=.8,col='#444444',col.axis='#444444',family='CM Roman')
    mtext(expression(paste('log(','rewards'['left']/'rewards'['right'],')')),1,line=2,family='CM Roman')
    mtext(expression(paste('log(','responses'['left']/'responses'['right'],')')),2,line=1.5,family='CM Roman')
    # mtext('response ratio',1,family='CM Roman')
  }
  
  if(which_plot=='marginal'){
    if(which_node=='beta1'){
      distro <- b1
      max_d <- 20
      x_label <- expression(beta[1])}
    else if(which_node=='beta0'){
      distro <- b0
      max_d <- 10
      x_label <- expression(beta[0])}
    plot(NULL,xlim=c(-1.2,1.2),ylim=c(0,max_d),axes=F,ann=F)
    clip(-1,1,0,100)
    for(b in 1:length(unique(bb))){
      hist(distro[,b],breaks=seq(-1.5,1.5,.01),plot=F)->ht
      lines(ht$mids,ht$density,lwd=2,col=brd_cl[b])
    }
    # axis(2,cex=.8,col='#444444',col.axis='#444444',family='CM Roman')
    axis(1,cex=.8,col='#444444',col.axis='#444444',family='CM Roman')
    mtext(x_label,1,line=2,family='CM Roman')
  }
  
}

