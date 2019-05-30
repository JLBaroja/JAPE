rm(list=ls())

library('extrafont')
library('png')
setwd('/home/bistecito/Documents/Research/JAPE/Reports')
# pigeon <- readPNG('nice_pigeon.png')
# matching <- readPNG('matching.png')
sessum <- read.csv('/home/bistecito/Documents/Research/JAPE/ConcurrentData/session_summary.csv')
load('~/Documents/Research/JAPE/ConcurrentData/ReinforcerAnalysis/full_reinforcers.RData')
load('~/Documents/Research/JAPE/ConcurrentData/ResponseAnalysis/full_responses.RData')
load('~/Documents/Research/JAPE/Reports/matching_jags.RData')

brd_cl <- c("#069668","#194F46","#6C9999","#1C4585","#3295E9","#6778F5")


try(dev.off())
# x11(width=8,height=5)
pdf_file <- '/home/bistecito/Documents/Research/JAPE/Reports/visit_sarah.pdf'
pdf(file=pdf_file,width=7,height=5,family='CM Roman')
par(mar=c(1,1,2,1),cex=2,fg='#000000',mgp=c(3,0,0))

plot(0,type='n',axes=F,ann=F)
y_pos <- .2
text(1,.2+y_pos,'Pigeon Adaptation to Unsignaled Changes\nin the Rates of Reward\nof Concurrent VI-VI Schedules',
     font=2,cex=.8)
text(1,-.3+y_pos,'José Luis Baroja',
     font=3,cex=.7)
text(1,-.6+y_pos,'https://github.com/JLBaroja/JAPE',
     cex=.6,family='CM Typewriter')

plot_experiment <- function(last_row=F){
  plot(0,main='design',type='n',axes=F)
  y_pos <- .2
  text(.7,0.75+y_pos,'6 birds, 180 daily sessions,',adj=0)
  text(.7,0.5+y_pos,'1080 total sessions',adj=0)
  text(.75,0.25+y_pos,'844 stable sessions',adj=0)
  text(.8,-.05+y_pos,'reward rates constant throughout\nthe session',font=3,adj=0,cex=.8)
  text(.75,-.4+y_pos,'236 dynamic sessions',adj=0)
  text(.8,-.7+y_pos,'reward rates switched at some\nrandom second (unsignaled)',font=3,adj=0,cex=.8)
  if(last_row){
    text(.7,-1.1+y_pos,'149299 rewards, 3132940 responses',adj=0)
  }
}

# plot_experiment()
plot_experiment(T)

# try(dev.off())
# x11(width = 7,height = 5)

par(mar=rep(0,4))
plot(NULL,xlim=c(-15,180),ylim=c(-3,10),axes=T)
axis(1,at=c(1,seq(15,180,15)),pos=0,tck=-.025,cex.axis=.5)
mtext('session',1,line=-2,cex=2)
legend(90,8,legend=c('stable','dynamic'),horiz = T,
       yjust=.5,xjust=.5,
       pch=22,pt.bg=c('#0000ddaa','#dd0000aa'),
       pt.cex=1,box.lty = 'blank')
cnt_bb <- 0
for(bb in unique(sessum$bird)){
  cnt_bb <- cnt_bb+1
  cnt_ss <- 0
  text(-2,cnt_bb,bb,adj=1,cex=.7)
  for(ss in unique(sessum$session)){
    cnt_ss <- cnt_ss+1
    color <- '#0000ddaa'
    if(subset(sessum,bird==bb&session==ss)$dynamic_env==T){
      color <- '#dd0000aa'
    }
    polygon(x=c(cnt_ss-.4,cnt_ss+.4,cnt_ss+.4,cnt_ss-.4),
            y=rep(c(cnt_bb-.4,cnt_bb+.4),each=2),
            border=F,
            col=color)
    # points(cnt_ss,cnt_bb,pch=21,bg=color)
  }
}


plot(0,type='n',ann=F,axes=F)
text(1,0,'(some)\nresults',font=2,cex=1.5)

par(plt=c(.2857,.7143,.2,.8),new=F,mgp=c(8,.25,0),tck=-.04,cex.axis=.7,cex.main=.8)
plot(0,type='n',axes=F,ylim=c(0,100),xlim=c(0,100),ylab='')
mtext('All sessions\nbefore the change point',3,line=0,cex=1.5,font=2)
axis(1,at=c(0,50,100))
axis(2,at=c(0,50,100))
mtext('% Rewards in left key',1,line=1,cex=1.5)
mtext('% Responses in left key',2,line=1,cex=1.5)
mtch <- read.csv('~/Documents/Research/JAPE/ConcurrentData/matching_by_session.csv')
mtch <- mtch[sample(nrow(mtch)),]
points(100*mtch$n_reinf_left/(mtch$n_reinf_left+mtch$n_reinf_right),
       100*mtch$n_resp_left/(mtch$n_resp_left+mtch$n_resp_right),
       cex=.6,pch=21,bg=paste(brd_cl[as.numeric(mtch$bird)],'99',sep=''))
abline(0,1,col='#ee0000aa',lwd=3)
text(50,50,'matching prediction',srt=45,adj=c(1.15,1.5),col='#ee0000',cex=.5,font=2)

plot_bayes <- function(prior=F,post=F){
  plot(0,#main='All sessions\nbefore the change point',
       type='n',axes=F,ylim=c(-4,4),xlim=c(-4,4),ylab='')
  mtext('All sessions\nbefore the change point',3,line=0,cex=1.5,font=2)
  axis(1,at=c(-4,0,4))
  axis(2,at=c(-4,0,4))
  mtext(expression(paste('log(','rewards'['left'] / 'rewards'['right'],')')),1,line=1.1,cex=1.5)
  mtext(expression(paste('log(','responses'['left'] / 'responses'['right'],')')),2,line=.9,cex=1.5)
  if(post){
    for(b in 1:6){
      for(s in 1:100){
        abline(nds$beta0[s,b],nds$beta1[s,b],col=paste(brd_cl[b],'88',sep=''))
      }
    }
  }
  if(prior){
    for(s in 1:100){
      abline(nds$beta0_prior[s],nds$beta1_prior[s],col='#ee880077')
    }
  }
  if(!prior){
    points(log(mtch$n_reinf_left/mtch$n_reinf_right),
           log(mtch$n_resp_left/mtch$n_resp_right),
           cex=.6,pch=21,bg=paste(brd_cl[as.numeric(mtch$bird)],'99',sep=''))
  }
  abline(0,1,col='#ee0000aa',lwd=3)
  text(0,0,'matching prediction',srt=45,adj=c(1.15,1.5),col='#ee0000',cex=.5,font=2)
}

plot_bayes()
plot_bayes(prior = T)
plot_bayes(post = T)




bb <- 'p054'
ss <- 's141'
bb <- 'p138'
ss <- 's139'
plot_dynamic <- function(bb,ss,xlab=F,cp=F,main=F){
  reinf <- subset(full_reinforcers,bird==bb&session==ss)
  resp <- subset(full_responses,bird==bb&session==ss)
  s_dat <- subset(sessum,bird==bb&session==ss)
  y_left <- .15
  y_right <- -.25

  plot(NULL,xlim=c(-300,3600),ylim=c(-.5,.5),ann=F,axes=F)
  text(0,.4,paste(bb,', ',ss,':',sep=''),font=2,cex=.5,adj=0)
  segments(x0=resp$time[resp$lever=='left'],
           x1=resp$time[resp$lever=='left'],
           y0=rep(y_left-.1,sum(resp$lever=='left')),
           y1=rep(y_left+.1,sum(resp$lever=='left')))
  segments(x0=resp$time[resp$lever=='right'],
           x1=resp$time[resp$lever=='right'],
           y0=rep(y_right-.1,sum(resp$lever=='right')),
           y1=rep(y_right+.1,sum(resp$lever=='right')))
  if(main){
    mtext('Session dynamics',3,line=.5,cex=1.5,font=2)
  }
  if(cp){
    abline(v=s_dat$CPleft,col='#ee0000',lwd=2)
    text(s_dat$CPleft,0.4,'change point',font=3,cex=.5,adj=-.1,col='#ee0000')
  }
  points(reinf$time[reinf$lever=='left'],rep(y_left,sum(reinf$lever=='left')),pch=21,bg='#00ee00dd',cex=.6)
  points(reinf$time[reinf$lever=='right'],rep(y_right,sum(reinf$lever=='right')),pch=21,bg='#00ee00dd',cex=.6)
  text(-250,y_left,'left',font=3,cex=.6)
  text(-250,y_right,'right',font=3,cex=.6)
  axis(1,at=seq(0,3600,600),labels=seq(0,3600,600)/60)
  if(xlab){
    mtext('session time (min.)',1,line=.3,adj=.55)
  }
}
par(plt=c(0,1,.55,.85),new=F,mgp=c(3,-.2,0),tck=-.04,cex.axis=.5,cex.main=.8)
plot_dynamic('p530','s155',cp=T,main=T)
par(plt=c(0,1,.15,.45),new=T,mgp=c(3,-.2,0),tck=-.04,cex.axis=.5,cex.main=.8)
plot_dynamic('p004','s134',xlab=T)



bb <- 'p004'
ss <- 's134'
reinf <- subset(full_reinforcers,bird==bb&session==ss)
resp <- subset(full_responses,bird==bb&session==ss)
rec1_left <- resp$cum_resp_left
rec1_right <- resp$cum_resp_right
bb <- 'p530'
ss <- 's155'
reinf <- subset(full_reinforcers,bird==bb&session==ss)
resp <- subset(full_responses,bird==bb&session==ss)
s_dat <- subset(sessum,bird==bb&session==ss)
rec2_cp <- which(resp$time>=s_dat$CPleft)[1]
rec2_left <- resp$cum_resp_left
rec2_right <- resp$cum_resp_right

par(plt=c(.2857,.7143,.2,.8),new=F,mgp=c(3,.25,0),tck=-.03,cex.axis=.5,cex.main=.8)
plot(NULL,xlim=c(0,2500),ylim=c(0,2500),ann=F,axes=F)
axis(1,padj=-1);axis(2)
mtext('Cumulative responses on right',1,line=0.75,cex=1.2)
mtext('Cumulative responses on left',2,line=1,cex=1.2)
lines(rec1_right,rec1_left,lwd=2)
lines(rec2_right,rec2_left,lwd=2)
points(rec2_right[rec2_cp],
       rec2_left[rec2_cp],pch=4,col='#dd0000',lwd=2)
text(rec2_right[rec2_cp],
     rec2_left[rec2_cp],'change point',font=3,col='#dd0000',adj=c(-.1,1.7),cex=.5)
mtext('Session dynamics',3,line=0,cex=1.5,font=2)



record_bunch <- function(env1,env2){
  # bb <- 'p736'
  # ss <- sessum$session[sessum$bird==bb&sessum$dynamic_env&sessum$session_n<105]
  # env1 <- '90_30'
  # env2 <- '30_90'
  # ss <- sessum$session[sessum$env_1==env1&sessum$env_2==env2]
  # env1 <- '30_90'
  # env2 <- '90_30'
  sch_2 <- as.numeric(strsplit(env2,split='_')[[1]])
  slope_2 <- sch_2[2]/sch_2[1]
  sch_1 <- as.numeric(strsplit(env1,split='_')[[1]])
  slope_1 <- sch_1[2]/sch_1[1]
  resp_zoom <- 1000

  plot(NULL,xlim=c(-resp_zoom,resp_zoom),ylim=c(-resp_zoom,resp_zoom),
       axes=F,ann=F)
  axis(1,padj=-1);axis(2)
  abline(0,1,lty='dashed',col='#dd0000')
  clip(-resp_zoom,resp_zoom,-resp_zoom,resp_zoom)
  for(bb in unique(sessum$bird)){
    # locses <<- subset(sessum,bird==bb&session%in%ss&env_1==env1&env_2==env2)
    locses <- subset(sessum,bird==bb&env_1==env1&env_2==env2)#&sessum$session_n<105)
    for(i in 1:nrow(locses)){
      lcrp <- subset(full_responses,bird==bb&session==locses$session[i])
      pos_cp <- which(lcrp$time>=locses$CPleft[i])[1]
      origin_x <- lcrp$cum_resp_right[pos_cp]
      origin_y <- lcrp$cum_resp_left[pos_cp]
      lines(lcrp$cum_resp_right-origin_x,
            lcrp$cum_resp_left-origin_y,
            lwd=2,
            col=brd_cl[which(unique(sessum$bird)==bb)])
    }
  }
  clip(-resp_zoom,0,-resp_zoom,0)
  abline(0,slope_1,col='#ee0000',lwd=2)
  clip(0,resp_zoom,0,resp_zoom)
  abline(0,slope_2,col='#ee0000',lwd=2)
  clip(-resp_zoom,resp_zoom,-resp_zoom,resp_zoom)
  points(0,0,pch=21,col='#dd0000',bg='#ffdddd77',lwd=2,cex=1)
  mtext(paste(paste('IV',sch_1,sep='',collapse = '-'),
              ' to ',
              paste('IV',sch_2,sep='',collapse = '-'),sep=''),3,font=2,cex=1.2)
  mtext('Cumulative responses on right\n(relative to the change point)',1,line=2,cex=.8)
  mtext('Cumulative responses on left\n(relative to the change point)',2,line=1,cex=.8)
}

par(plt=c(.2857,.7143,.2,.8),new=F,mgp=c(3,.25,0),tck=-.03,cex.axis=.5,cex.main=.8)
record_bunch('90_30','30_90')
record_bunch('30_90','90_30')
record_bunch('135_27','35_63')
record_bunch('63_35','24_360')
record_bunch('225_25','24_360')
record_bunch('24_360','360_24')

# PULSES
source('~/Documents/Research/JAPE/MoreCode/reinforcer_pulses.R')

layout(matrix(1:12,ncol=6))
par(oma=c(3,3,3,3),mar=rep(1,4))
for(bb in unique(sessum$bird)){
  for(lvr in c('left','right')){
    plot_pulses(bb,lvr)
    if(bb==unique(sessum$bird)[6]){
      mtext(paste('streaks in',lvr),4,font=2,line=1)
    }
  }
}
mtext('Reinforcers in a row from the same lever',1,outer=T,line=1)
mtext('Effect of reward continuations',3,outer=T,line=0,font=2)
mtext('responses left minus responses right',2,outer=T,line=1)

dev.off()
embed_fonts(pdf_file)







# 
# session <- NA
# bird <- NA
# n_resp_left <- NA
# n_resp_right <- NA
# n_reinf_left <- NA
# n_reinf_right <- NA
# cntr <- 0
# for(bb in unique(sessum$bird)){
#   for(ss in unique(sessum$session)){
#     cntr <- cntr+1
#     ss_dat <- subset(sessum,bird==bb&session==ss)
#     reinf <- subset(full_reinforcers,bird==bb&session==ss&time<ss_dat$CPleft)
#     resp <- subset(full_responses,bird==bb&session==ss&time<ss_dat$CPleft)
#     session[cntr] <- ss
#     bird[cntr] <- bb
#     n_resp_left[cntr] <- sum(resp$lever=='left')
#     n_resp_right[cntr] <- sum(resp$lever=='right')
#     n_reinf_left[cntr] <- sum(reinf$lever=='left')
#     n_reinf_right[cntr] <- sum(reinf$lever=='right')
#     print(c(bb,ss))
#   }
# }
# matching_session <- data.frame(session,
#                                bird,
#                                n_resp_left,
#                                n_resp_right,
#                                n_reinf_left,
#                                n_reinf_right)
# write.csv(matching_session,'~/Documents/Research/JAPE/ConcurrentData/matching_by_session.csv',row.names = F)




# 
# plot(matching_session$n_reinf_left/(matching_session$n_reinf_left+matching_session$n_reinf_right),
#      matching_session$n_resp_left/(matching_session$n_resp_left+matching_session$n_resp_right))
# abline(0,1)
# plot(log(matching_session$n_reinf_left/matching_session$n_reinf_right),
#      log(matching_session$n_resp_left/matching_session$n_resp_right),
#      ylim=c(-4,4),xlim=c(-4,4));abline(0,1)
# 
# 
# 
