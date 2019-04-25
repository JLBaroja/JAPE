rm(list=ls())

library('extrafont')
library('png')
setwd('/home/bistecito/Documents/Research/JAPE/Reports')
pigeon <- readPNG('nice_pigeon.png')
matching <- readPNG('matching.png')
sessum <- read.csv('/home/bistecito/Documents/Research/JAPE/ConcurrentData/session_summary.csv')
load('~/Documents/Research/JAPE/ConcurrentData/ReinforcerAnalysis/full_reinforcers.RData')
load('~/Documents/Research/JAPE/ConcurrentData/ResponseAnalysis/full_responses.RData')

brd_cl <- c("#069668","#194F46","#6C9999","#1C4585","#3295E9","#6778F5")

the_task <- function(text1,text2){
  plot(0,main='The Task',type='n',axes=F)
  text(1,0.85,text1)
  text(1,-0.85,text2)
  segments(x0=rep(.8,2),x1=rep(1.2,2),
           y0=c(-.6,.6),y1=c(-.6,.6))
  segments(x0=c(.8,1.2),x1=c(.8,1.2),
           y0=rep(.6,2),y1=rep(-.6,2))
  rasterImage(pigeon,xleft = .95,xright = 1.1,ybottom = -.5,ytop = .2)
  points(c(.9,1.1),rep(0.3,2),pch=21,cex=3,bg='#ee0000',lwd=3)
}


try(dev.off())
# x11(width=8,height=5)
pdf_file <- '/home/bistecito/Documents/Research/JAPE/Reports/labs_meeting.pdf'
pdf(file=pdf_file,width=7,height=5,family='CM Roman')
par(mar=c(1,1,2,1),cex=2,fg='#000000',mgp=c(3,0,0))

plot(0,type='n',axes=F,ann=F)
text(1,.2,'Pigeon Adaptation to Unsignaled Changes\nin the Rates of Reward\nof Concurrent VI-VI Schedules',
     font=2,cex=.8)
text(1,-.3,'José Luis Baroja',
     font=3,cex=.7)
text(1,-.6,'https://github.com/JLBaroja/JAPE',
     cex=.6,family='CM Typewriter')



the_task('Pigeons are presented with two keys...',
         '...each of which pays at different rate.')
the_task('Each key flips a biased coin each second.',
         'If heads, the next response on that key\n delivers a reward.')
the_task('This procedure is known as\n \"Concurrent VI-VI Schedules\"',
         '')

plot(0,main='The Phenomenon',type='n',axes=F)
text(1,0.6,'(For some reason),\n pigeons\' distribution of behavior\nmatches\nthe distribution of reinforcements.')
text(1,-.9,'Herrnstein, 1961',cex=.8)
rasterImage(matching,xleft = .85,xright = 1.15,ybottom = -.8,ytop = 0.15)

plot_experiment <- function(last_row=F){
  plot(0,main='This Experiment',type='n',axes=F)
  text(.7,0.75+.2,'6 birds, 180 daily sessions,',adj=0)
  text(.7,0.5+.2,'1080 total sessions',adj=0)
  text(.75,0.25+.2,'844 stable sessions',adj=0)
  text(.8,-.05+.2,'reward rates constant throughout\nthe session',font=3,adj=0,cex=.8)
  text(.75,-.4+.2,'236 dynamic sessions',adj=0)
  text(.8,-.7+.2,'reward rates switched at some\nrandom second (unsignaled)',font=3,adj=0,cex=.8)
  if(last_row){
    text(.7,-1.1+.2,'150379 rewards, 3132940 responses',adj=0)
  }
}

plot_experiment()
plot_experiment(T)

plot(0,type='n',ann=F,axes=F)
text(1,.3,'Results',font=2,cex=2)

par(plt=c(.2857,.7143,.2,.8),new=F,mgp=c(3,.25,0),tck=-.04,cex.axis=.7,cex.main=.8)
plot(0,main='All sessions\nbefore the change point',
     type='n',axes=F,ylim=c(0,100),xlim=c(0,100),ylab='')
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
  plot(0,main='All sessions\nbefore the change point',
       type='n',axes=F,ylim=c(-4,4),xlim=c(-4,4),ylab='')
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
mtext('Session dynamics',3,line=.5,cex=1.5,font=2)



cum_record <- function(resp_zoom,envr_2){
  par(plt=c(.2857,.7143,.2,.8),new=F,mgp=c(3,.25,0),tck=-.03,cex.axis=.5,cex.main=.8)
  plot(NULL,xlim=c(-resp_zoom,resp_zoom),ylim=c(-resp_zoom,resp_zoom),ann=F,axes=F)
  axis(1,padj=-1);axis(2)
  mtext('Cumulative responses on right',1,line=0.75,cex=1.2)
  mtext('Cumulative responses on left',2,line=1,cex=1.2)
  
  
  # envr_1 <- '30_90'
  # envr_2 <- '27_135'
  # envr_2 <- '30_90'
  target_rows <- which(sessum$env_2==envr_2&
                         # sessum$env_1==envr_1&
                         sessum$dynamic_env)
  # sch_1 <- as.numeric(strsplit(envr_1,split='_')[[1]])
  sch_2 <- as.numeric(strsplit(envr_2,split='_')[[1]])
  mtext(paste('Sessions that ended in VI ',sch_2[1],'-','VI ',sch_2[2],sep=''),3,line=.5,cex=1.5,font=2)
  # slope_1 <- sch_1[2]/sch_1[1]
  slope_2 <- sch_2[2]/sch_2[1]
  
  for(tr in target_rows){
    bb <- sessum$bird[tr]
    ss <- sessum$session[tr]
    reinf <- subset(full_reinforcers,bird==bb&session==ss)
    resp <- subset(full_responses,bird==bb&session==ss)
    s_dat <- subset(sessum,bird==bb&session==ss)
    rec_cp <- which(resp$time>=s_dat$CPleft)[1]
    rec_left <- resp$cum_resp_left
    rec_right <- resp$cum_resp_right
    lines(rec_right-rec_right[rec_cp],rec_left-rec_left[rec_cp],
          col=brd_cl[which(unique(sessum$bird)==sessum$bird[tr])])
  }
  
  # resp_zoom <- 100
  # if(slope_1<1){
  #   x_crds_1 <- c(-resp_zoom,0)
  #   y_crds_1 <- c(-resp_zoom*slope_1,0)}
  # if(slope_1>=1){
  #   y_crds_1 <- c(-resp_zoom,0)
  #   x_crds_1 <- c(-resp_zoom/slope_1,0)
  # }
  if(slope_2<1){
    x_crds_2 <- c(resp_zoom,0)
    y_crds_2 <- c(resp_zoom*slope_2,0)}
  if(slope_2>=1){
    y_crds_2 <- c(resp_zoom,0)
    x_crds_2 <- c(resp_zoom/slope_2,0)
  }
  # lines(x_crds_1,y_crds_1,col='#dd0000bb',lwd=3)
  lines(x_crds_2,y_crds_2,col='#dd0000bb',lwd=3)
  
  points(0,0,pch=21,col='#dd0000',bg='#ffffffaa',lwd=2,cex=2)
  points(0,0,pch=4,col='#dd0000',lwd=2)
  
  
}

cum_record(1000,'30_90')
cum_record(500,'30_90')
cum_record(100,'30_90')
cum_record(1000,'135_27')
cum_record(500,'135_27')
cum_record(100,'135_27')

dev.off()
embed_fonts(pdf_file)








session <- NA
bird <- NA
n_resp_left <- NA
n_resp_right <- NA
n_reinf_left <- NA
n_reinf_right <- NA
cntr <- 0
for(bb in unique(sessum$bird)){
  for(ss in unique(sessum$session)){
    cntr <- cntr+1
    ss_dat <- subset(sessum,bird==bb&session==ss)
    reinf <- subset(full_reinforcers,bird==bb&session==ss&time<ss_dat$CPleft)
    resp <- subset(full_responses,bird==bb&session==ss&time<ss_dat$CPleft)
    session[cntr] <- ss
    bird[cntr] <- bb
    n_resp_left[cntr] <- sum(resp$lever=='left')
    n_resp_right[cntr] <- sum(resp$lever=='right')
    n_reinf_left[cntr] <- sum(reinf$lever=='left')
    n_reinf_right[cntr] <- sum(reinf$lever=='right')
    print(c(bb,ss))
  }
}
matching_session <- data.frame(session,
                               bird,
                               n_resp_left,
                               n_resp_right,
                               n_reinf_left,
                               n_reinf_right)
write.csv(matching_session,'~/Documents/Research/JAPE/ConcurrentData/matching_by_session.csv',row.names = F)

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
