rm(list=ls())
sesssum <- read.csv('~/Documents/Research/JAPE/ConcurrentData/session_summary.csv')
visits <- read.csv('~/Documents/Research/JAPE/ConcurrentData/VisitAnalysis/full_visits.csv')

histogram <- function(distro,
                      brks,
                      xlmz,
                      color='#0000dd',
                      alpha='22',
                      main='Title',
                      xlb='XLab',
                      ylb='YLab',
                      add=F){
  hist(distro,breaks=brks,plot=F)->ht
  
  if(!add){
    plot(NULL,xlim=xlmz,ylim=c(0,max(ht$counts)*1.1),
         axes=F,ann=F)
    axis(1,at=seq(xlmz[1],xlmz[2],1),cex.axis=1.2)
    # axis(2,las=2,cex.axis=.5)
    mtext(main,3)
    mtext(xlb,1,line=2)
    mtext(ylb,2,line=2)
  }
  polygon(x=c(ht$mids[1],ht$mids,ht$mids[2]),
          y=c(0,ht$counts,0),
          col=paste(color,alpha,sep=''),
          border=F)
  lines(ht$mids,ht$counts,lwd=1,col=color,type='o',pch=16)
  print(c(max(ht$counts),bb))
  # return(ht)
}



pdf(file='~/Documents/Research/JAPE/Reports/ceic.pdf',width=11,height=8)

# try(dev.off())
layout(matrix(1:6,ncol=3,byrow = T))
par(mgp=c(3,1,0),tck=-.02)
histogram(visits$n_responses,
          main='# Respuestas',
          xlb = 'Respuestas por visita',
          ylb = '# Visitas',
          xlmz=c(1,10),brks=seq(.5,2360.5,1))
histogram(visits$n_reinforcers,
          main='# Reforzadores',
          xlb = 'Reforzadores por visita',
          ylb = '# Visitas',
          xlmz=c(0,5),brks=seq(-.5,100.5,1))
histogram(visits$duration,
          main='Duración',
          xlb = 'Segundos por visita',
          ylb = '# Visitas',
          xlmz=c(0,5),brks=seq(-.5,2675.5,1))

for(bb in unique(sesssum$bird)[c(4,(1:6)[-4])]){
  histogram(visits$n_responses[visits$bird==bb],
            main='# Respuestas',
            xlb = 'Respuestas por visita',
            ylb = '# Visitas',
            xlmz=c(1,10),brks=seq(.5,2360.5,1),
            add=bb!='p510')
}
for(bb in unique(sesssum$bird)[c(4,(1:6)[-4])]){
  histogram(visits$n_reinforcers[visits$bird==bb],
            main='# Reforzadores',
            xlb = 'Reforzadores por visita',
            ylb = '# Visitas',
            xlmz=c(0,5),brks=seq(-.5,100.5,1),
            add=bb!='p510')
}
for(bb in unique(sesssum$bird)[c(4,(1:6)[-4])]){
  histogram(visits$duration[visits$bird==bb],
            main='Duración',
            xlb = 'Segundos por visita',
            ylb = '# Visitas',
            xlmz=c(0,5),brks=seq(-.5,2675.5,1),
            add=bb!='p510')
}



# try(dev.off())
layout(matrix(1:12,ncol=4,byrow = T))
par(mgp=c(3,1,0),tck=-.03,mar=rep(2,4),oma=c(3,2,3,0))
# Respuestas IV local
cntr <- 0
for(ll in sort(unique(visits$local_VI))){
  cntr <- cntr+1
  histogram(visits$n_responses[visits$local_VI==ll],
            main=paste('IV ',ll,' (local)',sep=''),
            xlb = NULL,
            ylb = NULL,
            xlmz=c(1,10),brks=seq(.5,2360.5,1),
            add=F)
}
mtext('# Respuestas',3,outer=T,col='#444444')
mtext('Respuestas por visita',1,outer=T,col='#444444')
mtext('# Visitas',2,outer=T,col='#444444')
# Respuestas IV alter
cntr <- 0
for(aa in sort(unique(visits$alter_VI))){
  cntr <- cntr+1
  histogram(visits$n_responses[visits$alter_VI==aa],
            main=paste('IV ',aa,' (alter)',sep=''),
            xlb = NULL,
            ylb = NULL,
            xlmz=c(1,10),brks=seq(.5,2360.5,1),
            add=F)
}
mtext('# Respuestas',3,outer=T,col='#444444')
mtext('Respuestas por visita',1,outer=T,col='#444444')
mtext('# Visitas',2,outer=T,col='#444444')



# try(dev.off())
layout(matrix(1:12,ncol=4,byrow = T))
par(mgp=c(3,0.5,0),tck=-.03,mar=rep(2,4),oma=c(3,2,3,0))
# Respuestas IV local
cntr <- 0
for(ll in sort(unique(visits$local_VI))){
  cntr <- cntr+1
  histogram(visits$duration[visits$local_VI==ll],
            main=paste('IV ',ll,' (local)',sep=''),
            xlb = NULL,
            ylb = NULL,
            xlmz=c(0,5),brks=seq(-.5,2675.5,1),
            add=F)
}
mtext('Duración',3,outer=T,col='#444444')
mtext('Segundos por visita',1,outer=T,col='#444444')
mtext('# Visitas',2,outer=T,col='#444444')
# Respuestas IV alter
cntr <- 0
for(aa in sort(unique(visits$alter_VI))){
  cntr <- cntr+1
  histogram(visits$duration[visits$alter_VI==aa],
            main=paste('IV ',aa,' (alter)',sep=''),
            xlb = NULL,
            ylb = NULL,
            xlmz=c(0,5),brks=seq(-.5,2675.5,1),
            add=F)
}
mtext('Duración',3,outer=T,col='#444444')
mtext('Segundos por visita',1,outer=T,col='#444444')
mtext('# Visitas',2,outer=T,col='#444444')

# dev.off()




dynamic <- subset(sesssum,dynamic_env==T)
dynamic <- dynamic[order(dynamic$VIleft1),]
# try(dev.off())
layout(matrix(1:12,ncol=4,byrow = T))
par(mgp=c(3,0.5,0),tck=-.03,mar=rep(2,4),oma=c(3,2,3,0))

# sessions_2_plot <- dynamic[dynamic$env_1==unique(dynamic$env_1)[8],]
for(denv in unique(dynamic$env_1)){
  sessions_2_plot <- dynamic[dynamic$env_1==denv,]
  plot(NULL,xlim=c(-1200,1200),ylim=c(0,15),ann=F,axes=F)
  axis(1,at=seq(-1200,1200,600),labels=seq(-1200,1200,600)/60)
  axis(2,at=seq(0,15,5))
  for(rr in 1:nrow(sessions_2_plot)){
    cp <- sessions_2_plot$CPleft[rr]
    sub_visits <- subset(visits,
                         lever=='left'&
                           session==sessions_2_plot$session[rr]&
                           bird==sessions_2_plot$bird[rr]&
                           time_start>=(cp-1200)&
                           time_start<=(cp+1200))
    times <- sub_visits$time_start-cp
    responses <- sub_visits$n_responses
    brks <- seq(-1200,1200,50)
    means <- NA
    for(br in 1:(length(brks)-1)){
      means[br] <- mean(responses[times>=brks[br]&times<brks[(br+1)]])
    }
    lines(brks[1:(length(brks)-1)],means,col='#00000088')
    # lines(sub_visits$time_start-cp,
    #       sub_visits$n_responses,col='#00000044')
  }
  text(-1200,14,paste('IVizq =',unique(sessions_2_plot$VIleft1)),
       cex=1.75,col='#dd0000',font=2,adj=0)
  abline(v=0,type='dashed',col='#aa0000')
}
mtext('Efectos del cambio de programa',3,outer=T,col='#666666')
mtext('Minutos relativos al cambio de programa',1,outer=T,col='#444444')
mtext('Respuestas por visita en IZQUIERDA',2,outer=T,col='#444444')


dev.off()
