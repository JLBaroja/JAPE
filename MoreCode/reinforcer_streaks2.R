rm(list=ls())
load('~/Documents/Research/JAPE/ConcurrentData/ReinforcerAnalysis/full_reinforcers.RData')

birds <- sort(unique(full_reinforcers$bird))
col_birds <- c("#32964d", "#c20da6", "#96e97c", "#7f42a9", "#7fd6c5", "#154975")
col_local_vi <- heat.colors(n=length(unique(full_reinforcers$local_VI)),alpha = NULL)
unique(full_reinforcers$local_VI)
local_vi <- sort(unique(full_reinforcers$local_VI),decreasing = T)
layout(matrix(1:6,ncol=3))
par(bg='#000000',fg='#aaaaaa',col.axis='#999999',
    mar=rep(2,4),oma=c(3,4,2,0))
for(l_vi in 1:length(local_vi)){
  for(bb in birds){
    bd <- subset(full_reinforcers,bird==bb)
    plot(NULL,
         xlim=c(0,20),
         ylim=c(1,5000),
         log='y',axes=F)
    axis(1,las=1)
    axis(2,las=1)
    mtext(bb,3)
    # for(lv in unique(bd$lever)){
    #   ld <- subset(bd,lever==lv)
    # for(lvi in local_vi){
    for(lvi in local_vi[setdiff(1:length(local_vi),l_vi)]){
      ld <- subset(bd,local_VI==lvi)
      hist(ld$streak_length,
           breaks=seq(0.5,100.5,1),
           plot=F)->ht
      lines(ht$mids,ht$counts,
            # col=col_birds[which(birds==bb)],lwd=2)
            col=paste(col_local_vi[which(local_vi==lvi)],'55',sep=''),
            type='o',lwd=1)
      # text(15,seq(500))
      print(head(ht$counts))
    }
    abline(h=5,lty='dashed',col='#999999')
    lvi <- local_vi[l_vi]
    ld <- subset(bd,local_VI==lvi)
    hist(ld$streak_length,
         breaks=seq(0.5,100.5,1),
         plot=F)->ht
    lines(ht$mids,ht$counts,
          # col=col_birds[which(birds==bb)],lwd=2)
          col=paste(col_local_vi[which(local_vi==lvi)],'ee',sep=''),
          type='o',lwd=1.2,cex=1.2)
    # mtext(paste('VI',local_vi[l_vi]),3,outer=T,
    #       col=col_local_vi[which(local_vi==lvi)],cex=1.2)
  }
    mtext(paste('VI',local_vi[l_vi]),3,outer=T,
          col=col_local_vi[which(local_vi==lvi)],cex=1.2)
  mtext('Number of reinforcers in a row',1,outer=T,cex=1,col='#aaaaaa',line=1)
  mtext('Number of streaks',2,outer=T,cex=1,col='#aaaaaa',line=1.5)
}



