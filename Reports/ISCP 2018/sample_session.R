
sample_session <- function(){
  
  session <- read.csv('~/Documents/Research/JAPE/ConcurrentData/CSV files/p530s174.csv')
  start_time <- 1200
  end_time <- 3300
  session <- session[session$session_time_sec>start_time&session$session_time_sec<end_time,]
  resp_left <- session$session_time_sec[session$event=='response_left_key']
  resp_right <- session$session_time_sec[session$event=='response_right_key']
  reinf_left <- session$session_time_sec[session$event=='feeder_on_left']
  reinf_right <- session$session_time_sec[session$event=='feeder_on_right']
  axis_base <- seq(1200,3300,300)
  
  # try(dev.off())
  # x11(width=28,height=7)
  # par(mar=rep(2,4))
  plot(NULL,xlim=c(start_time-200,end_time+10),ylim=c(-1.1,.9),axes=F,ann=F)
  axis(1,at=axis_base,labels=axis_base/60,col='#666666',pos=-.5,cex.axis=1.5,family='CM Roman')
  text(end_time-20,-.65,'session time (min.)',adj=1,cex=1.5,col='#666666',family='CM Roman')
  lines(x=rep(unique(session$first_cp_left),2),y=c(-1,.8),col='#cc000088',lwd=4)
  text(unique(session$first_cp_left),-.9,
       'at some point the reward rates of both keys changed such that the richest became the poorest and vice versa',
       adj=c(-.01,0),font=3,cex=2,col='#cc0000',family='CM Roman')
  text(unique(session$first_cp_left),-1,
       'the change was not signaled: the physical appearence of all stimuli in the operant chamber remained constant during the whole session',
       adj=c(-.01,0),font=3,cex=1.5,col='#cc0000',family='CM Roman')
  
  # segments(x0=reinf_left,x1=reinf_left,y0=rep(.1,length(reinf_left)),y1=rep(.4,length(reinf_left)),pch=21,col='red')
  # segments(x0=reinf_right,x1=reinf_right,y0=rep(-.1,length(reinf_right)),y1=rep(-.4,length(reinf_right)),pch=21,col='red')
  segments(x0=resp_left,x1=resp_left,y0=rep(.15,length(resp_left)),y1=rep(.35,length(resp_left)))
  segments(x0=resp_right,x1=resp_right,y0=rep(-.15,length(resp_right)),y1=rep(-.35,length(resp_right)))
  points(reinf_left,rep(.25,length(reinf_left)),pch=21,bg='#00ee77',cex=2.5)
  points(reinf_right,rep(-.25,length(reinf_right)),pch=21,bg='#00ee77',cex=2.5)
  
  text(1180,.25,'Left Key',family='CM Roman',cex=3,adj=1,font=3)
  text(1180,-.25,'Right Key',family='CM Roman',cex=3,adj=1,font=3)
  
  text(1500,.6,
       'during the first half of the session, one key payed more frequently than the other;',
       font=3,cex=1.5,col='#0000cc',family='CM Roman')
  
  text(1500,.5,
       'birds invested more time and responses exploiting the richest key',
       font=3,cex=1.5,col='#0000cc',family='CM Roman')

  text(2590,.6,
       'after the change, birds continued to invest in the previously rich alternative for some time,',
       font=3,cex=1.5,col='#0000cc',family='CM Roman')
  
  text(2590,.5,
       'but eventually behavior allocation switched towards the new best key',
       font=3,cex=1.5,col='#0000cc',family='CM Roman')
  
  base_x <- resp_right[resp_right<1500&resp_right>1450]-140
  segments(x0=base_x,x1=base_x,y0=rep(-.8,length(base_x)),
           y1=rep(-.7,length(base_x)))
  text(1420,-.75,'responses',family='CM Roman',adj=c(0.5,0.5),cex=1.75)
  
  base_x <- reinf_right[reinf_right<2900&reinf_right>2820]-1240
  points(base_x,rep(-.75,length(base_x)),pch=21,bg='#00ee77',cex=2)
  text(1700,-.75,'rewards',family='CM Roman',adj=c(0.5,0.5),cex=1.75)
  

}

# sample_session()


