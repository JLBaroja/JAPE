# rm(list=ls())
library('extrafont')
library('png')
library('grImport')
source('~/Documents/Programming/R/BistecitoFunctions/ehpmp/ehpmp.R')
setwd('~/Documents/Research/JAPE/Reports/ISCP 2018/')
source('sample_session.R')
source('cum_records.R')

unam <- readPicture('Escudo-UNAM.eps.xml')
lab <- readPicture('Lab25_logo_2015.eps.xml')

poster_title <- function(){
  plot(0,type='n',xlim=c(-1,1),ylim=c(-1,1),axes=F,ann=F)
  text(0,.8,'XIX Biennal Meeting of the International Society for Comparative Psychology. October 29-31, 2018. University of California, Los Angeles.',family='CM Roman',cex=3)
  text(0,.2,'Pigeon Adaptation to Unsignaled Changes in the Rates of Reward\nof VI-VI Concurrent Schedules',
       family='CM Roman',cex=6)
  text(0,-.5,'José Luis Baroja, Elena Villalobos, & Arturo Bouzas',family='CM Roman',cex=4.75)
  text(0,-.85,'Laboratorio 25, Psychology School, UNAM',family='CM Roman',cex=3.8,font=3)
}

zone_polygon <- function(){
  plot(0,type='n',xlim=c(-1,1),ylim=c(-1,1),ann=F,axes=F)
  polygon(x=rep(c(-1,1),each=2),y=c(1,-1,-1,1),border=F,col='#eeeeee')
}

zone_label <- function(left_center,height,label){
  new_plot(which_point = 'left_center',coordinates=left_center,width=1.5,height=height)
  plot(0,type='n',xlim=c(-1,1),ylim=c(-1,1),ann=F,axes=F)
  text(0,0,label,font=2,srt=90,cex=5,col='#444444')
}

file <- 'poster_iscp_2018.pdf'
start_poster(file,48,40,rep(2/4,4))

new_plot(which_point = 'left_top',coordinates = c(-23.5,19.5),width=5,height=5,
         label = 'unam')
grid.picture(unam,x=plt_x_coord(-21),y=plt_y_coord(17),exp=4)

new_plot(which_point = 'right_top',coordinates = c(23.5,19.5),width=5,height=5,
         label = 'lab\nlogo')
grid.picture(lab,x=plt_x_coord(21),y=plt_y_coord(17),exp=5)

new_plot(which_point = 'center_top',coordinates = c(0,19.5),width=18.5*2,height=5,
         label = 'poster title')
poster_title()

new_plot(which_point = 'center_top',coordinates = c(0,14),width=23.5*2,height=2.5,
         label = 'findings')
zone_polygon()
text(0,0.65,'Many environments are dynamic, in the sense that the underlying reward probabilities may change suddenly and abruptly; besides, many of such changes are not necessarily signaled:',family='CM Roman',cex=2.5,font=2)
text(0,.2167,'the price of some stock, the outbreak of a new disease, or the relationships within our closest social groups, may dramatically switch from night to day, with little or no clues of the upcoming change.',family='CM Roman',cex=2.5,font=2)
text(0,-.2167,'Arguably, the ability to detect and quickly adapt to such changes may be crucial for survival. In this work, we use an animal model to study the speed of adjustment to new environments following',family='CM Roman',cex=2.5,font=2)
text(0,-.65,'abrupt, unsignaled changes in the rates of reward. Research supported by grant PAPIIT IG120818',family='CM Roman',cex=2.5,font=2)


new_plot(which_point = 'left_bottom',coordinates = c(-23.5,3),width=16,height=8,
         label = 'task_area')
zone_polygon()
text(-.8,.1+.8,'Six pigeons worked for food in standard operant chambers.',adj=0,cex=3.15,family='CM Roman')
text(-.8,.1+.6,'Two keys were available during the session, each delivering',adj=0,cex=3.15,family='CM Roman')
text(-.75,.1+.45,'rewards accordingly to a Variable Interval (VI) schedule:',adj=0,cex=3.15,family='CM Roman')
text(-.75,.1+.3,'each second, the computer flipped a biased coin to decide',adj=0,cex=3.15,family='CM Roman')
text(-.75,.1+.15,'whether to set up a reward on each key, waiting for the',adj=0,cex=3.15,family='CM Roman')
text(-.75,.1+0,'the next response that key to be delivered.',adj=0,cex=3.15,family='CM Roman')
text(-.8,.1+-.2,'During the first half of the session, one key payed more',adj=0,cex=3.15,family='CM Roman')
text(-.75,.1+-.35,'frequently than the other, but at some random second',adj=0,cex=3.15,family='CM Roman')
text(-.75,.1+-.50,'this relationship was reversed with no explicit signal',adj=0,cex=3.15,family='CM Roman')
text(-.75,.1+-.65,'to the bird.',adj=0,cex=3.15,family='CM Roman')
text(-.8,.1+-.85,'We attepted to measure the speed of adjustment to the',adj=0,cex=3.15,family='CM Roman')
text(-.75,.1+-1,'new environment.',adj=0,cex=3.15,family='CM Roman')

zone_label(c(-23.5,7),8,'task')



### Conditions
new_plot(which_point = 'left_top',coordinates = c(-7,11),width=30.5,height=8,
         label = '')
zone_polygon()
zone_label(c(-7,7),8,'sample session')
new_plot(which_point = 'left_top',coordinates = c(-5,10.5),width=28,height=7,
         label = 'sample session')
sample_session()


new_plot(which_point = 'left_top',coordinates = c(-23.5,2.5),
         width=47,height=10,
         label = 'cumulative records')
zone_polygon()
zone_label(c(-23.5,-2.5),10,'cumulative records')


# Cumulative records
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
x_strt <- -20.5
y_strt <- 1.5
x_coords <- rep(seq(x_strt,length.out = 11,by=4),2)
y_coords <- c(rep(y_strt,length(x_coords)/2),rep(y_strt-4.5,length(x_coords)/2))
for(pp in 1:length(x_coords)){
  pp_inx <- pp
  if(pp>11){
    pp_inx <- pp-11
  }
  new_plot(which_point = 'left_top',
           coordinates = c(x_coords[pp],y_coords[pp]),
           width=3.25,height=3.25,label=paste('cum_rec',pp,sep=''))
  cum_records(environmentz[pp_inx],starting_sessions = pp<=11)
}
  
new_plot(which_point = 'left_top',
           coordinates = c(-22,2),
           width=1,height=8.5,label='ylabel')
plot(NULL,xlim=c(-1,1),ylim=c(-1,1),axes=F,ann=F)
text(1,0,'cumulative responses on left',family='CM Roman',srt=90,cex=2.5,font=3,adj=c(0.5,-1.5))
text(1,0,'(relative to the change point)',family='CM Roman',srt=90,cex=1.5,font=3,adj=c(0.5,-.5))

new_plot(which_point = 'left_top',
           coordinates = c(-21,-6.5),
           width=8.5,height=1,label='ylabel')
plot(NULL,xlim=c(-1,1),ylim=c(-1,1),axes=F,ann=F)
text(0,1,'cumulative responses on right',family='CM Roman',cex=2.5,font=3,adj=c(0.5,1.5))
text(0,1,'(relative to the change point)',family='CM Roman',cex=1.5,font=3,adj=c(0.5,4.5))



# Plot Bayes
new_plot(which_point = 'left_top',coordinates = c(-23.5,-8),
         width=23.25,height=11.5,
         label = 'matching stuff')
zone_polygon()
zone_label(c(-23.4,-13.75),11.5,'matching')

load(file='~/Documents/Research/JAPE/Reports/ISCP 2018/jabm.RData')
brd_cl <- c(rgb(6,150,104,alpha=NULL,maxColorValue = 255),
            rgb(25,79,70,alpha=NULL,maxColorValue = 255),
            rgb(108,153,153,alpha=NULL,maxColorValue = 255),
            rgb(28,69,133,alpha=NULL,maxColorValue = 255),
            rgb(50,149,233,alpha=NULL,maxColorValue = 255),
            rgb(103,120,245,alpha=NULL,maxColorValue = 255))

new_plot(which_point = 'left_top',coordinates = c(-21,-9),
         width=6,height=6,
         label = 'scatter before')
plot_bayes('scatter','before')
new_plot(which_point = 'left_top',coordinates = c(-14,-9),
         width=6,height=6,
         label = 'scatter after')
plot_bayes('scatter','after')

new_plot(which_point = 'left_top',coordinates = c(-20.75,-16.25),
         width=2.5,height=2.5,
         label = 'intercepts before')
plot_bayes('marginal','before','beta0')
new_plot(which_point = 'left_top',coordinates = c(-17.75,-16.25),
         width=2.5,height=2.5,
         label = 'slopes before')
plot_bayes('marginal','before','beta1')

new_plot(which_point = 'left_top',coordinates = c(-13.75,-16.25),
         width=2.5,height=2.5,
         label = 'intercepts after')
plot_bayes('marginal','after','beta0')
new_plot(which_point = 'left_top',coordinates = c(-10.75,-16.25),
         width=2.5,height=2.5,
         label = 'slopes after')
plot_bayes('marginal','after','beta1')

new_plot(which_point = 'left_top',coordinates = c(-7,-9),
         width=6.25,height=4.25,
         label = 'delta intercepts')
plot_bayes('marginal',NULL,'delta_beta0')

new_plot(which_point = 'left_top',coordinates = c(-7,-14.25),
         width=6.25,height=4.5,
         label = 'delta slopes')
plot_bayes('marginal',NULL,'delta_beta1')



new_plot(which_point = 'left_top',coordinates = c(.25,-8),width=23.25,height=8,
         label = 'discussion')
zone_polygon()
text(-.85,.8,'During the first half of the session, previous to the change in the rates of reward,',adj=0,cex=3.5,family='CM Roman')
text(-.8,.65,' birds\' distribution of responses was relatively close to the matching relationship.',adj=0,cex=3.5,family='CM Roman')
text(-.85,.45,'However, following abrupt, unsignaled changes in the rates of reward, they showed',adj=0,cex=3.5,family='CM Roman')
text(-.8,.3,'no immediate re-distribution of behavior: the new response equilibrium was',adj=0,cex=3.5,family='CM Roman')
text(-.8,.15,'reached only after several minutes and after having obtained numerous rewards',adj=0,cex=3.5,family='CM Roman')
text(-.8,0,'in the new environmnent.',adj=0,cex=3.5,family='CM Roman')
text(-.85,-.2,'This preliminar result contrasts with findings reported using rats and mice, that',adj=0,cex=3.5,family='CM Roman')
text(-.8,-.35,'suggest those species detect and re-adjust to similar changes \"as rapidly',adj=0,cex=3.5,family='CM Roman')
text(-.8,-.50,'as they could in principle do so\" (Gallistel et al., 2001).',adj=0,cex=3.5,family='CM Roman')
text(-.85,-.7,'Future, more precise analyses are needed in order to better understand the source',adj=0,cex=3.5,family='CM Roman')
text(-.8,-.85,'of this discrepancy and its implications regarding the study of change detection.',adj=0,cex=3.5,family='CM Roman')
zone_label(c(.25,-12),8,'discussion')

new_plot(which_point = 'left_top',coordinates = c(.25,-16.5),width=23.25,height=1.25,
         label = 'discussion')
zone_polygon()
text(-.85,0,'Gallistel, C. R., Mark, T. A., King, A. P. & Latham, P. E. (2001). The rat approximates and ideal detector of changes in rates of reward:\n     Implications for the Law of Effect. Journal of Experimental Psychology, 27, 354:372.',adj=0,cex=2,family='CM Roman')

new_plot(which_point = 'left_top',coordinates = c(.25,-18.25),width=23.25,height=1.25,
         label = 'discussion')
zone_polygon()

text(-.975,-.10,'Data, code, and updates:',adj=0,cex=3,family='CM Roman')
text(-.5,0-.1,'https://github.com/JLBaroja/JAPE',adj=0,cex=3,family='CM Typewriter')
text(.28,0,'Contact:',adj=0,cex=3,family='CM Roman')
text(.45,0-.15,'j.luis.baroja@gmail.com',adj=0,cex=3,family='CM Typewriter')

end_poster(global_guides = F,
           local_guides = F)
embed_fonts(file)
