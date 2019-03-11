rm(list=ls())

# Building data lists
japede_events <- vector(mode='list')
japede_cum_events <- vector(mode='list')
setwd('~/Documents/Research/JAPE/ConcurrentData/RData files/')
birds <- c('p004','p054','p138','p510','p530','p736')
c_bb <- 0
for(bb in birds){
  c_bb <- c_bb+1
  load(paste(bb,'cum_resp_reinf_list.RData',sep='_'))
  load(paste(bb,'events_list.RData',sep='_'))
  japede_events[[c_bb]] <- events_list
  japede_cum_events[[c_bb]] <- cum_list
  names(japede_events)[c_bb] <- bb
  names(japede_cum_events)[c_bb] <- bb
}

rm(list=ls()[!ls()%in%c('japede_events','japede_cum_events','birds','all_cums')])

source('~/Documents/Programming/R/hpmp/ehpmp.R')
source('~/Documents/Research/JAPE/MoreCode/cumulative_records.R')

# cum_p004 <- get_cum_counts('p004',1:136)
# cum_p054 <- get_cum_counts('p054',1:136)
# cum_p138 <- get_cum_counts('p138',1:136)
# cum_p510 <- get_cum_counts('p510',1:136)
# cum_p530 <- get_cum_counts('p530',1:136)
# cum_p736 <- get_cum_counts('p736',1:136)
# all_cums <- list(p004=cum_p004,
#                  p054=cum_p054,
#                  p138=cum_p138,
#                  p510=cum_p510,
#                  p530=cum_p530,
#                  p736=cum_p736)


start_slide <- function(file_name){
  setwd('~/Documents/Research/JAPE/Reports/')
  start_poster(file_name,11,8,rep(.25,4))
}
end_slide <- function(file_name){
  setwd('~/Documents/Research/JAPE/Reports/')
  end_poster(global_guides = F,
             local_guides = F)
  embed_fonts(file_name)
}

insert_text <- function(string,pos=c(0,0),cex,family='CM Roman',font=1){
  plot(NULL,xlim=c(-1,1),ylim=c(-1,1),ann=F,axes=F)
  text(pos[1],pos[2],string,cex=cex,family=family,font=font)
}

slide_title <- function(string){
  new_plot('center_top',c(0,3.75),10,1)
  insert_text(string,cex=3.25)
}

cum_record <- function(bb,variable,annotations=T,line_col='#000000'){
  # bd <- get_cum_counts(bb,1:136)
  bd <- all_cums[[which(names(all_cums)==bb)]]
  if(variable=='responses'){
    var_left <- (bd$cum_resp_left)
    var_right <- (bd$cum_resp_right)}
  if(variable=='reinforcers'){
    var_left <- (bd$cum_reinf_left)
    var_right <- (bd$cum_reinf_right)}
  max_left <- max(var_left)
  max_right <- max(var_right)
  ax_lim <- max(c(max_left,max_right))*1.1
  par(fg='#888888',col.axis='#000000',mgp=c(3,.5,0))
  plot(NULL,
       xlim=c(0-ax_lim*.05,ax_lim*1.05),
       ylim=c(0-ax_lim*.05,ax_lim*1.05),axes=F,ann=F)
  if(annotations){
    clip(0,ax_lim,0,ax_lim)
    abline(0,1,lty='dashed')
    clip(-100,ax_lim*1.2,-100,ax_lim*1.2)
    segments(x0=c(0,0,ax_lim,0),
             x1=c(ax_lim,0,ax_lim,ax_lim),
             y0=c(0,0,0,ax_lim),
             y1=c(0,ax_lim,ax_lim,ax_lim),col='#888888',lwd=2)
    segments(x0=c(max_right,0),
             x1=c(max_right,max_right),
             y0=c(0,max_left),
             y1=c(max_left,max_left),col=line_col,lty='solid')
    axis(1,at=c(0,max_right),col='#888888',cex=1.25,lwd=0,lwd.ticks = 1,pos=0)
    axis(2,at=c(0,max_left),col='#888888',cex=1.25,lwd=0,lwd.ticks = 1,pos=0)
  }
  lines(var_right,var_left,lwd=3,col=line_col)
  points(max_right,max_left,pch=21,cex=1.5,lwd=2.5,bg='white',col=line_col)
}

cum_record_2 <- function(bb,variable,annotations=T,line_col='#000000',sessions=118:136){
  # bd <- get_cum_counts(bb,1:136)
  bd <- all_cums[[which(names(all_cums)==bb)]]
  bd <- subset(bd,session%in%sessions)
  if(variable=='responses'){
    var_left <- (bd$cum_resp_left)
    var_right <- (bd$cum_resp_right)}
  if(variable=='reinforcers'){
    var_left <- (bd$cum_reinf_left)
    var_right <- (bd$cum_reinf_right)}
  var_left <- var_left-min(var_left)
  var_right <- var_right-min(var_right)
  max_left <- max(var_left)
  max_right <- max(var_right)
  ax_lim <- max(c(max_left,max_right))*1.1
  par(fg='#888888',col.axis='#000000',mgp=c(3,.5,0))
  plot(NULL,
       xlim=c(0-ax_lim*.05,ax_lim*1.05),
       ylim=c(0-ax_lim*.05,ax_lim*1.05),axes=F,ann=F)
  if(annotations){
    clip(0,ax_lim,0,ax_lim)
    abline(0,1,lty='dashed')
    clip(-100,ax_lim*1.2,-100,ax_lim*1.2)
    segments(x0=c(0,0,ax_lim,0),
             x1=c(ax_lim,0,ax_lim,ax_lim),
             y0=c(0,0,0,ax_lim),
             y1=c(0,ax_lim,ax_lim,ax_lim),col='#888888',lwd=2)
    segments(x0=c(max_right,0),
             x1=c(max_right,max_right),
             y0=c(0,max_left),
             y1=c(max_left,max_left),col=line_col,lty='solid')
    axis(1,at=c(0,max_right),col='#888888',cex=1.25,lwd=0,lwd.ticks = 1,pos=0)
    axis(2,at=c(0,max_left),col='#888888',cex=1.25,lwd=0,lwd.ticks = 1,pos=0)
  }
  lines(var_right,var_left,lwd=3,col=line_col)
  points(max_right,max_left,pch=21,cex=1.5,lwd=2.5,bg='white',col=line_col)
}

# cum_record_2('p530','responses')

cum_records <- function(both=F){
  y_centers <- c(rep(0+1.25,3),rep(0-2,3))
  x_centers <- c(rep(c(-3.25,0,3.25),2))
  for(brd in 1:length(birds)){
    new_plot('center_center',c(x_centers[brd],y_centers[brd]),3,3)
    cum_record(birds[brd],'reinforcers',annotations=!both,line_col='#555555')
    if(both){
      new_plot('center_center',c(x_centers[brd],y_centers[brd]),3,3)
      cum_record(birds[brd],'responses')
    }
  }
}


cum_records_2 <- function(both=F){
  y_centers <- c(rep(0+1.25,3),rep(0-2,3))
  x_centers <- c(rep(c(-3.25,0,3.25),2))
  for(brd in 1:length(birds)){
    new_plot('center_center',c(x_centers[brd],y_centers[brd]),3,3)
    cum_record_2(birds[brd],'reinforcers',annotations=!both,line_col='#555555')
    if(both){
      new_plot('center_center',c(x_centers[brd],y_centers[brd]),3,3)
      cum_record_2(birds[brd],'responses')
    }
  }
}


cp_reference <- function(target_session,scale=c(-100,100),axez=F){
  par(mgp=c(3,.5,0))
  plot(NULL,xlim=scale,ylim=scale,axes=F,ann=F)
  if(T){
    clip(scale[1],scale[2],scale[1],scale[2])
    abline(0,1,lty='dashed')
    segments(x0=c(scale[1],scale[1],scale[2],scale[1]),
             x1=c(scale[2],scale[1],scale[2],scale[2]),
             y0=c(scale[1],scale[1],scale[1],scale[2]),
             y1=c(scale[1],scale[2],scale[2],scale[2]),col='#888888',lwd=2)
  }
  if(axez){
    axis(1,at=c(.9*scale,0),labels=c(scale,0),lwd=0,lwd.ticks = 1)
    axis(2,at=c(.9*scale,0),labels=c(scale,0),lwd=0,lwd.ticks = 1)
  }
  # bb <- birds[2]
  for(bb in birds){
    cum_b <- all_cums[[which(names(all_cums)==bb)]]
    if(bb==birds[1]){
      cum_s <- subset(cum_b,session==target_session)
      vi_l_b <- unique(cum_s$vi_left[which(cum_s$change_point)-2])
      vi_l_a <- unique(cum_s$vi_left[which(cum_s$change_point)+2])
      vi_r_b <- unique(cum_s$vi_right[which(cum_s$change_point)-2])
      vi_r_a <- unique(cum_s$vi_right[which(cum_s$change_point)+2])
      clip(scale[1],0,scale[1],0)
      abline(0,vi_r_b/vi_l_b,lwd=5,col='#ee000077')
      clip(0,scale[2],0,scale[2])
      abline(0,vi_r_a/vi_l_a,lwd=5,col='#ee000077')
      clip(scale[1],scale[2],scale[1],scale[2])}
    tr_cum_left <- cum_b$cum_resp_left-cum_b$cum_resp_left[cum_b$change_point&cum_b$session==target_session]
    tr_cum_right <- cum_b$cum_resp_right-cum_b$cum_resp_right[cum_b$change_point&cum_b$session==target_session]
    # plot(cum_s$cum_resp_right,cum_s$cum_resp_left)
    points(tr_cum_right[cum_b$session_end],
           tr_cum_left[cum_b$session_end],col='#0000ee88',pch=4,cex=2,lwd=1)
    points(tr_cum_right,tr_cum_left,type='l',lwd=2.5)
    
  }
    # points(tr_cum_right[cum_b$change_point&cum_b$session==target_session],
    #        tr_cum_left[cum_b$change_point&cum_b$session==target_session],
    #        bg='#ffffff',pch=21,cex=2.5,lwd=2)
    points(0,0,bg='#ffffff',pch=21,cex=2.5,lwd=2)
}

cp_references <- function(scale=c(-100,100)){
  file <- paste('cp_ref_',scale[2],'.pdf',sep='')
  start_slide(file)
  slide_title('Cambios esporádicos')
  y_centers <- c(rep(0+1.25,3),rep(0-2,3))
  x_centers <- c(rep(c(-3.25,0,3.25),2))
  c_ss <- 0
  for(t_s in c(16,26,36,53,75,101)){
    c_ss <- c_ss+1
    new_plot('center_center',c(x_centers[c_ss],y_centers[c_ss]),3,3)
    cp_reference(t_s,scale=scale,axez=c_ss==4)
  }
  end_slide(file)
}


# cp_references(c(-100,100))
# cp_references(c(-1000,1000))
# cp_references(c(-5000,5000))
# cp_references(c(-10000,10000))


start_slide('cum_rec_dyn.pdf')
slide_title('Cambios frecuentes')
cum_records_2(F)
end_slide('cum_rec_dyn.pdf')
start_slide('cum_rec_dyn_2.pdf')
slide_title('Cambios frecuentes')
cum_records_2(T)
end_slide('cum_rec_dyn_2.pdf')

# start_slide('cum_rec.pdf')
# slide_title('Reforzadores totales')
# cum_records(F)
# end_slide('cum_rec.pdf')
# 
# start_slide('cum_rec_2.pdf')
# slide_title('Respuestas totales')
# cum_records(T)
# end_slide('cum_rec_2.pdf')



# # Title 
# start_slide('jap.pdf')
# new_plot(which_point = 'center_top',
#          coordinates = c(0,3.75),
#          width = 10,height = 1)
# insert_text('VI Seminario Internacional sobre Comportamiento y Aplicaciones',cex=2)
# 
# new_plot(which_point = 'center_bottom',
#          coordinates = c(0,0.5),
#          width = 8.5,height = 2)
# insert_text('Velocidad de Ajuste\n en Ambientes Dinámicos',cex=4.5)
# 
# new_plot(which_point = 'center_top',
#          coordinates = c(0,0.25),
#          width = 8.5,height = 1.25)
# insert_text('José Luis Baroja\n Arturo Bouzas',cex=2.75)
# new_plot(which_point = 'center_top',
#          coordinates = c(0,-1),
#          width = 8.5,height = 1.25)
# insert_text('Laboratorio 25\n Facultad de Psicología, UNAM',cex=2.25,font=3)
# 
# new_plot(which_point = 'center_bottom',
#          coordinates = c(0,-3.75),
#          width = 10,height = 1)
# insert_text('Universidad Autónoma de Tlaxcala\n Noviembre 15, 2017',cex=2)
# end_slide('jap.pdf')



# # Design
# start_slide('design_1.pdf')
# slide_title('Diseño experimental')
# 
# new_plot('center_top',c(0,2.25),8.5,.75)
# insert_text('6 palomas',font=2,cex=2.75)
# new_plot('center_top',c(0,1.5),8.5,.5)
# insert_text('Toda la comida en caja experimental',cex=2.25,font=3)
# 
# new_plot('center_top',c(0,0.5),8.5,.75)
# insert_text('Concurrente VI - VI',font=2,cex=2.75)
# new_plot('center_top',c(0,-.25),8.5,1)
# insert_text('Distribución geométrica sobre segundos\n (sin Flesher & Hoffman\'s)',cex=2.25,font=3)
# 
# new_plot('center_top',c(0,-1.75),8.5,.75)
# insert_text('1 hr para comer',font=2,cex=2.75)
# new_plot('center_top',c(0,-2.5),8.5,.5)
# insert_text('136 sesiones diarias, y contando...',cex=2.25,font=3)
# 
# end_slide('design_1.pdf')

# Design
start_slide('stay_tuned.pdf')
slide_title('Contacto')

new_plot('center_top',c(0,2.25),8.5,.75)
insert_text('Repositorio del Proyecto',font=2,cex=2.75)
new_plot('center_top',c(0,1.5),8.5,.5)
insert_text('Datos, código y actualizaciones',cex=2.25,font=3)
new_plot('center_top',c(0,1),8.5,.5)
insert_text('https://github.com/JLBaroja/JAPE',cex=2.25,
            family='CM Typewriter')

new_plot('center_top',c(0,0),8.5,.75)
insert_text('Lab 25',font=2,cex=2.75)
new_plot('center_top',c(0,-.75),8.5,.5)
insert_text('http://bouzaslab25.com/',cex=2.25,
            family='CM Typewriter')

new_plot('center_top',c(0,-1.75),8.5,.75)
insert_text('jlbm',font=2,cex=2.75)
new_plot('center_top',c(0,-2.5),8.5,.5)
insert_text('j.luis.baroja@gmail.com',cex=2.25,
            family='CM Typewriter')

end_slide('stay_tuned.pdf')



# # Design
# start_slide('design_2.pdf')
# slide_title('Diseño experimental')
# 
# new_plot('center_top',c(0,2.25),8.5,.75)
# insert_text('2 tipos de sesión',font=2,cex=2.75)
# new_plot('center_top',c(0,1.5),8.5,.75)
# insert_text('Estables: los valores de los VI no cambian',cex=2.25,font=3)
# new_plot('center_top',c(0,.85),8.5,1)
# insert_text('Dinámicas: en algún segundo aleatorio\n la tecla rica se convierte en pobre',cex=2.25,font=3)
# 
# 
# new_plot('center_top',c(0,0-.75),8.5,.75)
# insert_text('2 fases',font=2,cex=2.75)
# new_plot('center_top',c(0,-1.5),8.5,1)
# insert_text('Cambios esporádicos: sesiones dinámicas\n separadas por muchas sesiones estables',cex=2.25,font=3)
# new_plot('center_top',c(0,-2.5),8.5,1)
# insert_text('Cambios frecuentes: sesiones dinámicas ocurren\n con alta frecuencia',cex=2.25,font=3)
# 
# end_slide('design_2.pdf')

 
# start_slide('design_3.pdf')
# slide_title('Diseño experimental')
# vi_left <- c(30,90,30,90,30,90,45)
# vi_right <- c(90,30,90,30,90,30,45)
# stable_lengths <- c(15,9,9,16,21,25,13)
# change_sessions <- c(16,26,36,53,75,101,117)
# rich <- c(24,25,27,30,35)
# poor <- c(360,225,135,90,63)
# new_plot('center_top',c(0,1.5),10.5,5)
# tex_cex <- 2
# text_family <- 'CM Roman'
# plot(NULL,xlim=c(-1,3),ylim=c(-1,1),axes=F,ann=F)
# text(rep(-.5,8),seq(.9,-.9,length.out = 8),c('VI izq',as.character(vi_left)),cex=tex_cex,family=text_family)
# text(rep(0,8),seq(.9,-.9,length.out = 8),c('VI der',as.character(vi_right)),cex=tex_cex,family=text_family)
# text(rep(.5,8),seq(.9,-.9,length.out = 8),c('# Ses.',as.character(stable_lengths)),cex=tex_cex,family=text_family)
# text(2,.9,'En cada sesión de cada paloma:',cex=tex_cex,family=text_family)
# text(2,.65,'Decide si es sesión dinámica (p=0.5)',cex=tex_cex,family=text_family)
# text(2,.45,'Alterna entre estos programas:',cex=tex_cex,family=text_family)
# text(rep(1.5,2),seq(.2,-.9,length.out = 6),c('Tecla rica',as.character(rich)),cex=tex_cex,family=text_family)
# text(rep(2.5,2),seq(.2,-.9,length.out = 6),c('Tecla pobre',as.character(poor)),cex=tex_cex,family=text_family)
# new_plot('center_top',c(-2.5,2.5),5,1)
# insert_text('Cambios esporádicos',cex=2.25,font=2)
# new_plot('center_top',c(2.5,2.5),5,1)
# insert_text('Cambios frecuentes',cex=2.25,font=2)
# end_slide('design_3.pdf')



# Design
start_slide('conclusiones.pdf')
slide_title('Conclusiones (hasta ahora)')

# new_plot('center_top',c(0,2.25),8.5,.75)
# insert_text('6 palomas',font=2,cex=2.75)
new_plot('center_top',c(0,2.5),8.5,1.5)
insert_text('El ajuste "completo" toma varias sesiones\n (cientos o miles de RESPUESTAS)...',cex=2.25,font=3)

# new_plot('center_top',c(0,0.5),8.5,.75)
# insert_text('Concurrente VI - VI',font=2,cex=2.75)
new_plot('center_top',c(0,.75),8.5,2)
insert_text('...aunque parece que las palomas\n comienzan a ajustar poco tiempo después\n del cambio programado.',cex=2.25,font=3)

# new_plot('center_top',c(0,-1.75),8.5,.75)
# insert_text('1 hr para comer',font=2,cex=2.75)
# new_plot('center_top',c(0,-1.75),8.5,.5)
# insert_text('¿Qué tan rápido lo hacen las palomas?',cex=2.25,font=4)
new_plot('center_top',c(0,-1.5),8.5,1.5)
insert_text('Parece que ni siquiera los cambios más\n drásticos tienen el mismo efecto\n entre palomas.',cex=2.25,font=3)
end_slide('conclusiones.pdf')


# # Design
# start_slide('direcciones.pdf')
# slide_title('Preguntas por resolver')
# 
# # new_plot('center_top',c(0,2.25),8.5,.75)
# # insert_text('6 palomas',font=2,cex=2.75)
# new_plot('center_top',c(0,2.75),8.5,1.5)
# insert_text('¿Qué detectan los animales?\n ¿Cambios en tasas locales o globales?',cex=2.25,font=3)
# 
# # new_plot('center_top',c(0,0.5),8.5,.75)
# # insert_text('Concurrente VI - VI',font=2,cex=2.75)
# new_plot('center_top',c(0,1.5),8.5,2)
# insert_text('¿La distribución de tiempos cambia\n de manera similar a la de\n respuestas?',cex=2.25,font=3)
# 
# # new_plot('center_top',c(0,-1.75),8.5,.75)
# # insert_text('1 hr para comer',font=2,cex=2.75)
# # new_plot('center_top',c(0,-1.75),8.5,.5)
# # insert_text('¿Qué tan rápido lo hacen las palomas?',cex=2.25,font=4)
# new_plot('center_top',c(0,-.3),8.5,1.5)
# insert_text('¿Qué ocurre bajo otro tipo\n de contigencias (VR-VR, VR-VI)?',cex=2.25,font=3)
# 
# new_plot('center_top',c(0,-1.75),8.5,1.5)
# insert_text('¿Qué es diferente entre palomas?\n ¿Qué es igual?',cex=2.25,font=3)
# 
# end_slide('direcciones.pdf')

# 
# # Design
# start_slide('background.pdf')
# slide_title('Antecedentes')
# 
# # new_plot('center_top',c(0,2.25),8.5,.75)
# # insert_text('6 palomas',font=2,cex=2.75)
# new_plot('center_top',c(0,2.5),8.5,1.5)
# insert_text('Muchos ambientes aleatorios cambian sin aviso;\n detectar el cambio y ajustar el comportamiento\n puede ser crucial para sobrevivir.',cex=2.25,font=3)
# 
# # new_plot('center_top',c(0,0.5),8.5,.75)
# # insert_text('Concurrente VI - VI',font=2,cex=2.75)
# new_plot('center_top',c(0,.75),8.5,2)
# insert_text('Las ratas y ratones parecen ajustarse\n rápidamente a cambios no señalados\n en programas concurrentes VI-VI\n (Gallistel et al., 2001).',cex=2.25,font=3)
# 
# # new_plot('center_top',c(0,-1.75),8.5,.75)
# # insert_text('1 hr para comer',font=2,cex=2.75)
# new_plot('center_top',c(0,-1.75),8.5,.5)
# insert_text('¿Qué tan rápido lo hacen las palomas?',cex=2.25,font=4)
# new_plot('center_top',c(0,-2.25),8.5,.5)
# insert_text('¿Qué tan general es el efecto?',cex=2.25,font=3)
# 
# end_slide('background.pdf')


# Design
start_slide('thanks.pdf')
slide_title('Agradecimientos')

uppers <- seq(2.75,-2,length.out = 5)
names <- c('Elena Villalobos','Uriel González','Karina Ruiz',
           'Azul Meléndez','Enrique Estrada')
for(nn in 1:length(names)){
new_plot('center_top',c(0,uppers[nn]),8.5,1.5)
insert_text(names[nn],cex=3,font=3)}

end_slide('thanks.pdf')
