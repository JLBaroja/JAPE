rm(list=ls())
setwd("~/Documents/Luis/JAPE/ConcurrentPrograms")
dy_sch <- read.csv('dynamic_schedule.csv')
dynamic_schedule <- NULL
# ss <- unique(dy_sch$session)
for(ss in unique(dy_sch$session)){
  ssdat <- subset(dy_sch,session==ss)
  dynamic_schedule <- rbind(dynamic_schedule,ssdat[c(4,2,5,6,1,3),])
}
write.csv(dynamic_schedule[,c('bird','session','date','med_program')],
          file='dynamic_schedule.csv',row.names = F)
