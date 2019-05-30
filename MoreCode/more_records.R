rm(list=ls())

brd_cl <- c("#069668","#194F46","#6C9999","#1C4585","#3295E9","#6778F5")

sessum <- read.csv('~/Documents/Research/JAPE/ConcurrentData/session_summary.csv',stringsAsFactors = F)
# ses_num <- NA
# for(i in 1:nrow(sessum)){
#   ses_num[i] <- as.numeric(strsplit(sessum$session[i],split='s')[[1]][2])
# }
# sessum$session_n <- ses_num

load('~/Documents/Research/JAPE/ConcurrentData/ReinforcerAnalysis/full_reinforcers.RData')
load('~/Documents/Research/JAPE/ConcurrentData/ResponseAnalysis/full_responses.RData')

record_bunch <- function(env1,env2){
  # bb <- 'p736'
  # ss <- sessum$session[sessum$bird==bb&sessum$dynamic_env&sessum$session_n<105]
  env1 <- '90_30'
  env2 <- '30_90'
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
  axis(1);axis(2)
  abline(0,1,lty='dashed',col='#dd0000')
  clip(-resp_zoom,resp_zoom,-resp_zoom,resp_zoom)
  for(bb in unique(sessum$bird)){
    # locses <<- subset(sessum,bird==bb&session%in%ss&env_1==env1&env_2==env2)
    locses <<- subset(sessum,bird==bb&env_1==env1&env_2==env2)#&sessum$session_n<105)
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
  points(0,0,pch=21,col='#dd0000',bg='#ffdddd',lwd=2,cex=1.5)
  mtext(paste(paste('IV',sch_1,sep='',collapse = '-'),
              ' to ',
              paste('IV',sch_2,sep='',collapse = '-'),sep=''),3)
}

