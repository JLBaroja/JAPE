rm(list=ls())
load('~/Documents/Luis/JAPE/ConcurrentData/ResponseAnalysis/full_responses.RData')
sessum <- read.csv('~/Documents/Luis/JAPE/ConcurrentData/session_summary.csv')
# bb <- 'p530'
# ss <- 's130'

infer_change_points <- function(bb,ss){
  
  locres <- subset(full_responses,bird==bb&session==ss)
  locreinf <- subset(locres,reinforced==T)
  # layout(matrix(1:6,ncol=3))
  # plot(locres$cum_resp_right,locres$cum_resp_left)
  # plot(locres$cum_reinf_right,locres$cum_reinf_left)
  # plot(locres$cum_resp_right,locres$cum_reinf_right)
  # plot(locres$cum_resp_left,locres$cum_reinf_left)
  # plot(locres$cum_resp_right,locres$cum_reinf_left)
  # plot(locres$cum_resp_left,locres$cum_reinf_right)
  
  
  resp_left <- as.numeric(locres$lever=='left')
  reinf_left <- as.numeric(locreinf$lever=='left')
  time_resp <- locres$time
  time_reinf <- locreinf$time
  n_resp <- length(resp_left)
  n_reinf <- length(reinf_left)
  
  
  write('

model{
  pi_resp <- rep(1/n_resp,n_resp)
  pi_reinf <- rep(1/n_reinf,n_reinf)
  cp_resp ~ dcat(pi_resp)
  cp_reinf ~ dcat(pi_reinf)
  gamma_resp[1] ~ dbeta(1,1)
  gamma_resp[2] ~ dbeta(1,1)
  gamma_reinf[1] ~ dbeta(1,1)
  gamma_reinf[2] ~ dbeta(1,1)
  for(rs in 1:n_resp){
    z_resp[rs] <- step(rs-cp_resp)+1 # CP scale is # of response
    resp_left[rs] ~ dbern(gamma_resp[z_resp[rs]])
  }
  for(rf in 1:n_reinf){
    z_reinf[rf] <- step(rf-cp_reinf)+1 # CP scale is # of response
    reinf_left[rf] ~ dbern(gamma_reinf[z_reinf[rf]])
  }
  
}
','change_points.bug')
  
  obs <- list('resp_left','reinf_left',
              'n_resp','n_reinf')
  unobs <- c('gamma_resp','gamma_reinf',
             'cp_resp','cp_reinf')
  
  library('R2jags')
  time_start <- proc.time()
  inference <- jags.parallel(
    data = obs,
    parameters.to.save = unobs,
    model.file = 'change_points.bug',
    n.chains=4,
    n.iter=50,
    n.burnin=1
  )
  total_time <- proc.time()-time_start
  nds <- inference$BUGSoutput$sims.list
  
  time_cp_resp <- time_resp[nds$cp_resp]
  time_cp_reinf <- time_reinf[nds$cp_reinf]
  convergence <- (inference$BUGSoutput$summary)[c('cp_reinf','cp_resp'),c('Rhat','n.eff')]
  results <- list(total_time=total_time,
                  time_cp_reinf=time_cp_reinf,
                  time_cp_resp=time_cp_resp,
                  convergence=convergence)
  return(results)
}


# Running for all dynamic sessions:
dyn_sess <- subset(sessum,dynamic_env==T)
for(row in 1:nrow(dyn_sess)){
  brd <- dyn_sess$bird[row]
  sssn <- dyn_sess$session[row]
  c_pts <- infer_change_points(brd,sssn)
  file_name <- paste('ChPts_',brd,sssn,'.RData',sep='')
  save(c_pts,file = paste('/home/lab25/Documents/Luis/JAPE/ConcurrentData/CP_inference/',
                          file_name,
                          sep=''))
  print(nrow(dyn_sess)-row)
}

# 
# 
# layout(matrix(1:4,ncol=2))
# plot(cumsum(reinf_left))
# hist(nds$cp_reinf,breaks=100,xlim=c(1,n_reinf))
# plot(cumsum(resp_left))
# hist(nds$cp_resp,breaks=100,xlim=c(1,n_resp))
# 
# hist(time_reinf[nds$cp_reinf],breaks=100,xlim=c(0,3600))
# hist(time_resp[nds$cp_resp],breaks=100,xlim=c(0,3600))


