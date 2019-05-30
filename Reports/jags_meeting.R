

mtch <- read.csv('~/Documents/Research/JAPE/ConcurrentData/matching_by_session.csv')
mtch <- mtch[(mtch$n_reinf_left!=0&
                mtch$n_reinf_right!=0&
                mtch$n_resp_left!=0&
                mtch$n_resp_right!=0),]

log_resp <- log(mtch$n_resp_left/mtch$n_resp_right)
log_reinf <- log(mtch$n_reinf_left/mtch$n_reinf_right)
brds <- as.numeric(mtch$bird)
n_obs <- length(brds)
n_brds <- length(unique(brds))
observed <- list('log_resp',
                 'log_reinf',
                 'brds',
                 'n_obs',
                 'n_brds')
unobserved <- c('beta1','beta0','err',
                'beta1_prior','beta0_prior')
write('
model{
  beta1_prior ~ dnorm(1,1)T(0,)
  beta0_prior ~ dnorm(0,1)
  # err_prior ~ dgamma(0.001,0.001)
  
  for(b in 1:n_brds){
    beta1[b] ~ dnorm(1,1)T(0,)
    beta0[b] ~ dnorm(0,1)
    err[b] ~ dgamma(0.001,0.001)
  }

  for(i in 1:n_obs){
    log_resp[i]~dnorm(log_reinf[i]*beta1[brds[i]]+beta0[brds[i]],err[brds[i]])
  }
  
}
','model.bug')
library('R2jags')
inference <-jags( 
  data=observed, 
  # inits=iniciales, 
  parameters.to.save=unobserved, 
  model.file='model.bug',
  n.chains=3, 
  n.iter=5000, 
  n.burnin=1000, 
  n.thin=4, 
  DIC=T) 
nds <- inference$BUGSoutput$sims.list
save(nds,file='matching_jags.RData')
