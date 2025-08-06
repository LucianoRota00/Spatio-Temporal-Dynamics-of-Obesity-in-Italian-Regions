# preliminaries -------------
library(nimble) 
library(mcmcplots)
library(tidyverse) 
library(ggridges)
library(ggrepel)

setwd("C:\\Users\\lucia\\Desktop\\STBetaBayes_Obesity_Italy\\data_and_models")

min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

de_min_max_normalize = function(x_norm, x_max, x_min){
  x = x_norm * (x_max-x_min) + x_min
  return(x)
}


inv_logit <- function(eta) {
  return(1 / (1 + exp(-eta)))
}

W = readRDS('W')

load('X_til')
load('y_til')

X_til[,41] # 0 females, 1 males
colnames(X_til)
mean(y_til[1:260]) # females
mean(y_til[261:520]) # males
X_til[,c(41:43)] %>% head()
X_til[,c(41:43)] %>% tail()

sex = as.integer(factor(X_til[,41]))
space_index = as.integer(factor(X_til[,42]))
time_index = as.integer(factor(X_til[,43]))

n_reg = X_til %>% as_tibble() %>% select(space) %>% unique() %>% nrow()
n_years = X_til %>% as_tibble() %>% select(time) %>% unique() %>% nrow()
n_sex = length(unique(sex))

reg_names <- c('Piemonte', "Valle d'Aosta", 'Lombardia', 'Liguria', 'Emilia-Romagna',
               'Trentino-Alto Adige', 'Veneto', 'Friuli-Venezia Giulia',
               'Toscana', 'Umbria', 'Marche', 'Lazio',
               'Abruzzo', 'Molise', 'Campania', 'Puglia', 'Basilicata', 'Calabria',
               'Sicilia', 'Sardegna')

reg_names_zones = c('NO-Piemonte',"NO-Valle d'Aosta",'NO-Lombardia','NO-Liguria','NO-Emilia-Romagna',
                    'NE-Trentino-Alto Adige','NE-Veneto','NE-Friuli-Venezia Giulia',
                    'C-Toscana','C-Umbria','C-Marche','C-Lazio',
                    'S-Abruzzo','S-Molise','S-Campania','S-Puglia','S-Basilicata','S-Calabria',
                    'SI-Sicilia','SI-Sardegna')

N = 2*n_reg*n_years

# SSVS ---------------------------------------
c_ss<-4000 
cc = c_ss^2
intersect<-0.001
exp(0.001)-1 # % increase/decrease in the odds
tau_ss<-intersect/sqrt(2*log(c_ss)*c_ss^2/(c_ss^2-1))
tau_ss^2 
tau_ss
tau2 <- tau_ss^2
(tau2 * cc) %>% sqrt()

# DATA & CONSTANTS ---------------------------------------
# run until # constants for model ---
y_til_norm = min_max_normalize(y_til)
y_til_norm[order(y_til_norm)]

# excluding 1 and 0
y_til_norm[y_til_norm==1] = 0.999
y_til_norm[y_til_norm==0] = 0.001

y_max = y_til[which.max(y_til)]
y_min = y_til[which.min(y_til)]

# Y
#data = list(Y = y_til)
data = list(Y = y_til_norm)
y_til_norm[1:260] %>% mean() # female mean
y_til_norm[261:520] %>% mean() # male mean

# X with standardized values and PC1 and PC2 from mortality variables
load('X_all_covariates_in_model') 
colnames(X)
X[1:260,1]%>%mean()
X[261:520,1]%>%mean()
colnames(X)

p_temp = ncol(X)

adj <- apply(W, 1, function(row) which(row == 1)) %>% unlist()
sumNumNeigh = length(adj)
L = length(adj)
weights = rep(1, L)
num = W %*% rep(1,20) %>% as.vector()

#####################################################
# TRAINING MODEL   --------------------------------------------------------------
# gender as fixed and random effect, rho beta and normal distributed

# SPLIT TRAIN / TEST -----------------------------------
# Separate 2022 test indices
train_indices <- which(X_til[,43] <= 12)  # Years 1 to 12
test_indices <- which(X_til[,43] == 13)   # Year 13 = 2022

# Training set
X_train <- X[train_indices,]
y_train <- y_til[train_indices]
space_index_train <- space_index[train_indices]
time_index_train <- time_index[train_indices]
sex_train <- sex[train_indices]

# Normalize training response
y_train_norm <- min_max_normalize(y_train)
y_train_norm[y_train_norm == 1] <- 0.999
y_train_norm[y_train_norm == 0] <- 0.001

table(sex_train)
y_train_norm %>% length()

y_train_norm[1:240] %>% mean()
y_train_norm[241:480] %>% mean()

# Save min/max to denormalize predictions later
y_max <- max(y_train)
y_min <- min(y_train)

# Create design matrix for 2022
X_test <- X[test_indices,]
true_2022 <- y_til[test_indices]
sex_test <- sex[test_indices]
space_test <- space_index[test_indices]
time_test <- rep(13, length(test_indices))  # year 13 (2022)

# constants for model -----
# Constants & data for train model
# comment/uncomment based on what specification you want: 
# gender fixed/random and rho beta/normal
constants <- list(X = X_train,
                  N = length(y_train), p = ncol(X),
                  n_reg = n_reg, n_years = 12,
                  space_index = space_index_train,
                  time_index = time_index_train,
                  sex = sex_train,
                  n_sex = n_sex,
                  adj = adj, L = L, num = num,
                  a = 50, a_phi = 0.1, b_phi = 0.1,
                  tau2 = tau2, cc = cc)

inits <- list(beta0 = 0,
              gender_rand_eff = array(0, n_sex), # gender rand eff
              mu_sex = 0, sig_sex_randeff = 2,   # gender rand eff
              # beta_sex = 0, 
              beta = rep(0, p_temp),
              space_rand_eff = rep(0, n_reg), 
              time_rand_eff = rep(0, n_years),
              tau_space = 1, tau_time = 1, 
              rho = 0.5, 
              pred_y = rep(0.5, length(y_train)),
              betaphi_pre = 0.5,
              gamma = rep(1,p_temp), theta = rep(1,p_temp)
)

params_to_save = c('beta0', 
                   #'gender_rand_eff',          #GENDER RANDOM EFFECT
                   #'mu_sex', 'sig_sex_randeff',#GENDER RANDOM EFFECT
                   'beta_sex',                  #GENDER FIXED EFFECT
                   "beta",
                   'betaphi',
                   'space_rand_eff', 'tau_space',  
                   'time_rand_eff', 'tau_time', 
                   'rho',
                   'rmse', 'pred_y',
                   'gamma', 'theta', 'sig2')


data <- list(Y = y_train_norm)

params_to_save # check if there is what you want


# Code for Training Model -------------------------------------------------
code_train <- nimbleCode({
  ## sampling
  for(i in 1:N){
    Y[i] ~ dbeta(mu[i]*betaphi, (1-mu[i])*betaphi)
    
    pred_y[i] ~ dbeta(mu[i] * betaphi, (1 - mu[i]) * betaphi)
    
    # GENDER FIXED EFFECT
    logit(mu[i]) <- inprod(X[i,1:p], beta[1:p]) + beta_sex * (sex[i] - 1) +
      space_rand_eff[space_index[i]] + time_rand_eff[time_index[i]] + beta0
    
    # GENDER RANDOM EFFECT
    # logit(mu[i]) <- inprod(X[i,1:p], beta[1:p]) + gender_rand_eff[sex[i]] +
    #   space_rand_eff[space_index[i]] + time_rand_eff[time_index[i]] + beta0
    
    se[i] <- pow((Y[i] - pred_y[i]),2)
    rse[i] <- pow(se[i], 1/2)
  }
  
  # priors
  
  # space and time rand effects
  for(k in 1:L){
    weights[k] <- 1
  }
  space_rand_eff[1:n_reg] ~ dcar_normal(adj[1:L], weights[1:L], num[1:n_reg], tau_space, zero_mean = 1)
  tau_space ~ dgamma(shape = 0.5, rate = 0.0005)
  
  time_rand_eff[1] ~ dnorm(0,  sd = sig_time)
  for(t in 2:n_years){
    time_rand_eff[t] ~ dnorm(rho * time_rand_eff[t-1], sd = sig_time)
  }
  tau_time ~ dgamma(shape = 0.5, rate = 0.0005)
  sig_time <- 1 / sqrt(tau_time)
  
  #rho ~ dnorm(0, sd = 1)
  rho ~ dbeta(1,1)
  
  # phi parameters of the beta distrib
  # betaphi ~ dgamma(a_phi,b_phi)
  betaphi_pre ~ dbeta(1 + a_phi, 1 + b_phi)
  betaphi <- pow(a * betaphi_pre, 2)
  
  # intercept
  beta0 ~ dflat()
  
  # GENDER FIXED EFFECT PRIOR
  beta_sex ~ dnorm(0, sd = 10)
  
  # GENDER RANDOM EFFECT PRIOR
  # for(s in 1:n_sex){
  #   gender_rand_eff[s] ~ dnorm(mu_sex, sd = sig_sex_randeff)
  # }
  # mu_sex ~ dnorm(0, sd = 2)
  # sig_sex_randeff ~ dunif(1,10)
  # 
  # SSVS for betas 1:p
  for(j in 1:p){
    sig2[j] <- equals(gamma[j],0)*var_spike+equals(gamma[j],1)*var_slab 
    sig[j] <- pow(sig2[j], 1/2)
    beta[j] ~ dnorm(0, sd = sig[j]) 
    gamma[j] ~ dbern(theta[j])
  }
  var_spike <- tau2
  var_slab  <- cc*tau2
  for(j in 1:p){
    ## Two options:
    theta[j]~dunif(0,1)
  }
  
  # getting rmse
  rmse <- mean(rse[1:N])
})

# steps to run nimble -------------------------------------------------
set.seed(1)

iter = 150000
nchain = 2
burn = 50000
thin = 10

# # NB here code train and not code
# model <- nimbleModel(code_train, constants = constants, data = data, inits = inits)
# cModel <- compileNimble(model)
# conf <- configureMCMC(model, monitors = params_to_save)
# #conf$printSamplers()
# 
# MCMC <- buildMCMC(conf)
# cMCMC <- compileNimble(MCMC, project = cModel)
# 
# samples <- runMCMC(cMCMC, niter = iter, nburnin = burn, thin = thin, nchains = nchain)




#####################################################
# COMPLETE MODEL: gender as random effect ---------------------------------

# # constants for model ---
# constants <- list(X = X, #X_no_overweight,
#                   N = length(y_til), p = p_temp,
#                   n_reg = n_reg, n_years = n_years, 
#                   space_index = space_index, time_index = time_index,
#                   sex = sex, n_sex = n_sex, 
#                   adj = adj, L = L, num = num, # icar
#                   a = 50, a_phi = 0.1, b_phi = 0.1, #betaphi
#                   tau2 = tau2, cc = cc # ssvs
# )
# 
# inits <- list(beta0 = 0,
#               gender_rand_eff = array(0, n_sex), # gender rand eff
#               mu_sex = 0, sig_sex_randeff = 2,
#               beta = rep(0, p_temp),
#               space_rand_eff = rep(0, n_reg), time_rand_eff = rnorm(n_years, 0, 0.1),
#               ##space_rand_eff = array(0, c(n_sex, n_reg)), time_rand_eff = rep(0, n_years),
#               #sig_space = 1, sig_time = 1, 
#               tau_space = 1, tau_time = 1, 
#               rho = 0.5, 
#               pred_y = array(0.5, dim = N),
#               betaphi_pre = 0.5,
#               gamma = rep(1,p_temp), theta = rep(1,p_temp)
# )
# 
# params_to_save = c('beta0', 
#                    'gender_rand_eff',
#                    'mu_sex', 'sig_sex_randeff',
#                    "beta",
#                    'betaphi',
#                    'space_rand_eff', 'time_rand_eff','tau_space', 'tau_time', # 'sig_space', 'sig_time', 
#                    'rho',
#                    'rmse', 'pred_y',
#                    'gamma', 'theta', 'sig2')
# 

# code <- nimbleCode({
#   ## sampling
#   for(i in 1:N){
#     Y[i] ~ dbeta(mu[i]*betaphi, (1-mu[i])*betaphi)
#     
#     pred_y[i] ~ dbeta(mu[i] * betaphi, (1 - mu[i]) * betaphi)
#     
#     
#     logit(mu[i]) <- inprod(X[i,1:p], beta[1:p]) + gender_rand_eff[sex[i]] +
#       space_rand_eff[space_index[i]] + time_rand_eff[time_index[i]] + beta0
#     
#     se[i] <- pow((Y[i] - pred_y[i]),2)
#     rse[i] <- pow(se[i], 1/2)
#   }
#   
#   # priors
#   
#   # space and time rand effects
#   for(k in 1:L){
#     weights[k] <- 1
#   }
#   # for(s in 1:n_sex){
#   #   space_rand_eff[s,1:n_reg] ~ dcar_normal(adj[1:L], weights[1:L], num[1:n_reg], tau_space, zero_mean = 0)
#   # }
#   space_rand_eff[1:n_reg] ~ dcar_normal(adj[1:L], weights[1:L], num[1:n_reg], tau_space, zero_mean = 1)
#   tau_space ~ dgamma(shape = 0.5, rate = 0.0005)
#   
#   time_rand_eff[1] ~ dnorm(0,  sd = sig_time)
#   for(t in 2:n_years){
#     time_rand_eff[t] ~ dnorm(rho * time_rand_eff[t-1], sd = sig_time)
#   }
#   # sig_time ~ dexp(1)
#   tau_time ~ dgamma(shape = 0.5, rate = 0.0005)
#   sig_time <- 1 / sqrt(tau_time)
#   
#   #rho ~ dnorm(0, sd = 1)
#   rho ~ dbeta(1,1)
#   
#   # phi parameters of the beta distrib
#   # betaphi ~ dgamma(a_phi,b_phi)
#   betaphi_pre ~ dbeta(1 + a_phi, 1 + b_phi)
#   betaphi <- pow(a * betaphi_pre, 2)
#   
#   # intercept
#   beta0 ~ dflat()
#   
#   for(s in 1:n_sex){
#     gender_rand_eff[s] ~ dnorm(mu_sex, sd = sig_sex_randeff)
#   }
#   mu_sex ~ dnorm(0, sd = 2)
#   sig_sex_randeff ~ dunif(1,10)
#   
#   # SSVS for betas 1:p
#   for(j in 1:p){
#     sig2[j] <- equals(gamma[j],0)*var_spike+equals(gamma[j],1)*var_slab 
#     sig[j] <- pow(sig2[j], 1/2)
#     beta[j] ~ dnorm(0, sd = sig[j]) 
#     gamma[j] ~ dbern(theta[j])
#   }
#   var_spike <- tau2
#   var_slab  <- cc*tau2
#   for(j in 1:p){
#     ## Two options:
#     theta[j]~dunif(0,1)
#   }
#   
#   # getting rmse
#   rmse <- mean(rse[1:N])
# })
# 
# # steps to run nimble -------------------------------------------------
# set.seed(1)
# 
# iter = 150000
# nchain = 2
# burn = 50000
# thin = 10
# 
# model <- nimbleModel(code, constants = constants, data = data, inits = inits)
# cModel <- compileNimble(model)
# conf <- configureMCMC(model, monitors = params_to_save)
# #conf$printSamplers()
# 
# MCMC <- buildMCMC(conf)
# cMCMC <- compileNimble(MCMC, project = cModel)
# 
# samples <- runMCMC(cMCMC, niter = iter, nburnin = burn, thin = thin, nchains = nchain)


#####################################################
# COMPLETE MODEL: gender as fixed effect ---------------------------------

constants <- list(X = X, 
                  N = length(y_til), p = p_temp,
                  n_reg = n_reg, n_years = n_years, 
                  space_index = space_index, time_index = time_index,
                  sex = sex, 
                  adj = adj, L = L, num = num, # icar
                  a = 50, a_phi = 0.1, b_phi = 0.1, #betaphi
                  tau2 = tau2, cc = cc # ssvs
)

# inits <- list(beta0 = 0,
#               beta_sex = 0,
#               beta = rep(0, p_temp),
#               space_rand_eff = rep(0, n_reg), time_rand_eff = rep(0, n_years),
#               tau_space = 1, tau_time = 1, 
#               rho = 0, 
#               pred_y = array(0.5, dim = N),
#               betaphi_pre = 0.5,
#               gamma = rep(1,p_temp), theta = rep(1,p_temp)
# )
set.seed(37)
inits_list <- list(
  list(
    beta0 = runif(1,-50,50),
    beta_sex = rnorm(1),
    beta = rnorm(p_temp),
    space_rand_eff = rnorm(n_reg),
    time_rand_eff = rnorm(n_years),
    tau_space = rgamma(1,shape = 0.5, rate = 0.0005), 
    tau_time = rgamma(1,shape = 0.5, rate = 0.0005),
    rho = runif(1, 0, 0.5),
    pred_y = runif(N),
    betaphi_pre = runif(1),
    gamma = rbinom(p_temp,size = 1, prob=0.5),
    theta = runif(p_temp)
  ),
  list(
    beta0 = runif(1,-50,50),
    beta_sex = rnorm(1),
    beta = rnorm(p_temp),
    space_rand_eff = rnorm(n_reg),
    time_rand_eff = rnorm(n_years),
    tau_space = rgamma(1,shape = 0.5, rate = 0.0005), 
    tau_time = rgamma(1,shape = 0.5, rate = 0.0005),
    rho = runif(1, 0, 0.5),
    pred_y = runif(N),
    betaphi_pre = runif(1),
    gamma = rbinom(p_temp,size = 1, prob=0.5),
    theta = runif(p_temp)
  )
)
inits_list

params_to_save = c('beta0', 
                   'beta_sex',
                   "beta",
                   'betaphi',
                   'space_rand_eff', 'time_rand_eff','tau_space', 'tau_time', # 'sig_space', 'sig_time', 
                   'rho',
                   'rmse', 'pred_y',
                   'gamma', 'theta', 'sig2')

code <- nimbleCode({
  ## sampling
  for(i in 1:N){
    Y[i] ~ dbeta(mu[i]*betaphi, (1-mu[i])*betaphi)
    
    pred_y[i] ~ dbeta(mu[i] * betaphi, (1 - mu[i]) * betaphi)
    
    logit(mu[i]) <- inprod(X[i,1:p], beta[1:p]) + beta_sex * (sex[i] - 1) +
      space_rand_eff[space_index[i]] + time_rand_eff[time_index[i]] + beta0
    
    se[i] <- pow((Y[i] - pred_y[i]),2)
    rse[i] <- pow(se[i], 1/2)
  }
  
  # priors
  
  # space and time rand effects
  for(k in 1:L){
    weights[k] <- 1
  }
  space_rand_eff[1:n_reg] ~ dcar_normal(adj[1:L], weights[1:L], num[1:n_reg], tau_space, zero_mean = 1)
  tau_space ~ dgamma(shape = 0.5, rate = 0.0005)
  
  time_rand_eff[1] ~ dnorm(0,  sd = sig_time)
  for(t in 2:n_years){
    time_rand_eff[t] ~ dnorm(rho * time_rand_eff[t-1], sd = sig_time)
  }
  tau_time ~ dgamma(shape = 0.5, rate = 0.0005)
  sig_time <- 1 / sqrt(tau_time)
  
  #rho ~ dnorm(0, sd = 1)
  rho ~ dbeta(1,1)
  
  # phi parameters of the beta distrib
  # betaphi ~ dgamma(a_phi,b_phi)
  betaphi_pre ~ dbeta(1 + a_phi, 1 + b_phi)
  betaphi <- pow(a * betaphi_pre, 2)
  
  # intercept
  beta0 ~ dflat()
  
  # beta_sex
  beta_sex ~ dnorm(0, sd = 10)
  
  # for(s in 1:n_sex){
  #   gender_rand_eff[s] ~ dnorm(mu_sex, sd = sig_sex_randeff)
  # }
  #mu_sex ~ dnorm(0, sd = 2)
  #sig_sex_randeff ~ dunif(1,10)
  
  # SSVS for betas 1:p
  for(j in 1:p){
    sig2[j] <- equals(gamma[j],0)*var_spike+equals(gamma[j],1)*var_slab 
    sig[j] <- pow(sig2[j], 1/2)
    beta[j] ~ dnorm(0, sd = sig[j]) 
    gamma[j] ~ dbern(theta[j])
  }
  var_spike <- tau2
  var_slab  <- cc*tau2
  for(j in 1:p){
    ## Two options:
    theta[j]~dunif(0,1)
  }
  
  # getting rmse
  rmse <- mean(rse[1:N])
})

# # steps to run nimble -------------------------------------------------
iter = 150000
nchain = 2
burn = 50000
thin = 10

# model <- nimbleModel(code, constants = constants, data = data, inits = inits)
# cModel <- compileNimble(model)
# conf <- configureMCMC(model, monitors = params_to_save)
# #conf$printSamplers()
# 
# MCMC <- buildMCMC(conf)
# cMCMC <- compileNimble(MCMC, project = cModel)
# 
# samples <- runMCMC(cMCMC, niter = iter, nburnin = burn, thin = thin, nchains = nchain)

# The following is for the main model of the paper (M1, section 3 of paper): it just 
# changes the way we saved the output, everything else stays the same.

samples_list <- list()

for (chain in 1:2) {
  model <- nimbleModel(code, constants = constants, data = data, inits = inits_list[[chain]])
  print(model$calculate()) # if returns NA some problems in model definition so check!
  cModel <- compileNimble(model)
  conf <- configureMCMC(model, monitors = params_to_save)
  MCMC <- buildMCMC(conf)
  cMCMC <- compileNimble(MCMC, project = cModel)
  
  samples_list[[chain]] <- runMCMC(cMCMC, niter = iter, nburnin = burn, thin = thin)
}


#####################################################
# SAVED MODELS  -----------------------------------------------------------

# Complete ---------------------------------------------------
# rho with beta prior, gender specific intercept (gender random effect)
#load('REDONE_Final_Model_rhobeta_yesintercept_sexrandom')
# rho with normal prior, gender specific intercept (gender random effect)
#load('REDONE_Final_Model_rhonormal_yesintercept_sexrandom')
# rho with normal prior, gender specific intercept (gender fixed effect)
#load('Final_Model_ssvs_0.001_rhonormal_yesintercept_sexfixed')
# Model considered in the paper (see Section 3, Appendix A and model M1 in appendix B)
load('Paper_Model_rhobeta_sexfixed.Rdata') # chain 2 converged for sex and time 
                                           # effects only after 3900 iterations 
                                           # more or less

# Training ---------------------------------------------------
# the four models in Appendix B (M1, M2, M3, M4)
# load('Train_rhonormal_yesintercept_sexfixed')
# load('Train_rhobeta_yesintercept_sexfixed')
# load('Train_rhobeta_yesintercept_sexrandom')
# load('Train_rhonormal_yesintercept_sexrandom')

# mcmc chains ----------------------------
# NB different objects (samples_list vs samples) just because the final model
# (the one described in the main part of the paper) has run again after few 
# time and used a little different code for it
mcmc_samples_chain1 <- mcmc(samples_list[[1]]) # for Paper_Model_rhobeta_sexfixed.Rdata
mcmc_samples_chain2 <- mcmc(samples_list[[2]]) # for Paper_Model_rhobeta_sexfixed.Rdata
iters = 1:nrow(mcmc_samples_chain2)

mcmc_samples_chain1 <- mcmc(samples$chain1)
mcmc_samples_chain2 <- mcmc(samples$chain2)
iters = 1:nrow(mcmc_samples_chain2)


# PREDICTIONS  -----------------------------------------------

# Extract posterior means 
# NB:
# Something is called "train" but if using model trained on all 13 years
# it is still valid

# comment/uncomment for discarding or not the first observations
pred_y_train_allchains <- rbind(
  mcmc_samples_chain1[, grep("^pred_y\\[", colnames(mcmc_samples_chain1))],
  #mcmc_samples_chain2[, grep("^pred_y\\[", colnames(mcmc_samples_chain1))]
  mcmc_samples_chain2[3900:10000, grep("^pred_y\\[", colnames(mcmc_samples_chain1))]
)

posterior_mean_pred_train <- colMeans(pred_y_train_allchains)
posterior_mean_pred_train

# Prediction using the posterior means
mean(mcmc_samples_chain1[,"rho"])
mean(mcmc_samples_chain2[3900:10000,"rho"])

rho_hat <- mean(c(mcmc_samples_chain1[,"rho"], 
                  #mcmc_samples_chain2[,"rho"])
                  mcmc_samples_chain2[3900:10000,"rho"]
) )

beta_hat <- colMeans(
  rbind(mcmc_samples_chain1[, grep("^beta\\[", colnames(mcmc_samples_chain1))],
        #mcmc_samples_chain2[, grep("^beta\\[", colnames(mcmc_samples_chain1))]
        mcmc_samples_chain2[3900:10000, grep("^beta\\[", colnames(mcmc_samples_chain1))]
  ))

beta0_hat <- mean(c(mcmc_samples_chain1[,"beta0"], 
                    #mcmc_samples_chain2[,"beta0"]
                    mcmc_samples_chain2[3900:10000,"beta0"]
))

space_eff_hat <- colMeans(
  rbind(mcmc_samples_chain1[, grep("^space_rand_eff\\[", colnames(mcmc_samples_chain1))],
        #mcmc_samples_chain2[, grep("^space_rand_eff\\[", colnames(mcmc_samples_chain1))]
        mcmc_samples_chain2[3900:10000, grep("^space_rand_eff\\[", colnames(mcmc_samples_chain1))]
  ))

time_eff_hat <- colMeans(  
  rbind(mcmc_samples_chain1[, grep("^time_rand_eff\\[", colnames(mcmc_samples_chain1))],
        #mcmc_samples_chain2[, grep("^time_rand_eff\\[", colnames(mcmc_samples_chain1))]
        mcmc_samples_chain2[3900:10000, grep("^time_rand_eff\\[", colnames(mcmc_samples_chain1))]
  ))

# In case of train or complete:
# For the last time_eff_hat just use time_eff_hat[13] = time_eff_hat[12]*rho_hat

################### SEX RANDOM EFFECT ###
# gender_eff_hat <- colMeans(
#   rbind(mcmc_samples_chain1[, grep("^gender_rand_eff\\[", colnames(samples$chain1))],
#         #mcmc_samples_chain2[, grep("^gender_rand_eff\\[", colnames(samples$chain2))]
#         mcmc_samples_chain2[3900:10000, grep("^gender_rand_eff\\[", colnames(samples$chain2))]
#         ))

# # combined_intercepts = tibble(
# #   Intercept_f_ch1 = samples$chain1[,"beta0"] + samples$chain1[,"gender_rand_eff[1]"],
# #   Intercept_f_ch2 = samples$chain2[,"beta0"] + samples$chain2[,"gender_rand_eff[1]"],
# #   Intercept_m_ch1 = samples$chain1[,"beta0"] + samples$chain1[,"gender_rand_eff[2]"],
# #   Intercept_m_ch2 = samples$chain2[,"beta0"] + samples$chain2[,"gender_rand_eff[2]"]
# #   )
# combined_intercepts = tibble(
#   Intercept_f = c(samples$chain1[,"beta0"] +
#                             samples$chain1[,"gender_rand_eff[1]"],
#                       samples$chain2[,"beta0"] +
#                         samples$chain2[,"gender_rand_eff[1]"]),
#
#   Intercept_m = c(samples$chain1[,"beta0"] +
#                         samples$chain1[,"gender_rand_eff[2]"],
#                       samples$chain2[,"beta0"] +
#                         samples$chain2[,"gender_rand_eff[2]"])
#   )
# combined_intercepts
# combined_intercepts_hat = colMeans(combined_intercepts)

################### SEX FIXED EFFECT ###
beta_sex_hat <- mean(c(mcmc_samples_chain1[,"beta_sex"], 
                       #mcmc_samples_chain2[,"beta_sex"]
                       mcmc_samples_chain2[3900:10000,"beta_sex"]
))

#### Inspecting coefficients
round(beta_hat,3)
space_eff_hat
time_eff_hat
rho_hat


beta0_hat    # SEX FIXED EFFECT
beta_sex_hat # SEX FIXED EFFECT

#beta0_hat + gender_eff_hat # SEX RANDOM EFFECT
#combined_intercepts_hat # SEX RANDOM EFFECT

#  POSTERIOR PREDICTION FUNCTION 2022 -------------------
# NB: what is commented from here to "END COMMENT" is just to run if you did not
# run # SPLIT TRAIN / TEST section

# # Separate 2022 test indices 
# train_indices <- which(X_til[,43] <= 12)  # Years 1 to 12
# test_indices <- which(X_til[,43] == 13)   # Year 13 = 2022
# # Training set
# X_train <- X[train_indices,]
# y_train <- y_til[train_indices]
# space_index_train <- space_index[train_indices]
# time_index_train <- time_index[train_indices]
# sex_train <- sex[train_indices]
# 
# # Normalize training response
# y_train_norm <- min_max_normalize(y_train)
# y_train_norm[y_train_norm == 1] <- 0.999
# y_train_norm[y_train_norm == 0] <- 0.001
# 
# table(sex_train)
# y_train_norm %>% length()
# 
# y_train_norm[1:240] %>% mean()
# y_train_norm[241:480] %>% mean()
# 
# # Save min/max to denormalize predictions later 
# y_max <- max(y_train)
# y_min <- min(y_train)
# 
# min(y_til)
# max(y_til)
# # min and max overall are inside years 1 to 12
# 
# # Create design matrix for 2022
# X_test <- X[test_indices,]
# true_2022 <- y_til[test_indices]
# sex_test <- sex[test_indices]
# space_test <- space_index[test_indices]
# time_test <- rep(13, length(test_indices))  # year 13 (2022)
# END COMMENT

true_2022[1:20] %>% mean()  #females
true_2022[21:40] %>% mean() # males

y_pred_denorm = de_min_max_normalize(posterior_mean_pred_train, y_max, y_min)

# the following is not to look if you're dealing with training models
# becasue they do not have y_pred for 2022 (year 13) incorporated
y_pred_denorm[test_indices][1:20] %>% mean() # females
y_pred_denorm[test_indices][21:40] %>% mean() # males

# mean prediction for year 2022 random effect
rho_hat * time_eff_hat[12] # time random effect prediction from training model
time_eff_hat[13]

# ################### GENDER RANDOM EFFECT: Predict 2022 ### ### ###
# eta_2022 <- as.vector(X_test %*% beta_hat) +
#   space_eff_hat[space_test] + 
#   #time_eff_hat[13] + 
#   rho_hat * time_eff_hat[12] +
#   rep(c(beta0_hat + gender_eff_hat), each=20)
#gender_eff_hat #uncomment this if dealing with Thesis model rhonormal gender rand eff

# # eta_2022_no_fixed_effects=space_eff_hat[space_test] + 
# #                                            time_eff_hat[13] +
# #                                            combined_intercepts_hat
# # mu_2022_no_fixed_effects = inv_logit(eta_2022_no_fixed_effects)
# # mu_2022_real_units_no_fixed_effects <- de_min_max_normalize(mu_2022, y_max, y_min)
# # 
# # mu_2022_real_units_no_fixed_effects[1:20] %>% mean()
# # mu_2022_real_units_no_fixed_effects[21:40] %>% mean()

# ################### GENDER FIXED EFFECT: Predict 2022 ### ### ###
eta_2022 <- as.vector(X_test %*% beta_hat) + beta0_hat +
  space_eff_hat[space_test] + 
  time_eff_hat[13] +
  #rho_hat * time_eff_hat[12]+
  beta_sex_hat * c(rep(0,20),rep(1,20))

mu_2022 <- inv_logit(eta_2022)
mu_2022_real_units <- de_min_max_normalize(mu_2022, y_max, y_min)
length(mu_2022_real_units)
mu_2022_real_units = mu_2022_real_units %>% as.vector()
mu_2022_real_units[1:20] %>% mean()
mu_2022_real_units[21:40] %>% mean()

# just for curiosity see difference in normalized scale
y_til_norm[test_indices][1:20] %>% mean()
y_til_norm[test_indices][21:40] %>% mean()
mu_2022[1:20] %>% mean()
mu_2022[21:40] %>% mean()


### Compute RMSE all together and by sex with models using post mean coeffs ###

rmse_2022 <- sqrt(mean((mu_2022_real_units - true_2022)^2))
rmse_2022_f <- sqrt(mean((mu_2022_real_units[1:20] - true_2022[1:20])^2))
rmse_2022_m <- sqrt(mean((mu_2022_real_units[21:40] - true_2022[21:40])^2))
cat("RMSE on 2022:", rmse_2022, "\n")
cat("RMSE on 2022 for females:", rmse_2022_f, "\n")
cat("RMSE on 2022 for males:", rmse_2022_m, "\n")

# Visualization of errors: predicted vs observed --------------

df_plot <- tibble(
  Region = rep(reg_names,2),
  Sex = c(rep("Female", times = 20),rep("Male", times = 20)),
  Predicted = mu_2022_real_units,
  Actual = true_2022
)
df_plot %>% print(n=50)

ggplot(df_plot, aes(x = Actual, y = Predicted, color = Sex)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text_repel(aes(label = Region), size = 3, show.legend = FALSE) +
  labs(title = "Predicted vs Actual Obesity Rate (2022)",
       subtitle = paste0("RMSE: ", round(rmse_2022, 4), '; ',
                         "RMSE for females: ", round(rmse_2022_f, 4), '; ',
                         "RMSE for males: ", round(rmse_2022_m, 4)),
       x = "Actual", y = "Predicted") +
  theme_minimal()+
  facet_wrap(~Sex)


# Visualization of predictions --------------

posterior_mean_pred_denormalized = de_min_max_normalize(posterior_mean_pred_train, max(y_til), min(y_til))

# Means and split by sex (complete model)
female_preds <- posterior_mean_pred_denormalized[1:260]
male_preds <- posterior_mean_pred_denormalized[261:520]

# Means and split by sex (training model)
female_preds <- posterior_mean_pred_denormalized[1:240]
male_preds <- posterior_mean_pred_denormalized[241:480]

mean(female_preds)  
mean(male_preds)    

# Plot
boxplot(list(Female = female_preds, Male = male_preds),
        ylab = "Posterior Mean of Obesity Rate",
        main = "Posterior Predictions by Sex",
        col = c("pink", "lightblue"))

# RMSE for mean predictions --------------------------

# RMSE for each of the 520 mean predictions (COMPLETE MODELS)
sqrt(mean((posterior_mean_pred_denormalized - y_til)^2))

# RMSE for each of the 480 mean predictions (TRAINING MODELS)
sqrt(mean((posterior_mean_pred_denormalized - y_til[train_indices])^2))

# Visualization of density: predicted vs observed -------------------------------

# COMPLETE MODELS
df_plot_y_pred <- tibble(
  Region = c(rep(reg_names, each = 13),rep(reg_names, each = 13)),
  Year = c(rep(1:13,20),rep(1:13,20)),
  Sex = as.factor(c(rep("Female", times = 260),rep("Male", times = 260))),
  Predicted = posterior_mean_pred_denormalized,
  Actual = y_til)


df_plot_y_pred %>% group_by(Sex) %>% 
  summarise(Observed_mean = mean(Actual),
            Predicted_mean = mean(Predicted),
            Observed_median = median(Actual),
            Predicted_median = median(Predicted))

df_plot_y_pred_long = df_plot_y_pred %>% pivot_longer(cols = c("Predicted", 
                                                               "Actual"),
                                                      names_to = "Response_type",
                                                      values_to = "Obesity") %>% 
  mutate(Response_type = as.factor(Response_type))

df_plot_y_pred_long

df_plot_y_pred_long %>% ggplot()+
  geom_density(aes(Obesity, fill = Response_type),alpha = 0.5)+
  facet_wrap(~Sex)

df_plot_y_pred_long %>% filter(Year==13) %>% ggplot()+
  geom_density(aes(Obesity, fill = Response_type),alpha = 0.5)+
  facet_wrap(~Sex)

# TRAINING MODELS
df_plot_y_pred <- tibble(
  Region = c(rep(reg_names, each = 12),rep(reg_names, each = 12)),
  Year = c(rep(1:12,20),rep(1:12,20)),
  Sex = as.factor(c(rep("Female", times = 240),rep("Male", times = 240))),
  Predicted = posterior_mean_pred_denormalized,
  Actual = y_til[train_indices])


df_plot_y_pred %>% group_by(Sex) %>% 
  summarise(Observed_mean = mean(Actual),
            Predicted_mean = mean(Predicted),
            Observed_median = median(Actual),
            Predicted_median = median(Predicted))

df_plot_y_pred_long = df_plot_y_pred %>% pivot_longer(cols = c("Predicted", 
                                                               "Actual"),
                                                      names_to = "Response_type",
                                                      values_to = "Obesity") %>% 
  mutate(Response_type = as.factor(Response_type))

df_plot_y_pred_long

df_plot_y_pred_long %>% ggplot()+
  geom_density(aes(Obesity, fill = Response_type),alpha = 0.5)+
  facet_wrap(~Sex)

df_plot_y_pred_long %>% filter(Year==12) %>% ggplot()+
  geom_density(aes(Obesity, fill = Response_type),alpha = 0.5)+
  facet_wrap(~Sex)


# POSTERIOR PREDICTIVE CHECK ----------------------------------------------

# Predictive sampling from posterior
get_pp_samples <- function(samples, 
                           X_test, 
                           space_test, time_test, sex_test, 
                           n_draws = 10000) {
  S <- n_draws
  N <- nrow(X_test)
  
  # set.seed(42)
  sample_indices <- sample(1:nrow(samples), S)
  
  pred_mat <- matrix(NA, nrow = S, ncol = N)
  
  for (s in 1:S) {
    idx <- sample_indices[s]
    
    beta0 <- samples[idx, "beta0"]
    betaphi <- samples[idx, "betaphi"]
    beta <- samples[idx, grep("^beta\\[", colnames(samples))]
    space_eff <- samples[idx, grep("^space_rand_eff\\[", colnames(samples))]
    time_eff <- samples[idx, grep("^time_rand_eff\\[", colnames(samples))]
    rho <- samples[idx, 'rho']
    
    # Handle gender effect (random or fixed)
    if ("gender_rand_eff[1]" %in% colnames(samples)) {
      gender_eff <- samples[idx, grep("^gender_rand_eff\\[", colnames(samples))]
      gender_component <- gender_eff[sex_test]
    } else {
      beta_sex <- samples[idx, "beta_sex"]
      gender_component <- beta_sex * (sex_test - 1)
    }
    
    # Compute linear predictor
    linpred <- X_test %*% beta + beta0 + gender_component +
      space_eff[space_test] +
      time_eff[12] * rho # time_eff[time_test]
    
    mu <- inv_logit(linpred)
    
    alpha_param <- mu * betaphi
    beta_param <- (1 - mu) * betaphi
    
    pred_mat[s, ] <- rbeta(N, shape1 = alpha_param, shape2 = beta_param)
  }
  
  return(pred_mat)
}

pred_samples_ch1 <- get_pp_samples(mcmc_samples_chain1, X_test, space_test, time_test, sex_test)
#pred_samples_ch2 <- get_pp_samples(mcmc_samples_chain2, X_test, space_test, time_test, sex_test)
pred_samples_ch2 <- get_pp_samples(mcmc_samples_chain2[3900:10000,], 
                                   X_test, space_test, time_test, sex_test,
                                   n_draws = (10000-3900))

# pred_samples <- get_pp_samples(rbind(mcmc_samples_chain1, mcmc_samples_chain2),
#                                X_test, space_test, time_test, sex_test)

pred_samples = get_pp_samples(rbind(mcmc_samples_chain1,
                                    mcmc_samples_chain2[3900:10000,]),
                              X_test, space_test, time_test, sex_test)
dim(pred_samples)

# Posterior predictive mean
pred_mean <- colMeans(pred_samples)
# 95% posterior predictive intervals
pred_ci_lower <- apply(pred_samples, 2, quantile, probs = 0.025)
pred_ci_upper <- apply(pred_samples, 2, quantile, probs = 0.975)
# Denormalize predictions
pred_mean_denorm <- pred_mean * (y_max - y_min) + y_min
pred_ci_lower_denorm <- pred_ci_lower * (y_max - y_min) + y_min
pred_ci_upper_denorm <- pred_ci_upper * (y_max - y_min) + y_min
# True 2022 values
true_2022 <- y_til[test_indices]

# RMSE and MAE for mean of predictions
rmse_2022 <- sqrt(mean((pred_mean_denorm - true_2022)^2))
rmse_2022_f <- sqrt(mean((pred_mean_denorm[1:20] - true_2022[1:20])^2))
rmse_2022_m <- sqrt(mean((pred_mean_denorm[21:40] - true_2022[21:40])^2))
mae <- mean(abs(pred_mean_denorm - true_2022))
cat("RMSE:", rmse_2022, "\n")
cat("RMSE:", rmse_2022_f, "\n")
cat("RMSE:", rmse_2022_m, "\n")
cat("MAE:", mae, "\n")

# RMSE for all predictions
pred_samples_denorm <- pred_samples * (y_max - y_min) + y_min
head(pred_samples)
head(pred_samples_denorm)

# RMSE and MAE on all samples
# Subtract true_2022 from each column
squared_diffs <- (pred_samples_denorm - 
                    matrix(true_2022, 
                           nrow = nrow(pred_samples_denorm), 
                           ncol = length(true_2022), 
                           byrow = TRUE))^2
# Mean squared error per column (region)
mse_per_region <- colMeans(squared_diffs)
round(mse_per_region,4)
# RMSE across regions
rmse_overall <- sqrt(mean(mse_per_region))
rmse_overall_f <- sqrt(mean(mse_per_region[1:20]))
rmse_overall_m <- sqrt(mean(mse_per_region[21:40]))
rmse_overall
rmse_overall_f
rmse_overall_m


# Bayesian p-values (i goes from region 1 female to region 20 male, so
# 40 regions, that are the 40 cols of the matrix pred_samples)
# This is the proportion of predicted samples that are greater than 
# the observed value. 
bayes_pvalues <- sapply(1:length(true_2022), function(i) {
  mean(pred_samples[, i] > (true_2022[i] - y_min) / (y_max - y_min))
})

# Histogram of p-values
hist(bayes_pvalues, breaks = 15, main = "Bayesian p-values", xlab = "p-value")

cat("Mean Bayesian p-value:", mean(bayes_pvalues), "\n")

# Basic plot
barplot(bayes_pvalues,
        names.arg = rep(reg_names,2),
        las = 2,
        col = ifelse(bayes_pvalues < 0.05 | bayes_pvalues > 0.95, "red", "grey"),
        main = "Bayesian p-values per Region-Sex Unit",
        ylab = "Bayesian p-value",
        ylim = c(0,1))
abline(h = 0.05, col = "blue", lty = 2)
abline(h = 0.95, col = "blue", lty = 2)

# Visualizations ----------------------------------------------------------

# Plot with intervals
df_plot.tb <- tibble(
  Region = rep(reg_names,2),
  Sex = c(rep("Female", times = 20),rep("Male", times = 20)),
  True = true_2022,
  Predicted = pred_mean_denorm,
  lower = pred_ci_lower_denorm,
  upper = pred_ci_upper_denorm
)

ggplot(df_plot.tb, aes(x = True, y = Predicted, color = Sex)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text_repel(aes(label = Region), size = 3, show.legend = FALSE) +
  labs(title = "Predicted vs Actual Obesity Rate (2022)",
       subtitle = paste0("RMSE: ", round(rmse_2022, 4), '; ',
                         "RMSE for females: ", round(rmse_2022_f, 4), '; ',
                         "RMSE for males: ", round(rmse_2022_m, 4)),
       x = "Actual", y = "Predicted") +
  theme_minimal()+
  facet_wrap(~Sex)

