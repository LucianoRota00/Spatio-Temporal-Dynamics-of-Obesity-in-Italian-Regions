# Preliminaries -----------------------------------------------------------
setwd("C:\\Users\\lucia\\Desktop\\STBetaBayes_Obesity_Italy\\data_and_models")
library(tidyverse)
library(mcmcplots)

reg_names_zones = c('NO-Piemonte',"NO-Valle d'Aosta",'NO-Lombardia','NO-Liguria','NO-Emilia-Romagna',
                    'NE-Trentino-Alto Adige','NE-Veneto','NE-Friuli-Venezia Giulia',
                    'C-Toscana','C-Umbria','C-Marche','C-Lazio',
                    'S-Abruzzo','S-Molise','S-Campania','S-Puglia','S-Basilicata','S-Calabria',
                    'SI-Sicilia','SI-Sardegna')


# SAVED MODELS  -----------------------------------------------------------

# Complete ---------------------------------------------------
#load('Final_Model_ssvs_0.001_rhonormal_yesintercept_sexfixed')
load('Paper_Model_rhobeta_sexfixed.Rdata') # chain 2 is bad before 3900 iters more or less, then converged
#load('REDONE_Final_Model_rhobeta_yesintercept_sexrandom')
#load('REDONE_Final_Model_rhonormal_yesintercept_sexrandom')

# Training ---------------------------------------------------
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

# Diagnostics -------------------------------------------------------------

colnames(mcmc_samples_chain1)

# intercept (dflat) 
plot(iters, mcmc_samples_chain1[,"beta0"], type = "l", xlab = "beta0", 
     ylab = expression(beta[0]))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "beta0"], col = 'red')

# beta_sex (gender fixed effect) by sex
plot(iters, mcmc_samples_chain1[, "beta_sex"], type = "l", xlab = "Iterations", 
     ylab = expression(delta[1]))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "beta_sex"], col = 'red')

# space and time precisions
plot(iters, mcmc_samples_chain1[, "tau_space"], type = "l", xlab = "tau_space",
     ylab = expression(sig_sex_randeff))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "tau_space"], col = 'red')
acf(mcmc_samples_chain1[, "tau_space"])
acf(mcmc_samples_chain2[, "tau_space"])

plot(iters, mcmc_samples_chain1[, "tau_time"], type = "l", xlab = "sig_sex_randeff",
     ylab = expression(sig_sex_randeff))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "tau_time"], col = 'red')
acf(mcmc_samples_chain1[, "tau_time"])
acf(mcmc_samples_chain2[, "tau_time"])

# betaphi
plot(iters, mcmc_samples_chain1[, "betaphi"], type = "l", xlab = "betaphi", 
     ylab = expression(betaphi))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "betaphi"], col = 'red')
acf(mcmc_samples_chain1[, "betaphi"])
acf(mcmc_samples_chain2[, "betaphi"])

# RHO
plot(iters, mcmc_samples_chain1[, "rho"], type = "l", xlab = "rho", 
     ylab = expression(rho))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "rho"], col = 'red')
acf(mcmc_samples_chain1[, "rho"])
acf(mcmc_samples_chain2[, "rho"])

# root mse
plot(iters, mcmc_samples_chain1[, "rmse"], type = "l", xlab = "rmse", 
     ylab = expression(rmse))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "rmse"], col = 'red')
acf(mcmc_samples_chain1[, "rmse"])
acf(mcmc_samples_chain2[, "rmse"])


# betas 
# function for plotting beta traceplots
load('X_all_covariates_in_model') 
p_temp = ncol(X)
plot_beta <- function(chain, col, xlab, ylab) {
  plot(iters, mcmc_samples_chain1[, paste0("beta[", col, "]")], type = "l", 
       xlab = xlab, ylab = ylab)
  lines(iters, mcmc_samples_chain2[, paste0("beta[", col, "]")], col = 'red')
}
for (i in 1:p_temp) {
  plot_beta("chain2", i, colnames(X)[i], expression(beta[i]))
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}


# GAMMAS
# function for plotting gamma traceplots
plot_gamma <- function(chain, col, xlab, ylab) {
  plot(iters, mcmc_samples_chain1[, paste0("gamma[", col, "]")], type = "l", 
       xlab = xlab, ylab = ylab)
  lines(iters, mcmc_samples_chain2[, paste0("gamma[", col, "]")], col = 'red')
}
# for loop for plotting traceplots
for (i in 1:p_temp) {
  plot_gamma("chain2", i, colnames(X)[i], expression(gamma[i]))
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}

# THETAS
# function for plotting theta traceplots
plot_theta <- function(chain, col, xlab, ylab) {
  plot(iters, mcmc_samples_chain1[, paste0("theta[", col, "]")], type = "l", 
       xlab = xlab, ylab = ylab)
  lines(iters, mcmc_samples_chain2[, paste0("theta[", col, "]")], col = 'red')
}
# for loop for plotting traceplots
for (i in 1:p_temp) {
  plot_theta("chain2", i, colnames(X)[i], expression(theta[i]))
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}

# SIGMA2 BETAS
# function for plotting theta traceplots
plot_sig2 <- function(chain, col, xlab, ylab) {
  plot(iters, mcmc_samples_chain1[, paste0("sig2[", col, "]")], type = "l", 
       xlab = xlab, ylab = ylab)
  lines(iters, mcmc_samples_chain2[, paste0("sig2[", col, "]")], col = 'red')
}
# for loop for plotting traceplots
for (i in 1:p_temp) {
  plot_sig2("chain2", i, colnames(X)[i], expression(sigma[i]))
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}

# random effects: SPACE
n_reg = length(reg_names_zones)
plot_space_randeff <- function(chain, i, xlab, ylab) {
  plot(iters, mcmc_samples_chain1[, paste0('space_rand_eff[',i,']')], type = "l", 
       xlab = xlab, ylab = ylab)
  lines(iters, mcmc_samples_chain2[, paste0('space_rand_eff[', i,']')], col = 'red')
}
for (i in 1:n_reg) {
  plot_space_randeff("chain2", i, reg_names_zones[i], expression(space_rand_eff))
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}  

# random effects: TIME
n_years = 13
plot_time_randeff <- function(chain, i, xlab, ylab) {
  plot(iters, mcmc_samples_chain1[, paste0('time_rand_eff[',i,']')], type = "l", 
       xlab = xlab, ylab = ylab)
  lines(iters, mcmc_samples_chain2[, paste0('time_rand_eff[', i,']')], col = 'red')
}
for (i in 1:n_years) {
  plot_time_randeff("chain2", i, seq(2010,2022,1)[i], expression(time_rand_eff))
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}  

####  coeff DENSITIES ---------------------------------------

# BETAS
for(i in 1:p_temp){
  name = paste0("beta_",i)
  value_name = rlang::sym(name)
  df_betas_long = data.frame(iters,
                             mcmc_samples_chain1[,paste0("beta[",i, "]")],
                             mcmc_samples_chain2[,paste0("beta[",i, "]")])  %>% 
    pivot_longer(-iters, names_to = "chain", values_to = name) %>% 
    mutate(chain = factor(ifelse(chain == 'var1', 'chain 1', 'chain 2')))
  
  p = df_betas_long %>% ggplot()+
    geom_density(aes(!!value_name, fill = chain), alpha = 0.6) +
    labs(title = paste0('Beta coefficient for: ',  colnames(X)[i]))+
    theme_bw()
  
  print(p)
  
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  #dev.off()
}

# BETA PHI
data.frame(iters,
           mcmc_samples_chain1[,paste0('betaphi')],
           mcmc_samples_chain2[,paste0('betaphi')]) %>% 
  pivot_longer(-iters, names_to = "chain", values_to = "phi_value") %>% 
  mutate(chain = factor(ifelse(chain == 'var1', 'chain 1', 'chain 2'))) %>% 
  ggplot()+
  geom_density(aes(phi_value, fill = chain), alpha = 0.6)+
  labs(title = 'Precision parameter')

# MSE
data.frame(iters,
           mcmc_samples_chain1[,'rmse'],
           mcmc_samples_chain2[,'rmse']) %>% 
  pivot_longer(-iters, names_to = "chain", values_to = "rmse") %>% 
  mutate(chain = factor(ifelse(chain == 'var1', 'chain 1', 'chain 2'))) %>% 
  ggplot()+
  geom_density(aes(rmse, fill = chain), alpha = 0.6)+
  labs(title = 'RMSE for predictions')+
  theme_bw()

