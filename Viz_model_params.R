# preliminaries -------------
library(nimble) 
library(mcmcplots)
library(tidyverse) 
library(ggridges)
library(moments)
setwd("C:\\Users\\lucia\\Desktop\\STBetaBayes_Obesity_Italy\\data_and_models")

reg_names_zones = c('NO-Piemonte',"NO-Valle d'Aosta",'NO-Lombardia','NO-Liguria','NO-Emilia-Romagna',
                    'NE-Trentino-Alto Adige','NE-Veneto','NE-Friuli-Venezia Giulia',
                    'C-Toscana','C-Umbria','C-Marche','C-Lazio',
                    'S-Abruzzo','S-Molise','S-Campania','S-Puglia','S-Basilicata','S-Calabria',
                    'SI-Sicilia','SI-Sardegna')


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

mcmc_samples_chain1 <- mcmc(samples$chain1)
mcmc_samples_chain2 <- mcmc(samples$chain2)
iters = 1:nrow(mcmc_samples_chain2)

mcmc_samples_chain1 <- mcmc(samples_list[[1]])
mcmc_samples_chain2 <- mcmc(samples_list[[2]])
iters = 1:nrow(mcmc_samples_chain2)


# parameters to show ------------------------------------------------------------
## preliminaries --- gender random effect
# colnames(mcmc_samples_chain1)
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
# params_to_show = params_to_save[c(1,2,7,8)]

## preliminaries --- gender fixed effect
colnames(mcmc_samples_chain1)

params_to_save = c('beta0', 
                   'beta_sex',
                   "beta",
                   'betaphi',
                   'space_rand_eff', 'time_rand_eff','tau_space', 'tau_time', # 'sig_space', 'sig_time', 
                   'rho',
                   'rmse', 'pred_y',
                   'gamma', 'theta', 'sig2')

params_to_show = params_to_save[c(1,2,5,6)]


# # Filter the column names of mcmc_samples_chain1 to include only the params_to_show
# cols_rand_effects <- colnames(mcmc_samples_chain1)[
#   grepl(paste(params_to_show, collapse = "|"), colnames(mcmc_samples_chain1))
# ]
# cols_rand_effects
# 
# random_effects_df_ch1 = as_tibble(mcmc_samples_chain1) %>% select(all_of(cols_rand_effects))
# random_effects_df_ch2 = as_tibble(mcmc_samples_chain2) %>% select(all_of(cols_rand_effects))
# 
# random_effects_df_ch1 %>% colnames()

## # creating df for all intercepts ---
# intercepts_ch1 <- list()
# intercepts_ch2 <- list()
# 
# # Loop over all combinations of s, i, and t
# for (s in 1:2) {
#   for (i in 1:20) {
#     for (t in 1:13) {
#       # CHAIN 1
#       # Calculate the additive intercept
#       intercept_name <- paste0('additive_intercept_s', s, '_i', i, '_t', t)
#       intercept_value <- random_effects_df_ch1$beta0 +
#         random_effects_df_ch1[[paste0("gender_rand_eff[", s, "]")]] +
#         random_effects_df_ch1[[paste0("space_rand_eff[", i, "]")]] +
#         random_effects_df_ch1[[paste0("time_rand_eff[", t, "]")]]
#       
#       # Store in the list
#       intercepts_ch1[[intercept_name]] <- intercept_value
#       
#       # CHAIN 2
#       # Calculate the additive intercept
#       #intercept_name <- paste0('additive_intercept_s', s, '_i', i, '_t', t)
#       intercept_value <- random_effects_df_ch2$beta0 +
#         random_effects_df_ch2[[paste0("gender_rand_eff[", s, "]")]] +
#         random_effects_df_ch2[[paste0("space_rand_eff[", i, "]")]] +
#         random_effects_df_ch2[[paste0("time_rand_eff[", t, "]")]]
#       
#       # Store in the list
#       intercepts_ch2[[intercept_name]] <- intercept_value
#     }
#   }
# }
# 
# # Convert the list into a dataframe for easier manipulation
# intercepts_df_ch1 <- as.data.frame(intercepts_ch1)
# intercepts_df_ch2 <- as.data.frame(intercepts_ch2)
# 
# head(intercepts_df_ch1)
# head(intercepts_df_ch2)

## # creating df for all intercepts -PANEL FORMAT-  ---
# panel_data_intercepts_ch1 <- list()
# panel_data_intercepts_ch2 <- list()

# The following loop and code until 'STOP' is just for GENDER RANDOM EFFECT
# Loop over all combinations of s, i, and t
# row_index <- 1
# for (s in 1:2) {
#   for (i in 1:20) {
#     for (t in 1:13) {
#       # CHAIN 1
#       # Calculate the additive intercept
#       intercept_value <- random_effects_df_ch1$beta0 +
#         random_effects_df_ch1[[paste0("gender_rand_eff[", s, "]")]] +
#         random_effects_df_ch1[[paste0("space_rand_eff[", i, "]")]] +
#         random_effects_df_ch1[[paste0("time_rand_eff[", t, "]")]]
#       
#       # Append the row to the list
#       panel_data_intercepts_ch1[[row_index]] <- data.frame(
#         Iteration = iters,
#         Gender = s,
#         Region = i,
#         Time = t,
#         Intercept = intercept_value,
#         Chain = "Chain 1"
#       )
#       
#       # CHAIN 2
#       # Calculate the additive intercept
#       intercept_value <- random_effects_df_ch2$beta0 +
#         random_effects_df_ch2[[paste0("gender_rand_eff[", s, "]")]] +
#         random_effects_df_ch2[[paste0("space_rand_eff[", i, "]")]] +
#         random_effects_df_ch2[[paste0("time_rand_eff[", t, "]")]]
#       
#       # Append the row to the list
#       panel_data_intercepts_ch2[[row_index]] <- data.frame(
#         Iteration = iters,
#         Gender = s,
#         Region = i,
#         Time = t,
#         Intercept = intercept_value,
#         Chain = "Chain 2"
#       )
#       
#       row_index <- row_index + 1
#     }
#   }
# }
# 
# 
# # Combine all rows into a single dataframe
# panel_data_df_ch1 <- do.call(rbind, panel_data_intercepts_ch1)
# panel_data_df_ch2 <- do.call(rbind, panel_data_intercepts_ch2)
# 
# # check
# head(panel_data_df_ch1)
# head(panel_data_df_ch2)
# 
# # merging the two
# panel_data_df = rbind(panel_data_df_ch1,panel_data_df_ch2)
# 
# # check
# head(panel_data_df)
# tail(panel_data_df)
# panel_data_df = panel_data_df %>% as_tibble()
# 
# glimpse(panel_data_df)
# 
# 
# panel_data_df %>% filter(Region == 3, Gender == 1,
#                          
#                          Iteration>=1500
#                          
#                          ) %>% 
#   ggplot(aes(x = Iteration, y = Intercept, color = Chain)) +
#   #ggplot(plot_data_long_NO, aes(x = Iteration, y = psi_values, color = Chain)) +
#   geom_line(linewidth = 1, alpha = 0.5) +  
#   theme_minimal() +
#   facet_grid(~factor(Region),
#              scale = "free_x"#,
#              #labeller = label_parsed
#   ) +
#   labs(
#     title = "MCMC Convergence: Lineplot",       
#     #subtitle = "Sampling for North-West Space Random Effect",        
#     x = "Iteration",                               
#     y = 'Posterior Sample Values',                 
#     color = "Chain"                             
#   ) +
#   scale_color_manual(values = c("blue", "red")) +  
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),   
#     axis.title.x = element_text(size = 14),                
#     axis.title.y = element_text(size = 14),                
#     legend.position = "bottom",                            
#     legend.title = element_text(size = 12)                 
#   )+
#   facet_wrap(~factor(Time))
# 
# ###############
# print('STOP')##
# ###############

# gender ------------------------------------------------------------------
# RANDOM EFFECT
# plot_data <- data.frame(
#   Iteration = rep(iters, 2),                    
#   beta0 = c(samples$chain1[, "beta0"], samples$chain2[, "beta0"]),
#   males = c(samples$chain1[, "gender_rand_eff[2]"], samples$chain2[, "gender_rand_eff[2]"]),
#   females = c(samples$chain1[, "gender_rand_eff[1]"], samples$chain2[, "gender_rand_eff[1]"]),
#   Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
# )
# plot_data = plot_data %>% mutate(Intercept_females = beta0+females,
#                                  Intercept_males = beta0+males)


# FIXED EFFECT (comment/uncomment parts with CHAIN 2 >= 3900 ITERS if using Paper_Model_rhobeta_sexfixed.Rdata)
plot_data_adjusted <- data.frame(
  Iteration = c(iters,c(3901:length(iters))), #rep(iters, 2),                    
  beta0 = c(mcmc_samples_chain1[, "beta0"],mcmc_samples_chain2[c(3901:length(iters)), "beta0"]),
  #beta0 = c(mcmc_samples_chain1[, "beta0"],mcmc_samples_chain2[, "beta0"]),
  beta_sex = c(mcmc_samples_chain1[, "beta_sex"], mcmc_samples_chain2[c(3901:length(iters)), "beta_sex"]),
  #beta_sex = c(mcmc_samples_chain1[, "beta_sex"], mcmc_samples_chain2[, "beta_sex"]),
  Chain = c( rep("Chain 1", length(iters)),
             rep("Chain 2",length(c(3901:length(iters)))) )#rep(c("Chain 1", "Chain 2"), each = length(iters))  
)


plot_data_adjusted = plot_data_adjusted %>% mutate(Intercept_females = beta0,
                                                   Intercept_males = beta0+beta_sex,
                                                   Intercept_Difference = beta_sex)


colnames(plot_data_adjusted) 
head(plot_data_adjusted)

### ### ### ### ### ### ### ### Descriptive stats ### START
mu_1 <- mean(plot_data_adjusted$Intercept_females)
mu_2 <- mean(plot_data_adjusted$Intercept_males)
sigma_1 <- sd(plot_data_adjusted$Intercept_females)
sigma_2 <- sd(plot_data_adjusted$Intercept_males)
CV_1 <- sigma_1 / mu_1
CV_2 <- sigma_2 / mu_2
skew_1 <- skewness(plot_data_adjusted$Intercept_females)
skew_2 <- skewness(plot_data_adjusted$Intercept_males)
kurt_1 <- kurtosis(plot_data_adjusted$Intercept_females)
kurt_2 <- kurtosis(plot_data_adjusted$Intercept_males)

# Create a data frame to hold the results
results_table <- data.frame(
  Statistic = c("Mean", "Standard Deviation", "Coefficient of Variation", "Skewness", "Kurtosis"),
  Females = round( c( mu_1, sigma_1, CV_1, skew_1, kurt_1), 4 ),
  Males = round( c(mu_2, sigma_2, CV_2, skew_2, kurt_2), 4)
)
print(results_table)
### ### ### ### ### ### ### ### Descriptive stats ### END

### Plot df's
# GENDER RANDOM EFFECT
# plot_data_long <- plot_data %>%
#   pivot_longer(cols = c(Intercept_females, Intercept_males),
#                names_to = "Gender",
#                names_prefix = "Intercept_",
#                values_to = "Intercept") %>%
#   mutate(Gender = ifelse(Gender == "females", "Females", "Males"))
# 
# 
# plot_data_long$genderIntercept = factor(plot_data_long$Gender, 
#                                         # labels = c(bquote(xi[1]),
#                                         #            bquote(xi[2]))
#                                         labels = c('Females',
#                                                    'Males'))

# GENDER FIXED EFFECT
plot_data_long <- plot_data %>% select(-c(beta0, beta_sex)) %>% 
  pivot_longer(cols = c(Intercept_females, Intercept_males, Intercept_Difference),
               names_to = "Gender",
               names_prefix = "Intercept_",
               values_to = "Intercept") %>%
  mutate(Gender = ifelse(Gender == "females", "Females", 
                         ifelse(Gender == "males", "Males", "Difference")
  ) )

plot_data_long$genderIntercept = factor(plot_data_long$Gender, 
                                        # labels = c(bquote(xi[1]),
                                        #            bquote(xi[2]))
                                        labels = c(
                                          'Difference',         
                                          'Females',
                                          'Males'))


# check for beta rho sex fixed: difference between train and complete (complete has 
# a problem in the first part of chain 1)
plot_data_long %>% 
  group_by(Chain,genderIntercept) %>% 
  #group_by(genderIntercept) %>% 
  summarise(mean(Intercept))

plot_data_long %>% 
  mutate(chain_iteration=paste0(Chain,', ',Iteration)) %>%
  filter(!(grepl("^Chain 2, ", chain_iteration) & 
             as.integer(gsub("Chain 2, ", "", chain_iteration)) <= 4000)) %>% 
  #group_by(Chain,genderIntercept) %>% 
  group_by(genderIntercept) %>% 
  summarise(mean(Intercept))
# complete:
# 1 Chain 1 Difference                  2.26 
# 2 Chain 1 Females                    -1.43 
# 3 Chain 1 Males                       0.832
# 4 Chain 2 Difference                  2.04 
# 5 Chain 2 Females                    -1.38 
# 6 Chain 2 Males                       0.662

# complete(final model paper without first 3900 of chain 2):
# 1 Chain 1 Difference                  1.43 
# 2 Chain 1 Females                    -1.09 
# 3 Chain 1 Males                       0.339
# 4 Chain 2 Difference                  1.42 
# 5 Chain 2 Females                    -1.10 
# 6 Chain 2 Males                       0.314

# train:
# 1 Chain 1 Difference                  2.25 
# 2 Chain 1 Females                    -1.42 
# 3 Chain 1 Males                       0.835
# 4 Chain 2 Difference                  2.24 
# 5 Chain 2 Females                    -1.39 
# 6 Chain 2 Males                       0.844


# MCMC CONVERGENCE PLOT
plot_data_long %>% 
  
  mutate(chain_iteration=paste0(Chain,', ',Iteration)) %>% 
  
  # run this 2 lines if complete beta rho sex fixed
  # filter(!(grepl("^Chain 1, ", chain_iteration) & 
  #            as.integer(gsub("Chain 1, ", "", chain_iteration)) <= 2800)) %>% 
  
  # run this 2 lines if paper complete beta rho sex fixed
  filter(!(grepl("^Chain 2, ", chain_iteration) & 
             as.integer(gsub("Chain 2, ", "", chain_iteration)) <= 3900)) %>% 
  
  filter(
    #Chain == 'Chain 1',
    #Iteration>=1500
    genderIntercept != 'Difference'
    
  )  %>% 
  ggplot(aes(x = Iteration, y = Intercept, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  facet_grid(~factor(genderIntercept),
             scale = "free_x",
             labeller = label_parsed) +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Female and Male Intercepts",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )

# POSTERIOR DENSITIES
plot_data_long %>% 
  
  mutate(chain_iteration=paste0(Chain,', ',Iteration)) %>% 
  
  # run this 2 lines if complete beta rho sex fixed 
  # filter(!(grepl("^Chain 1, ", chain_iteration) & 
  #            as.integer(gsub("Chain 1, ", "", chain_iteration)) <= 2800)) %>% 
  
  # run this 2 lines if paper complete beta rho sex fixed
  filter(!(grepl("^Chain 2, ", chain_iteration) & 
             as.integer(gsub("Chain 2, ", "", chain_iteration)) <= 3900)) %>% 
  
  filter(
    #Chain == 'Chain 1',
    #Iteration>=1500
    genderIntercept != 'Difference'
  )  %>% 
  ggplot() +
  #geom_density(aes(Intercept, fill = interaction(Chain, Gender)), alpha = 0.5) +
  geom_density(aes(Intercept, 
                   fill = Gender
                   #fill = Chain
  ),
  alpha = 0.5) +
  labs(#title = 'Gender Random Effect Posterior Density Estimation',
    #subtitle = 'Females and Males Random Effect Samples from Chain 1 and Chain 2',
    #subtitle = 'Females and Males Random Effect Samples',
    x = expression(paste("Posterior values of ", xi[s])),
    y = 'Density',
    fill = 'Gender') +
  theme_ridges() +
  # scale_fill_manual(values = c("Chain 1.Males" = "blue", 
  #                              "Chain 1.Females" = "red", 
  #                              "Chain 2.Males" = "cyan", 
  #                              "Chain 2.Females" = "pink"),
  #                   labels = c("Ch. 1, Females",
  #                              "Ch. 2, Females",
  #                              "Ch. 1, Males",
  #                              "Ch. 2, Males")
  # )+
  theme(
    #plot.title = element_text(size = 16, face = "bold"),
    #axis.title.x = element_text(size = 14, face = "bold"),
    #axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

##

# space -------------------------------------------------------------------
# DF
reg_names_zones

plot_data_NO <- data.frame(
  Iteration = rep(iters, 2),                    
  psi1 = c(mcmc_samples_chain1[, "space_rand_eff[1]"], mcmc_samples_chain2[, "space_rand_eff[1]"]),
  psi2 = c(mcmc_samples_chain1[, "space_rand_eff[2]"], mcmc_samples_chain2[, "space_rand_eff[2]"]),
  psi3 = c(mcmc_samples_chain1[, "space_rand_eff[3]"], mcmc_samples_chain2[, "space_rand_eff[3]"]),
  psi4 = c(mcmc_samples_chain1[, "space_rand_eff[7]"], mcmc_samples_chain2[, "space_rand_eff[7]"]),
  psi5 = c(mcmc_samples_chain1[, "space_rand_eff[8]"], mcmc_samples_chain2[, "space_rand_eff[8]"]),
  Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
)

plot_data_NE <- data.frame(
  Iteration = rep(iters, 2),                    
  psi6 = c(mcmc_samples_chain1[, "space_rand_eff[4]"], mcmc_samples_chain2[, "space_rand_eff[4]"]),
  psi7 = c(mcmc_samples_chain1[, "space_rand_eff[5]"], mcmc_samples_chain2[, "space_rand_eff[5]"]),
  psi8 = c(mcmc_samples_chain1[, "space_rand_eff[6]"], mcmc_samples_chain2[, "space_rand_eff[6]"]),
  Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
)

plot_data_C <- data.frame(
  Iteration = rep(iters, 2),                    
  psi9 = c(mcmc_samples_chain1[, "space_rand_eff[9]"], mcmc_samples_chain2[, "space_rand_eff[9]"]),
  psi10 = c(mcmc_samples_chain1[, "space_rand_eff[10]"], mcmc_samples_chain2[, "space_rand_eff[10]"]),
  psi11 = c(mcmc_samples_chain1[, "space_rand_eff[11]"], mcmc_samples_chain2[, "space_rand_eff[11]"]),
  psi12 = c(mcmc_samples_chain1[, "space_rand_eff[12]"], mcmc_samples_chain2[, "space_rand_eff[12]"]),
  Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
)

plot_data_S <- data.frame(
  Iteration = rep(iters, 2),                    
  psi13 = c(mcmc_samples_chain1[, "space_rand_eff[13]"], mcmc_samples_chain2[, "space_rand_eff[13]"]),
  psi14 = c(mcmc_samples_chain1[, "space_rand_eff[14]"], mcmc_samples_chain2[, "space_rand_eff[14]"]),
  psi15 = c(mcmc_samples_chain1[, "space_rand_eff[15]"], mcmc_samples_chain2[, "space_rand_eff[15]"]),
  psi16 = c(mcmc_samples_chain1[, "space_rand_eff[16]"], mcmc_samples_chain2[, "space_rand_eff[16]"]),
  psi17 = c(mcmc_samples_chain1[, "space_rand_eff[17]"], mcmc_samples_chain2[, "space_rand_eff[17]"]),
  psi18 = c(mcmc_samples_chain1[, "space_rand_eff[18]"], mcmc_samples_chain2[, "space_rand_eff[18]"]),
  Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
)

plot_data_SI <- data.frame(
  Iteration = rep(iters, 2),                    
  psi19 = c(mcmc_samples_chain1[, "space_rand_eff[19]"], mcmc_samples_chain2[, "space_rand_eff[19]"]),
  psi20 = c(mcmc_samples_chain1[, "space_rand_eff[20]"], mcmc_samples_chain2[, "space_rand_eff[20]"]),
  Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
)

### ### ### ### ### ### ### ### Descriptive stats ### START
# North-West Region
mu_1 <- mean(plot_data_NO$psi1)
mu_2 <- mean(plot_data_NO$psi2)
mu_3 <- mean(plot_data_NO$psi3)
mu_4 <- mean(plot_data_NO$psi4)
mu_5 <- mean(plot_data_NO$psi5)

sigma_1 <- sd(plot_data_NO$psi1)
sigma_2 <- sd(plot_data_NO$psi2)
sigma_3 <- sd(plot_data_NO$psi3)
sigma_4 <- sd(plot_data_NO$psi4)
sigma_5 <- sd(plot_data_NO$psi5)

CV_1 <- sigma_1 / mu_1
CV_2 <- sigma_2 / mu_2
CV_3 <- sigma_3 / mu_3
CV_4 <- sigma_4 / mu_4
CV_5 <- sigma_5 / mu_5

skew_1 <- skewness(plot_data_NO$psi1)
skew_2 <- skewness(plot_data_NO$psi2)
skew_3 <- skewness(plot_data_NO$psi3)
skew_4 <- skewness(plot_data_NO$psi4)
skew_5 <- skewness(plot_data_NO$psi5)

kurt_1 <- kurtosis(plot_data_NO$psi1)
kurt_2 <- kurtosis(plot_data_NO$psi2)
kurt_3 <- kurtosis(plot_data_NO$psi3)
kurt_4 <- kurtosis(plot_data_NO$psi4)
kurt_5 <- kurtosis(plot_data_NO$psi5)

# North-East Region
mu_6 <- mean(plot_data_NE$psi6)
mu_7 <- mean(plot_data_NE$psi7)
mu_8 <- mean(plot_data_NE$psi8)

sigma_6 <- sd(plot_data_NE$psi6)
sigma_7 <- sd(plot_data_NE$psi7)
sigma_8 <- sd(plot_data_NE$psi8)

CV_6 <- sigma_6 / mu_6
CV_7 <- sigma_7 / mu_7
CV_8 <- sigma_8 / mu_8

skew_6 <- skewness(plot_data_NE$psi6)
skew_7 <- skewness(plot_data_NE$psi7)
skew_8 <- skewness(plot_data_NE$psi8)

kurt_6 <- kurtosis(plot_data_NE$psi6)
kurt_7 <- kurtosis(plot_data_NE$psi7)
kurt_8 <- kurtosis(plot_data_NE$psi8)

# Central Region
mu_9 <- mean(plot_data_C$psi9)
mu_10 <- mean(plot_data_C$psi10)
mu_11 <- mean(plot_data_C$psi11)
mu_12 <- mean(plot_data_C$psi12)

sigma_9 <- sd(plot_data_C$psi9)
sigma_10 <- sd(plot_data_C$psi10)
sigma_11 <- sd(plot_data_C$psi11)
sigma_12 <- sd(plot_data_C$psi12)

CV_9 <- sigma_9 / mu_9
CV_10 <- sigma_10 / mu_10
CV_11 <- sigma_11 / mu_11
CV_12 <- sigma_12 / mu_12

skew_9 <- skewness(plot_data_C$psi9)
skew_10 <- skewness(plot_data_C$psi10)
skew_11 <- skewness(plot_data_C$psi11)
skew_12 <- skewness(plot_data_C$psi12)

kurt_9 <- kurtosis(plot_data_C$psi9)
kurt_10 <- kurtosis(plot_data_C$psi10)
kurt_11 <- kurtosis(plot_data_C$psi11)
kurt_12 <- kurtosis(plot_data_C$psi12)

# Southern Region
mu_13 <- mean(plot_data_S$psi13)
mu_14 <- mean(plot_data_S$psi14)
mu_15 <- mean(plot_data_S$psi15)
mu_16 <- mean(plot_data_S$psi16)
mu_17 <- mean(plot_data_S$psi17)
mu_18 <- mean(plot_data_S$psi18)

sigma_13 <- sd(plot_data_S$psi13)
sigma_14 <- sd(plot_data_S$psi14)
sigma_15 <- sd(plot_data_S$psi15)
sigma_16 <- sd(plot_data_S$psi16)
sigma_17 <- sd(plot_data_S$psi17)
sigma_18 <- sd(plot_data_S$psi18)

CV_13 <- sigma_13 / mu_13
CV_14 <- sigma_14 / mu_14
CV_15 <- sigma_15 / mu_15
CV_16 <- sigma_16 / mu_16
CV_17 <- sigma_17 / mu_17
CV_18 <- sigma_18 / mu_18

skew_13 <- skewness(plot_data_S$psi13)
skew_14 <- skewness(plot_data_S$psi14)
skew_15 <- skewness(plot_data_S$psi15)
skew_16 <- skewness(plot_data_S$psi16)
skew_17 <- skewness(plot_data_S$psi17)
skew_18 <- skewness(plot_data_S$psi18)

kurt_13 <- kurtosis(plot_data_S$psi13)
kurt_14 <- kurtosis(plot_data_S$psi14)
kurt_15 <- kurtosis(plot_data_S$psi15)
kurt_16 <- kurtosis(plot_data_S$psi16)
kurt_17 <- kurtosis(plot_data_S$psi17)
kurt_18 <- kurtosis(plot_data_S$psi18)

# Southern Islands Region
mu_19 <- mean(plot_data_SI$psi19)
mu_20 <- mean(plot_data_SI$psi20)

sigma_19 <- sd(plot_data_SI$psi19)
sigma_20 <- sd(plot_data_SI$psi20)

CV_19 <- sigma_19 / mu_19
CV_20 <- sigma_20 / mu_20

skew_19 <- skewness(plot_data_SI$psi19)
skew_20 <- skewness(plot_data_SI$psi20)

kurt_19 <- kurtosis(plot_data_SI$psi19)
kurt_20 <- kurtosis(plot_data_SI$psi20)


results_table <- data.frame(
  Statistic = c("Mean", "SD", "CV", "Skewness", "Kurtosis"),
  
  # Northern Region
  psi1 = round(c(mu_1, sigma_1, CV_1, skew_1, kurt_1), 4),
  psi2 = round(c(mu_2, sigma_2, CV_2, skew_2, kurt_2), 4),
  psi3 = round(c(mu_3, sigma_3, CV_3, skew_3, kurt_3), 4),
  psi4 = round(c(mu_4, sigma_4, CV_4, skew_4, kurt_4), 4),
  psi5 = round(c(mu_5, sigma_5, CV_5, skew_5, kurt_5), 4),
  
  # North-East Region
  psi6 = round(c(mu_6, sigma_6, CV_6, skew_6, kurt_6), 4),
  psi7 = round(c(mu_7, sigma_7, CV_7, skew_7, kurt_7), 4),
  psi8 = round(c(mu_8, sigma_8, CV_8, skew_8, kurt_8), 4),
  
  # Central Region
  psi9 = round(c(mu_9, sigma_9, CV_9, skew_9, kurt_9), 4),
  psi10 = round(c(mu_10, sigma_10, CV_10, skew_10, kurt_10), 4),
  psi11 = round(c(mu_11, sigma_11, CV_11, skew_11, kurt_11), 4),
  psi12 = round(c(mu_12, sigma_12, CV_12, skew_12, kurt_12), 4),
  
  # Southern Region
  psi13 = round(c(mu_13, sigma_13, CV_13, skew_13, kurt_13), 4),
  psi14 = round(c(mu_14, sigma_14, CV_14, skew_14, kurt_14), 4),
  psi15 = round(c(mu_15, sigma_15, CV_15, skew_15, kurt_15), 4),
  psi16 = round(c(mu_16, sigma_16, CV_16, skew_16, kurt_16), 4),
  psi17 = round(c(mu_17, sigma_17, CV_17, skew_17, kurt_17), 4),
  psi18 = round(c(mu_18, sigma_18, CV_18, skew_18, kurt_18), 4),
  
  # Southern Islands Region
  psi19 = round(c(mu_19, sigma_19, CV_19, skew_19, kurt_19), 4),
  psi20 = round(c(mu_20, sigma_20, CV_20, skew_20, kurt_20), 4)
)

print(results_table)

psi_mean_row <- results_table[1, ]
psi_mean_numeric <- as.numeric(unlist(psi_mean_row))
psi_mean_numeric = round(psi_mean_numeric[2:length(psi_mean_numeric)],2)
#save('psi_mean_numeric', file = "...psi_mean")
load('psi_mean')

### ### ### ### ### ### ### ### Descriptive stats ### END


plot_data_long_NO <- plot_data_NO %>%
  pivot_longer(cols = c(psi1,psi2,psi3,psi4,psi5),
               names_to = "psi_i",
               values_to = "psi_values") %>%
  mutate(Region = ifelse(psi_i == "psi1", "1. Piemonte",
                         ifelse(psi_i == 'psi2', "2. Valle d'Aosta",
                                ifelse(psi_i == 'psi3', '3. Lombardia',
                                       ifelse(psi_i == 'psi4', '4. Liguria',
                                              '5. Emilia Romagna')))))

plot_data_long_NE <- plot_data_NE %>%
  pivot_longer(cols = c(psi6,psi7,psi8),
               names_to = "psi_i",
               values_to = "psi_values") %>%
  mutate(Region = ifelse(psi_i == "psi6", "6. Trentino-Alto Adige",
                         ifelse(psi_i == 'psi7', "7. Veneto",
                                '8. Friuli-Venezia Giulia')))

plot_data_long_C <- plot_data_C %>%
  pivot_longer(cols = c(psi9,psi10,psi11,psi12),
               names_to = "psi_i",
               values_to = "psi_values") %>%
  mutate(Region = ifelse(psi_i == "psi9", "9. Toscana",
                         ifelse(psi_i == 'psi10', "10. Umbria",
                                ifelse(psi_i == 'psi11', '11. Marche',
                                       '12. Lazio'))))

plot_data_long_SI <- plot_data_SI %>%
  pivot_longer(cols = c(psi19,psi20),
               names_to = "psi_i",
               values_to = "psi_values") %>%
  mutate(Region = ifelse(psi_i == "psi19", "19. Sicilia",
                         '20. Sardegna'))

plot_data_long_S <- plot_data_S %>%
  pivot_longer(cols = c(psi13,psi14,psi15,psi16,psi17,psi18),
               names_to = "psi_i",
               values_to = "psi_values") %>%
  mutate(Region = ifelse(psi_i == "psi13", "13. Abruzzo",
                         ifelse(psi_i == 'psi14', "14. Molise",
                                ifelse(psi_i == 'psi15', '15. Campania',
                                       ifelse(psi_i == 'psi16', '16. Puglia',
                                              ifelse(psi_i == 'psi17','17. Basilicata',
                                                     '18. Calabria'))))))

p_NO = ggplot(plot_data_long_NO, aes(x = Iteration, y = psi_values, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  facet_grid(~factor(Region),
             scale = "free_x"#,
             #labeller = label_parsed
  ) +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for North-West Space Random Effect",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )


p_NE = ggplot(plot_data_long_NE, aes(x = Iteration, y = psi_values, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  facet_grid(~factor(Region),
             scale = "free_x"#,
             #labeller = label_parsed
  ) +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for North-East Space Random Effect",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )


p_C = ggplot(plot_data_long_C, aes(x = Iteration, y = psi_values, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  facet_grid(~factor(Region),
             scale = "free_x"#,
             #labeller = label_parsed
  ) +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Centre Space Random Effect",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )


p_S = ggplot(plot_data_long_S, aes(x = Iteration, y = psi_values, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  facet_grid(~factor(Region),
             scale = "free_x"#,
             #labeller = label_parsed
  ) +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for South Space Random Effect",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )


p_SI = ggplot(plot_data_long_SI, aes(x = Iteration, y = psi_values, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  facet_grid(~factor(Region),
             scale = "free_x"#,
             #labeller = label_parsed
  ) +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Islands Space Random Effect",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )

p_NO
p_NE
p_C
p_S
p_SI

# DENSITIES SPACE
data_plot_space_chain2 = mcmc_samples_chain2 %>% 
  as.data.frame() %>% select(!starts_with("combination")) %>%
  rownames_to_column("ID") %>%
  pivot_longer(cols = starts_with("space_rand_eff"), 
               names_to = "Variable", 
               values_to = "Value") %>% 
  mutate(Variable = case_when(
    Variable == 'space_rand_eff[1]' ~ 'NO-Piemonte',
    Variable == 'space_rand_eff[2]' ~ "NO-Valle d'Aosta",
    Variable == 'space_rand_eff[3]' ~ 'NO-Lombardia',
    Variable == 'space_rand_eff[4]' ~ 'NE-Trentino-Alto Adige',
    Variable == 'space_rand_eff[5]' ~ 'NE-Veneto',
    Variable == 'space_rand_eff[6]' ~ 'NE-Friuli-Venezia Giulia',
    Variable == 'space_rand_eff[7]' ~ 'NO-Liguria',
    Variable == 'space_rand_eff[8]' ~ 'NO-Emilia-Romagna',
    Variable == 'space_rand_eff[9]' ~ 'C-Toscana',
    Variable == 'space_rand_eff[10]' ~ 'C-Umbria',
    Variable == 'space_rand_eff[11]' ~ 'C-Marche',
    Variable == 'space_rand_eff[12]' ~ 'C-Lazio',
    Variable == 'space_rand_eff[13]' ~ 'S-Abruzzo',
    Variable == 'space_rand_eff[14]' ~ 'S-Molise',
    Variable == 'space_rand_eff[15]' ~ 'S-Campania',
    Variable == 'space_rand_eff[16]' ~ 'S-Puglia',
    Variable == 'space_rand_eff[17]' ~ 'S-Basilicata',
    Variable == 'space_rand_eff[18]' ~ 'S-Calabria',
    Variable == 'space_rand_eff[19]' ~ 'SI-Sicilia',
    Variable == 'space_rand_eff[20]' ~ 'SI-Sardegna',
    TRUE ~ as.factor(Variable)
  )) %>% select(Value, Variable) %>% filter(Variable %in% reg_names_zones) 

data_plot_space_chain1 = mcmc_samples_chain1 %>% 
  as.data.frame() %>% select(!starts_with("combination")) %>%
  rownames_to_column("ID") %>%
  pivot_longer(cols = starts_with("space_rand_eff"), 
               names_to = "Variable", 
               values_to = "Value") %>% 
  mutate(Variable = case_when(
    Variable == 'space_rand_eff[1]' ~ 'NO-Piemonte',
    Variable == 'space_rand_eff[2]' ~ "NO-Valle d'Aosta",
    Variable == 'space_rand_eff[3]' ~ 'NO-Lombardia',
    Variable == 'space_rand_eff[4]' ~ 'NE-Trentino-Alto Adige',
    Variable == 'space_rand_eff[5]' ~ 'NE-Veneto',
    Variable == 'space_rand_eff[6]' ~ 'NE-Friuli-Venezia Giulia',
    Variable == 'space_rand_eff[7]' ~ 'NO-Liguria',
    Variable == 'space_rand_eff[8]' ~ 'NO-Emilia-Romagna',
    Variable == 'space_rand_eff[9]' ~ 'C-Toscana',
    Variable == 'space_rand_eff[10]' ~ 'C-Umbria',
    Variable == 'space_rand_eff[11]' ~ 'C-Marche',
    Variable == 'space_rand_eff[12]' ~ 'C-Lazio',
    Variable == 'space_rand_eff[13]' ~ 'S-Abruzzo',
    Variable == 'space_rand_eff[14]' ~ 'S-Molise',
    Variable == 'space_rand_eff[15]' ~ 'S-Campania',
    Variable == 'space_rand_eff[16]' ~ 'S-Puglia',
    Variable == 'space_rand_eff[17]' ~ 'S-Basilicata',
    Variable == 'space_rand_eff[18]' ~ 'S-Calabria',
    Variable == 'space_rand_eff[19]' ~ 'SI-Sicilia',
    Variable == 'space_rand_eff[20]' ~ 'SI-Sardegna',
    TRUE ~ as.factor(Variable)
  )) %>% select(Value, Variable) %>% filter(Variable %in% reg_names_zones) 


data_plot_space_chain1 = data_plot_space_chain1 %>% mutate(Chain = 'Chain 1',
                                                           Iteration = rep(
                                                             seq(1, 10000), 
                                                             each = 20))

data_plot_space_chain2 = data_plot_space_chain2 %>% mutate(Chain = 'Chain 2',
                                                           Iteration = rep(
                                                             seq(1, 10000), 
                                                             each = 20))

data_plot_space = rbind(data_plot_space_chain1,data_plot_space_chain2)
data_plot_space$Chain = as.factor(data_plot_space$Chain)
data_plot_space$Variable = as.factor(data_plot_space$Variable)
data_plot_space

data_plot_space_chain2 %>% 
  ggplot(aes(x = Value, y = factor(Variable), fill = Variable)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.75) +
  theme_ridges() +
  labs(title = "Space Random Effect Estimation",
       subtitle = 'Regional Random Effect Samples from Chain 2',
       x = 'Samples',
       y = 'Density')+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    #axis.title.x = element_text(size = 14),                
    #axis.title.y = element_text(size = 14),                
    legend.position = "none",                            
    legend.title = element_text(size = 12)                 
  )


# Adding regional areas
data_plot_space = data_plot_space %>%
  mutate(Geographic_area = case_when(
    grepl("^NO-", Variable) ~ "NO",
    grepl("^C-", Variable)  ~ "C",
    grepl("^NE-", Variable) ~ "NE",
    grepl("^S-", Variable)  ~ "S",
    grepl("^SI-", Variable) ~ "SI",
    TRUE ~ NA_character_  # In case of unexpected values
  ))
data_plot_space$Geographic_area = as.factor(data_plot_space$Geographic_area)

data_plot_space %>% ggplot(aes(x = Value, y = Variable, 
                               #fill = interaction(Chain,Variable)
                               #fill = Chain
                               fill = Geographic_area
                               #fill = Variable
)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.65) +
  theme_ridges() +
  labs(#title = "Space Random Effect Estimation",
    #subtitle = 'Regional Random Effect Samples from both Chains',
    x = expression(paste("Posterior values of ", psi[i])),
    y = 'Density')+
  # scale_fill_manual(values = c("Chain 1" = "blue",
  #                              "Chain 2" = "red"))
  
  # scale_fill_manual(values = c("Chain 1.Males" = "blue", 
  #                              "Chain 1.Females" = "red", 
  #                              "Chain 2.Males" = "cyan", 
  #                              "Chain 2.Females" = "pink"),
  #                   labels = c("Ch. 1, Females",
  #                              "Ch. 2, Females",
  #                              "Ch. 1, Males",
  #                              "Ch. 2, Males")
  # )+
  theme(
    #plot.title = element_text(size = 16, face = "bold"),   
    #axis.title.x = element_text(size = 14),                
    #axis.title.y = element_text(size = 14),                
    #legend.position = "bottom",                            
    legend.position = "none",                            
    #legend.title = element_text(size = 12)                 
  )



# time ------------------------------------------------------------------
# DF FOR MODEL PAPER BETA RHO SEX FIXED
plot_data <- data.frame(
  Iteration = c(iters,c(3901:length(iters))),                   
  alpha1 = c(mcmc_samples_chain1[, "time_rand_eff[1]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[1]"]),
  alpha2 = c(mcmc_samples_chain1[, "time_rand_eff[2]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[2]"]),
  alpha3 = c(mcmc_samples_chain1[, "time_rand_eff[3]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[3]"]),
  alpha4 = c(mcmc_samples_chain1[, "time_rand_eff[4]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[4]"]),
  alpha5 = c(mcmc_samples_chain1[, "time_rand_eff[5]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[5]"]),
  alpha6 = c(mcmc_samples_chain1[, "time_rand_eff[6]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[6]"]),
  alpha7 = c(mcmc_samples_chain1[, "time_rand_eff[7]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[7]"]),
  alpha8 = c(mcmc_samples_chain1[, "time_rand_eff[8]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[8]"]),
  alpha9 = c(mcmc_samples_chain1[, "time_rand_eff[9]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[9]"]),
  alpha10 = c(mcmc_samples_chain1[, "time_rand_eff[10]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[10]"]),
  alpha11 = c(mcmc_samples_chain1[, "time_rand_eff[11]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[11]"]),
  alpha12 = c(mcmc_samples_chain1[, "time_rand_eff[12]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[12]"]),
  alpha13 = c(mcmc_samples_chain1[, "time_rand_eff[13]"], mcmc_samples_chain2[c(3901:length(iters)), "time_rand_eff[13]"]),
  rho = c(mcmc_samples_chain1[, "rho"], mcmc_samples_chain2[c(3901:length(iters)), "rho"]),
  Chain = c( rep("Chain 1", length(iters)),
             rep("Chain 2",length(c(3901:length(iters)))) ) 
  #Chain = rep(c("Chain 1", "Chain 2"), each = length(iters)) 
)

sum( plot_data$alpha1 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha2 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha3 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha4 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha5 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha6 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha7 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha8 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha9 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha10 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha11 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha12 >= 0 ) / length(plot_data$alpha7)
sum( plot_data$alpha13 >= 0 ) / length(plot_data$alpha7)
# # DF (complete models)
# plot_data <- data.frame(
#   Iteration = rep(iters, 2),                    
#   alpha1 = c(mcmc_samples_chain1[, "time_rand_eff[1]"], mcmc_samples_chain2[, "time_rand_eff[1]"]),
#   alpha2 = c(mcmc_samples_chain1[, "time_rand_eff[2]"], mcmc_samples_chain2[, "time_rand_eff[2]"]),
#   alpha3 = c(mcmc_samples_chain1[, "time_rand_eff[3]"], mcmc_samples_chain2[, "time_rand_eff[3]"]),
#   alpha4 = c(mcmc_samples_chain1[, "time_rand_eff[4]"], mcmc_samples_chain2[, "time_rand_eff[4]"]),
#   alpha5 = c(mcmc_samples_chain1[, "time_rand_eff[5]"], mcmc_samples_chain2[, "time_rand_eff[5]"]),
#   alpha6 = c(mcmc_samples_chain1[, "time_rand_eff[6]"], mcmc_samples_chain2[, "time_rand_eff[6]"]),
#   alpha7 = c(mcmc_samples_chain1[, "time_rand_eff[7]"], mcmc_samples_chain2[, "time_rand_eff[7]"]),
#   alpha8 = c(mcmc_samples_chain1[, "time_rand_eff[8]"], mcmc_samples_chain2[, "time_rand_eff[8]"]),
#   alpha9 = c(mcmc_samples_chain1[, "time_rand_eff[9]"], mcmc_samples_chain2[, "time_rand_eff[9]"]),
#   alpha10 = c(mcmc_samples_chain1[, "time_rand_eff[10]"], mcmc_samples_chain2[, "time_rand_eff[10]"]),
#   alpha11 = c(mcmc_samples_chain1[, "time_rand_eff[11]"], mcmc_samples_chain2[, "time_rand_eff[11]"]),
#   alpha12 = c(mcmc_samples_chain1[, "time_rand_eff[12]"], mcmc_samples_chain2[, "time_rand_eff[12]"]),
#   alpha13 = c(mcmc_samples_chain1[, "time_rand_eff[13]"], mcmc_samples_chain2[, "time_rand_eff[13]"]),
#   rho = c(mcmc_samples_chain1[, "rho"], mcmc_samples_chain2[, "rho"]),
#   #tau_time = c(samples$chain1[, "tau_time"], samples$chain2[, "tau_time"]),
#   Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
# )

# DF (training models)
# plot_data <- data.frame(
#   Iteration = rep(iters, 2),                    
#   alpha1 = c(mcmc_samples_chain1[, "time_rand_eff[1]"], mcmc_samples_chain2[, "time_rand_eff[1]"]),
#   alpha2 = c(mcmc_samples_chain1[, "time_rand_eff[2]"], mcmc_samples_chain2[, "time_rand_eff[2]"]),
#   alpha3 = c(mcmc_samples_chain1[, "time_rand_eff[3]"], mcmc_samples_chain2[, "time_rand_eff[3]"]),
#   alpha4 = c(mcmc_samples_chain1[, "time_rand_eff[4]"], mcmc_samples_chain2[, "time_rand_eff[4]"]),
#   alpha5 = c(mcmc_samples_chain1[, "time_rand_eff[5]"], mcmc_samples_chain2[, "time_rand_eff[5]"]),
#   alpha6 = c(mcmc_samples_chain1[, "time_rand_eff[6]"], mcmc_samples_chain2[, "time_rand_eff[6]"]),
#   alpha7 = c(mcmc_samples_chain1[, "time_rand_eff[7]"], mcmc_samples_chain2[, "time_rand_eff[7]"]),
#   alpha8 = c(mcmc_samples_chain1[, "time_rand_eff[8]"], mcmc_samples_chain2[, "time_rand_eff[8]"]),
#   alpha9 = c(mcmc_samples_chain1[, "time_rand_eff[9]"], mcmc_samples_chain2[, "time_rand_eff[9]"]),
#   alpha10 = c(mcmc_samples_chain1[, "time_rand_eff[10]"], mcmc_samples_chain2[, "time_rand_eff[10]"]),
#   alpha11 = c(mcmc_samples_chain1[, "time_rand_eff[11]"], mcmc_samples_chain2[, "time_rand_eff[11]"]),
#   alpha12 = c(mcmc_samples_chain1[, "time_rand_eff[12]"], mcmc_samples_chain2[, "time_rand_eff[12]"]),
#   rho = c(mcmc_samples_chain1[, "rho"], mcmc_samples_chain2[, "rho"]),
#   Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
# )

colnames(plot_data) 
head(plot_data)
dim(plot_data)[1] == 10000 + 10000-3900
### ### ### ### ### ### ### ### Descriptive stats ### START
# Calculate statistics for females and males
mu_1 <- mean(plot_data$alpha1)
mu_2 <- mean(plot_data$alpha2)
mu_3 <- mean(plot_data$alpha3)
mu_4 <- mean(plot_data$alpha4)
mu_5 <- mean(plot_data$alpha5)
mu_6 <- mean(plot_data$alpha6)
mu_7 <- mean(plot_data$alpha7)
mu_8 <- mean(plot_data$alpha8)
mu_9 <- mean(plot_data$alpha9)
mu_10 <- mean(plot_data$alpha10)
mu_11 <- mean(plot_data$alpha11)
mu_12 <- mean(plot_data$alpha12)
mu_13 <- mean(plot_data$alpha13)

sigma_1 <- sd(plot_data$alpha1)
sigma_2 <- sd(plot_data$alpha2)
sigma_3 <- sd(plot_data$alpha3)
sigma_4 <- sd(plot_data$alpha4)
sigma_5 <- sd(plot_data$alpha5)
sigma_6 <- sd(plot_data$alpha6)
sigma_7 <- sd(plot_data$alpha7)
sigma_8 <- sd(plot_data$alpha8)
sigma_9 <- sd(plot_data$alpha9)
sigma_10 <- sd(plot_data$alpha10)
sigma_11 <- sd(plot_data$alpha11)
sigma_12 <- sd(plot_data$alpha12)
sigma_13 <- sd(plot_data$alpha13)

CV_1 <- sigma_1 / mu_1
CV_2 <- sigma_2 / mu_2
CV_3 <- sigma_3 / mu_3
CV_4 <- sigma_4 / mu_4
CV_5 <- sigma_5 / mu_5
CV_6 <- sigma_6 / mu_6
CV_7 <- sigma_7 / mu_7
CV_8 <- sigma_8 / mu_8
CV_9 <- sigma_9 / mu_9
CV_10 <- sigma_10 / mu_10
CV_11 <- sigma_11 / mu_11
CV_12 <- sigma_12 / mu_12
CV_13 <- sigma_13 / mu_13

skew_1 <- skewness(plot_data$alpha1)
skew_2 <- skewness(plot_data$alpha2)
skew_3 <- skewness(plot_data$alpha3)
skew_4 <- skewness(plot_data$alpha4)
skew_5 <- skewness(plot_data$alpha5)
skew_6 <- skewness(plot_data$alpha6)
skew_7 <- skewness(plot_data$alpha7)
skew_8 <- skewness(plot_data$alpha8)
skew_9 <- skewness(plot_data$alpha9)
skew_10 <- skewness(plot_data$alpha10)
skew_11 <- skewness(plot_data$alpha11)
skew_12 <- skewness(plot_data$alpha12)
skew_13 <- skewness(plot_data$alpha13)

kurt_1 <- kurtosis(plot_data$alpha1)
kurt_2 <- kurtosis(plot_data$alpha2)
kurt_3 <- kurtosis(plot_data$alpha3)
kurt_4 <- kurtosis(plot_data$alpha4)
kurt_5 <- kurtosis(plot_data$alpha5)
kurt_6 <- kurtosis(plot_data$alpha6)
kurt_7 <- kurtosis(plot_data$alpha7)
kurt_8 <- kurtosis(plot_data$alpha8)
kurt_9 <- kurtosis(plot_data$alpha9)
kurt_10 <- kurtosis(plot_data$alpha10)
kurt_11 <- kurtosis(plot_data$alpha11)
kurt_12 <- kurtosis(plot_data$alpha12)
kurt_13 <- kurtosis(plot_data$alpha13)

# Create the results table including all alpha columns
results_table <- data.frame(
  Statistic = c("Mean", "Standard Deviation", "Coefficient of Variation", "Skewness", "Kurtosis"),
  alpha1 = round( c(mu_1, sigma_1, CV_1, skew_1, kurt_1), 4 ),
  alpha2 = round( c(mu_2, sigma_2, CV_2, skew_2, kurt_2), 4 ),
  alpha3 = round( c(mu_3, sigma_3, CV_3, skew_3, kurt_3), 4 ),
  alpha4 = round( c(mu_4, sigma_4, CV_4, skew_4, kurt_4), 4 ),
  alpha5 = round( c(mu_5, sigma_5, CV_5, skew_5, kurt_5), 4 ),
  alpha6 = round( c(mu_6, sigma_6, CV_6, skew_6, kurt_6), 4 ),
  alpha7 = round( c(mu_7, sigma_7, CV_7, skew_7, kurt_7), 4 ),
  alpha8 = round( c(mu_8, sigma_8, CV_8, skew_8, kurt_8), 4 ),
  alpha9 = round( c(mu_9, sigma_9, CV_9, skew_9, kurt_9), 4 ),
  alpha10 = round( c(mu_10, sigma_10, CV_10, skew_10, kurt_10), 4 ),
  alpha11 = round( c(mu_11, sigma_11, CV_11, skew_11, kurt_11), 4 ),
  alpha12 = round( c(mu_12, sigma_12, CV_12, skew_12, kurt_12), 4 ),
  alpha13 = round( c(mu_13, sigma_13, CV_13, skew_13, kurt_13), 4 )
)

print(results_table)

### ### ### ### ### ### ### ### Descriptive stats ### END

# run this for complete models
plot_data_long <- plot_data %>%
  pivot_longer(cols = c(alpha1,alpha2,alpha3,alpha4,alpha5,alpha6,alpha7,alpha8,
                        alpha9,alpha10,alpha11,alpha12,alpha13,rho),
               names_to = "alpha_t",
               values_to = "alpha_values") %>%
  mutate(Year = ifelse(alpha_t == "alpha1", "2010",
                       ifelse(alpha_t == "alpha2", "2011",
                              ifelse(alpha_t == "alpha3", "2012",
                                     ifelse(alpha_t == "alpha4", "2013",
                                            ifelse(alpha_t == "alpha5", "2014",
                                                   ifelse(alpha_t == "alpha6", "2015",
                                                          ifelse(alpha_t == "alpha7", "2016",
                                                                 ifelse(alpha_t == "alpha8", "2017",
                                                                        ifelse(alpha_t == "alpha9", "2018",
                                                                               ifelse(alpha_t == "alpha10", "2019",
                                                                                      ifelse(alpha_t == "alpha11", "2020",
                                                                                             ifelse(alpha_t == "alpha12", "2021",
                                                                                                    ifelse(alpha_t == "alpha13", "2022",
                                                                                                           ifelse(alpha_t == "alpha13", "2022",
                                                                                                                  "rho coefficient")))))))))))))))

# run this for training models
# plot_data_long <- plot_data %>%
#   pivot_longer(cols = c(alpha1,alpha2,alpha3,alpha4,alpha5,alpha6,alpha7,alpha8,
#                         alpha9,alpha10,alpha11,alpha12,rho),
#                names_to = "alpha_t",
#                values_to = "alpha_values") %>%
#   mutate(Year = ifelse(alpha_t == "alpha1", "2010",
#                 ifelse(alpha_t == "alpha2", "2011",
#                 ifelse(alpha_t == "alpha3", "2012",
#                 ifelse(alpha_t == "alpha4", "2013",
#                 ifelse(alpha_t == "alpha5", "2014",
#                 ifelse(alpha_t == "alpha6", "2015",
#                 ifelse(alpha_t == "alpha7", "2016",
#                 ifelse(alpha_t == "alpha8", "2017",
#                 ifelse(alpha_t == "alpha9", "2018",
#                 ifelse(alpha_t == "alpha10", "2019",
#                 ifelse(alpha_t == "alpha11", "2020",
#                 ifelse(alpha_t == "alpha12", "2021",
#                 "rho coefficient")))))))))))))

# MCMC CONVERGENCE PLOT
# complete models
plot_data_long$Year = factor(plot_data_long$Year,
                             labels = c('2010','2011','2012','2013','2014','2015',
                                        '2016','2017','2018','2019','2020','2021',
                                        '2022', bquote(rho)))
#training models
#' plot_data_long$Year = factor(plot_data_long$Year,
#'                              labels = c('2010','2011','2012','2013','2014','2015',
#'                                         '2016','2017','2018','2019','2020','2021',
#'                                         #'2022', 
#'                                         bquote(rho)))

plot_data_long %>% 
  
  # mutate(chain_iteration=paste0(Chain,', ',Iteration)) %>%
  # 
  # filter(!(grepl("^Chain 2, ", chain_iteration) & 
  #            as.integer(gsub("Chain 2, ", "", chain_iteration)) <= 3900)) %>% 
  
  ggplot(aes(x = Iteration, y = alpha_values, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  facet_wrap(~factor(Year),
             scale = "free_x",
             labeller = label_parsed
  ) +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Time Random Effects",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom-right",                            
    legend.title = element_text(size = 12)                 
  )

# DENSITIES TIME 
### NB: (comment/uncomment 
#Variable == 'time_rand_eff[13]' ~ '2022', 
#        in case you're using train/complete model)
data_plot_time_chain2 = mcmc_samples_chain2 %>% as.data.frame() %>% 
  select(!starts_with("combination")) %>%
  rownames_to_column("ID") %>%
  pivot_longer(cols = starts_with("time_rand_eff"), 
               names_to = "Variable", 
               values_to = "Value") %>% 
  mutate(Variable = case_when(
    Variable == 'time_rand_eff[1]' ~ '2010',
    Variable == 'time_rand_eff[2]' ~ "2011",
    Variable == 'time_rand_eff[3]' ~ '2012',
    Variable == 'time_rand_eff[4]' ~ '2013',
    Variable == 'time_rand_eff[5]' ~ '2014',
    Variable == 'time_rand_eff[6]' ~ '2015',
    Variable == 'time_rand_eff[7]' ~ '2016',
    Variable == 'time_rand_eff[8]' ~ '2017',
    Variable == 'time_rand_eff[9]' ~ '2018',
    Variable == 'time_rand_eff[10]' ~ '2019',
    Variable == 'time_rand_eff[11]' ~ '2020',
    Variable == 'time_rand_eff[12]' ~ '2021',
    Variable == 'time_rand_eff[13]' ~ '2022',
    TRUE ~ as.factor(Variable)
  )) %>% select(Value, Variable) %>% filter(Variable %in% seq(2010,2022,1))

data_plot_time_chain1 = mcmc_samples_chain1 %>% as.data.frame() %>% 
  select(!starts_with("combination")) %>%
  rownames_to_column("ID") %>%
  pivot_longer(cols = starts_with("time_rand_eff"), 
               names_to = "Variable", 
               values_to = "Value") %>% 
  mutate(Variable = case_when(
    Variable == 'time_rand_eff[1]' ~ '2010',
    Variable == 'time_rand_eff[2]' ~ "2011",
    Variable == 'time_rand_eff[3]' ~ '2012',
    Variable == 'time_rand_eff[4]' ~ '2013',
    Variable == 'time_rand_eff[5]' ~ '2014',
    Variable == 'time_rand_eff[6]' ~ '2015',
    Variable == 'time_rand_eff[7]' ~ '2016',
    Variable == 'time_rand_eff[8]' ~ '2017',
    Variable == 'time_rand_eff[9]' ~ '2018',
    Variable == 'time_rand_eff[10]' ~ '2019',
    Variable == 'time_rand_eff[11]' ~ '2020',
    Variable == 'time_rand_eff[12]' ~ '2021',
    Variable == 'time_rand_eff[13]' ~ '2022',
    TRUE ~ as.factor(Variable)
  )) %>% select(Value, Variable) %>% filter(Variable %in% seq(2010,2022,1))


data_plot_time_chain1 = data_plot_time_chain1 %>% mutate(Chain = 'Chain 1',
                                                         Iteration = rep(
                                                           seq(1, 10000), 
                                                           each = 13
                                                           #each = 12
                                                         ))

data_plot_time_chain2 = data_plot_time_chain2 %>% mutate(Chain = 'Chain 2',
                                                         Iteration = rep(
                                                           seq(1, 10000), 
                                                           each = 13
                                                           #each = 12
                                                         ))


data_plot_time = rbind(data_plot_time_chain1,data_plot_time_chain2)
data_plot_time$Chain = as.factor(data_plot_time$Chain)
data_plot_time$Variable = as.factor(data_plot_time$Variable)
data_plot_time

## just one chain:
# data_plot_time_chain2 %>% 
#   mutate(chain_iteration=paste0(Chain,', ',Iteration)) %>%
#   filter(!(grepl("^Chain 2, ", chain_iteration) & 
#              as.integer(gsub("Chain 2, ", "", chain_iteration)) <= 3900)) %>% 
#   
#   ggplot(aes(x = Value, y = factor(Variable), fill = Variable)) +
#   geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.75) +
#   theme_ridges() +
#   labs(title = "Time Random Effect Estimation",
#        subtitle = 'Regional Random Effect Samples from Chain 2',
#        x = 'Samples',
#        y = 'Density')+
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),   
#     #axis.title.x = element_text(size = 14),                
#     #axis.title.y = element_text(size = 14),                
#     legend.position = "none",                            
#     legend.title = element_text(size = 12)                 
#   )

data_plot_time %>% 
  
  mutate(chain_iteration=paste0(Chain,', ',Iteration)) %>%
  filter(!(grepl("^Chain 2, ", chain_iteration) & 
             as.integer(gsub("Chain 2, ", "", chain_iteration)) <= 3900)) %>% 
  
  ggplot(aes(x = Value, y = Variable, 
             #fill = interaction(Chain,Variable)
             #fill = Chain
             fill = Variable
  )) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.65) +
  theme_ridges() +
  labs(#title = "Temporal Random Effect Estimation",
    #subtitle = 'Temporal Random Effect Samples from both Chains',
    x = expression(paste("Posterior values of ", alpha[t])),
    y = 'Density')+
  theme(
    #plot.title = element_text(size = 16, face = "bold"),   
    #axis.title.x = element_text(size = 14),                
    #axis.title.y = element_text(size = 14),                
    #legend.position = "bottom",                            
    legend.position = "none",                            
    #legend.title = element_text(size = 12)                 
  )#+
#scale_fill_manual(values = c("Chain 1" = "blue",
#"Chain 2" = "red"))


# Rho ---------------------------------------------------------------------
# Run first one and then ohter one to collect both rho

# COMPLETE MODELS
# load('Final_Model_Thesis_ssvs_0.001_rhobeta_yesintercept')
# load('Final_Model_Thesis_ssvs_0.001_rhonormal_yesintercept')

# TRAIN MODELS
#load('Train_rhonormal_yesintercept_sexfixed')
#load('Train_rhobeta_yesintercept_sexfixed')

rho_beta = c(mcmc_samples_chain1[, "rho"], mcmc_samples_chain2[c(3901:length(iters)), "rho"])
#rho_beta = c(mcmc_samples_chain1[, "rho"], mcmc_samples_chain2[, "rho"])
rho_normal = c((mcmc_samples_chain1[, "rho"])^2, (mcmc_samples_chain2[, "rho"])^2)

# comment/uncomment rho_beta or rho_beta if you just want to see one
plot_data <- data.frame(
  Iteration = c(iters,c(3901:length(iters))), #rep(iters, 2),                    
  rho_beta = rho_beta,
  #rho_normal = rho_normal, 
  Chain = c( rep("Chain 1", length(iters)),
             rep("Chain 2",length(c(3901:length(iters)))) ) 
  #Chain = rep(c("Chain 1", "Chain 2"), each = length(iters)) 
)

colnames(plot_data)
head(plot_data)
dim(plot_data)

### ### ### ### ### ### ### ### Descriptive stats ### START
mu_beta <- mean(plot_data$rho_beta)
mu_normal <- mean(plot_data$rho_normal)

sigma_beta <- sd(plot_data$rho_beta)
sigma_normal <- sd(plot_data$rho_normal)

CV_beta <- sigma_beta / mu_beta
CV_normal <- sigma_normal / mu_normal

skew_beta <- skewness(plot_data$rho_beta)
skew_normal <- skewness(plot_data$rho_normal)

kurt_beta <- kurtosis(plot_data$rho_beta)
kurt_normal <- kurtosis(plot_data$rho_normal)

# Create a data frame to hold the results
results_table_rho <- data.frame(
  Statistic = c("Mean", "Standard Deviation", "Coefficient of Variation", "Skewness", "Kurtosis"),
  rho_beta = round( c( mu_beta, sigma_beta, CV_beta, skew_beta, kurt_beta), 4 )#,
  #rho_normal = round( c( mu_normal, sigma_normal, CV_normal, skew_normal, kurt_normal), 4 )
)
# Display the table
print(results_table_rho)
### ### ### ### ### ### ### ### Descriptive stats ### END

plot_data_long <- plot_data %>%
  pivot_longer(
    #cols = c(rho_beta, rho_normal),
    cols = rho_beta,
    #cols = rho_normal,
    
    names_to = "rho",
    values_to = "rho_values")

plot_data_long$Chain = as.factor(plot_data_long$Chain)

# this just if you're looking at two rho's
plot_data_long$rho = factor( plot_data_long$rho,
                             labels = c( paste(bquote(rho), 'Beta-Distributed'),
                                         paste(bquote(rho), 'Normal-Distributed') ) )
# this just if you're looking at one rho
plot_data_long$rho = as.factor(plot_data_long$rho) 

# MCMC CONVERGENCE PLOT
# NB: comment/uncomment  #facet_grid(~rho)+  if looking/not looking at single rho
ggplot(plot_data_long, aes(x = Iteration, y = rho_values, 
                           color = Chain)) +
  #facet_grid(~rho)+
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Rho Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )

# POSTERIOR DENSITIES
plot_data_long %>% mutate(rho_values = rho_values) %>% 
  ggplot() +
  geom_density(aes(rho_values, fill = Chain), alpha = 0.5) +
  #facet_grid(~rho) +
  labs(title = 'Auto-Regressive Coefficient Estimation',
       subtitle = 'Coefficient Samples from Chain 1 and Chain 2',
       x = 'Auto-Regressive Coefficient',
       y = 'Density',
       fill = 'Chain') +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")
  )+
  theme(
    plot.title = element_text(size = 16, face = "bold"), 
    strip.text = element_text(size = 12),               
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )

# Posterior densities with prior
ggplot(plot_data_long) +
  # Plot posterior densities
  geom_density(aes(rho_values, fill = Chain), alpha = 0.5) +
  
  stat_function(
    fun = dbeta, args = list(1, 1), 
    aes(color = "Beta(1,1) Prior"), 
    linewidth = 1, linetype = "dashed",
    data = subset(plot_data_long, rho == "rho Beta-Distributed")  
  ) +
  
  stat_function(
    fun = dnorm, args = list(mean = 0, sd = 1), 
    aes(color = "Normal(0,1) Prior"), 
    linewidth = 1, linetype = "dotted",
    data = subset(plot_data_long, rho == "rho Normal-Distributed") 
  ) +
  
  # Faceting by the rho type (Beta or Normal specification)
  facet_grid(~rho, scales = 'free_x') +
  
  # Labels
  labs(
    title = 'Auto-Regressive Coefficient Estimation',
    subtitle = 'Coefficient Samples from Chain 1 and Chain 2 with Prior Distributions',
    x = 'Auto-Regressive Coefficient',
    y = 'Density',
    fill = 'Chain'
  ) +
  
  # Define theme and styles
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  
  # Add manual color scale for prior distributions
  scale_color_manual(values = c("Beta(1,1) Prior" = "black", "Normal(0,1) Prior" = "black")) +
  
  # Adjust the legend and other aesthetics
  theme(
    plot.title = element_text(size = 16, face = "bold"), 
    strip.text = element_text(size = 12),               
    legend.position = "bottom",                            
    legend.title = element_blank(),   # Removes the "Distribution" title from the legend
    legend.text = element_text(size = 12)
  )



# Precisions & mu (tau_gender, mu_gender, tau_space, tau_time) -----------------
# # Precision & mu Gender) -----------------
# plot_data <- data.frame(
#   Iteration = rep(iters, 2),                    
#   sig_sex = c(mcmc_samples_chain1[, "sig_sex_randeff"], mcmc_samples_chain2[, "sig_sex_randeff"]),
#   sig2_sex = c((mcmc_samples_chain1[, "sig_sex_randeff"])^2, (mcmc_samples_chain2[, "sig_sex_randeff"])^2),
#   mu_gender = c((mcmc_samples_chain1[, "mu_sex"]), (mcmc_samples_chain2[, "mu_sex"])),
#   Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
# )
# plot_data = plot_data %>% mutate(tau_gender = 1/sig2_sex)
# 
# colnames(plot_data)
# head(plot_data)

# # Descriptive stats
# mean(plot_data$sig2_sex)
# mean(plot_data$sig_sex)
# mu_tau <- mean(plot_data$sig_sex)
# mu_mu <- mean(plot_data$mu_gender)
# sigma_tau <- sd(plot_data$sig_sex)
# sigma_mu <- sd(plot_data$mu_gender)
# CV_tau <- sigma_tau / mu_tau
# CV_mu <- sigma_mu / mu_mu
# skew_tau <- skewness(plot_data$sig_sex)
# skew_mu <- skewness(plot_data$mu_gender)
# kurt_tau <- kurtosis(plot_data$sig_sex)
# kurt_mu <- kurtosis(plot_data$mu_gender)

# plot_data_long_gender <- plot_data %>%
#   pivot_longer(cols = c(tau_gender, mu_gender),
#                names_to = "Precision",
#                values_to = "tau_gender")
# # # # # # # # # # # #  MU
# plot_data_long = plot_data %>% 
#   pivot_longer(cols = mu_gender,
#                names_to = "Precision",
#                values_to = "mu_gender")  

# # MCMC CONVERGENCE PLOT (change all the titles according to tau o mu is needed 
# # to show unique plot code for both tau and mu)
# ggplot(plot_data_long, aes(x = Iteration, y = tau_gender, color = Chain)) +
#   geom_line(linewidth = 1, alpha = 0.5) +  
#   theme_minimal() +
#   labs(
#     title = "MCMC Convergence: Lineplot",       
#     subtitle = "Sampling for Precision Coefficient",        
#     x = "Iteration",                               
#     y = 'Posterior Sample Values',                 
#     color = "Chain"                             
#   ) +
#   scale_color_manual(values = c("blue", "red")) +  
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),   
#     axis.title.x = element_text(size = 14),                
#     axis.title.y = element_text(size = 14),                
#     legend.position = "bottom",                            
#     legend.title = element_text(size = 12)                 
#   )

# # POSTERIOR DENSITIES (mu)
# ggplot(plot_data_long) +
#   geom_density(aes(mu_gender, fill = Chain), alpha = 0.5) +
#   
#   stat_function(fun = dnorm, args = list(mean = 0, sd = 2), 
#                 aes(color = "Prior Distribution"), #linetype = "Prior Distribution"), 
#                 linewidth = 1) +
#   
#   labs(title = 'Mean Parameter of Gender Random Effect Estimation',
#        subtitle = 'Mean Parameter Samples from Chain 1 and Chain 2 with Prior Distribution',
#        x = 'Mean Parameter of Gender Random Effect',
#        y = 'Density',
#        fill = 'Chain') +
#   
#   theme_minimal() +
#   scale_fill_manual(values = c("blue", "red")) +
#   
#   scale_color_manual(values = c("Prior Distribution" = "black")) +
#   #scale_linetype_manual(values = c("Prior Distribution" = "dashed")) +
#   
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),   
#     legend.position = "bottom",                            
#     legend.title = element_blank(),  # Removes the "Distribution" title from the legend                      
#     legend.title.align = 0.5,                           
#     legend.text = element_text(size = 12)                 
#   )
# 
# # # # # # # # # # # #  TAU
# plot_data_long = plot_data %>%
#   pivot_longer(cols = sig_sex,
#                names_to = "Precision",
#                values_to = "sig_gender")
# 
# # POSTERIOR DENSITIES (tau)
# ggplot(plot_data_long) +
#   geom_density(aes(sig_gender, fill = Chain), alpha = 0.5) +
#   
#   stat_function(fun = dunif, args = list(0,10), 
#                 aes(color = "Prior Distribution"), #linetype = "Prior Distribution"), 
#                 linewidth = 1) +
#   
#   labs(title = 'SD Parameter of Gender Random Effect Estimation',
#        subtitle = 'SD Parameter Samples from Chain 1 and Chain 2 with Prior Distribution',
#        x = 'SD Parameter of Gender Random Effect',
#        y = 'Density',
#        fill = 'Chain') +
#   
#   theme_minimal() +
#   scale_fill_manual(values = c("blue", "red")) +
#   
#   scale_color_manual(values = c("Prior Distribution" = "black")) +
#   #scale_linetype_manual(values = c("Prior Distribution" = "dashed")) +
#   
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),   
#     legend.position = "bottom",                            
#     legend.title = element_blank(),  # Removes the "Distribution" title from the legend                      
#     legend.title.align = 0.5,                           
#     legend.text = element_text(size = 12)                 
#   )


# Precision Space ------------------------------------------------------------
plot_data <- data.frame(
  Iteration = rep(iters, 2),                    
  tau_space = c(mcmc_samples_chain1[, "tau_space"], mcmc_samples_chain2[, "tau_space"]),
  sig_space = c(sqrt(1/mcmc_samples_chain1[, "tau_space"]), sqrt(1/mcmc_samples_chain2[, "tau_space"])),
  Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
)


colnames(plot_data)
head(plot_data)

# Descriptive stats
mu_tauspace <- mean(plot_data$tau_space)

sigma_tauspace <- sd(plot_data$tau_space)

CV_tauspace <- sigma_tauspace / mu_tauspace

skew_tauspace <- skewness(plot_data$tau_space)

kurt_tauspace <- kurtosis(plot_data$tau_space)


# COMMENT/UNCOMMENT what you want: precision or standard deviation (_sigma)

# plot_data_long_space <- plot_data %>%
#   pivot_longer(cols = tau_space,
#                names_to = "Precision",
#                values_to = "tau_space")

plot_data_long_space_sigma <- plot_data %>%
  pivot_longer(cols = sig_space,
               names_to = "StdDev",
               values_to = "sig_space")

# plot_data_long = plot_data_long_space
plot_data_long = plot_data_long_space_sigma

# MCMC CONVERGENCE PLOT
ggplot(plot_data_long, aes(x = Iteration, y = sig_space, #tau_space, 
                           color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    # subtitle = "Sampling for Precision Coefficient",        
    subtitle = "Sampling for Standard Deviation Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )

# POSTERIOR DENSITIES
prior_sd <- function(sigma) {
  ifelse(sigma > 0,
         (2 * sqrt(0.0005) / gamma(0.5)) * exp(-0.0005 / sigma^2) / sigma^2,
         0)
}

ggplot(plot_data_long) +
  geom_density(aes(sig_space, #tau_space, 
                   fill = Chain), alpha = 0.5) +
  
  # stat_function(fun = dgamma, 
  #               args = list(shape = 0.5, rate = 0.0005), 
  #               aes(color = "Prior Distribution"), #linetype = "Prior Distribution"), 
  #               linewidth = 1) +
  # 
  stat_function(fun = prior_sd, aes(color = "Prior Distribution"), linewidth = 2) +
  
  labs(
    #title = 'Precision Parameter of Space Random Effect Estimation',
    title = 'Standard Deviation of Space Random Effect Estimation',
    
    #subtitle = 'Precision Parameter Samples from Chain 1 and Chain 2 with Prior Distribution',
    subtitle = 'Standard DeviationSamples from Chain 1 and Chain 2 with Prior Distribution',
    
    #x = 'Precision Parameter of Space Random Effect',
    x = 'Standard Deviation of Space Random Effect',
    
    y = 'Density',
    fill = 'Chain') +
  
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  
  scale_color_manual(values = c("Prior Distribution" = "black")) +
  #scale_linetype_manual(values = c("Prior Distribution" = "dashed")) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    legend.position = "bottom",                            
    legend.title = element_blank(),  # Removes the "Distribution" title from the legend                      
    legend.title.align = 0.5,                           
    legend.text = element_text(size = 12)                 
  )

# Precision Time ------------------------------------------------------------
# use [c(3901:length(iters)), "tau_time"] or [, "tau_time"] based on if you're
# seeing model of paper (Section 3) or other models
plot_data <- data.frame(
  Iteration = c(iters,c(3901:length(iters))),                    
  #Iteration = rep(iters, 2),                    
  tau_time = c(mcmc_samples_chain1[, "tau_time"], mcmc_samples_chain2[c(3901:length(iters)), "tau_time"]),
  #tau_time = c(mcmc_samples_chain1[, "tau_time"], mcmc_samples_chain2[, "tau_time"]),
  sig_time = c(sqrt(1/mcmc_samples_chain1[, "tau_time"]), sqrt(1/mcmc_samples_chain2[c(3901:length(iters)), "tau_time"])),
  #sig_time = c(sqrt(1/mcmc_samples_chain1[, "tau_time"]), sqrt(1/mcmc_samples_chain2[, "tau_time"])),
  Chain = c( rep("Chain 1", length(iters)),
             rep("Chain 2",length(c(3901:length(iters)))) ) 
  #Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
)

colnames(plot_data)
head(plot_data)

# Descriptive stats
mu_tautime <- mean(plot_data$tau_time)

sigma_tautime <- sd(plot_data$tau_time)

CV_tautime <- sigma_tautime / mu_tautime

skew_tautime <- skewness(plot_data$tau_time)

kurt_tautime <- kurtosis(plot_data$tau_time)

# COMMENT/UNCOMMENT what you want: precision or standard deviation (_sigma)
# plot_data_long_time <- plot_data %>%
#   pivot_longer(cols = tau_time,
#                names_to = "Precision",
#                values_to = "tau_time")

plot_data_long_time_sigma <- plot_data %>%
  pivot_longer(cols = sig_time,
               names_to = "Precision",
               values_to = "sig_time")

plot_data_long = plot_data_long_time_sigma

# MCMC CONVERGENCE PLOT
ggplot(plot_data_long, aes(x = Iteration, y = sig_time, #tau_time, 
                           color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    #subtitle = "Sampling for Precision Coefficient",        
    subtitle = "Sampling for Standard Deviation",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )

# POSTERIOR DENSITIES
prior_sd <- function(sigma) {
  ifelse(sigma > 0,
         (2 * sqrt(0.0005) / gamma(0.5)) * exp(-0.0005 / sigma^2) / sigma^2,
         0)
}

ggplot(plot_data_long) +
  geom_density(aes(sig_time, #tau_time,
                   fill = Chain), alpha = 0.5) +
  
  # stat_function(fun = dgamma, args = list(shape = 0.5, rate = 0.0005), 
  #               aes(color = "Prior Distribution"), #linetype = "Prior Distribution"), 
  #               linewidth = 1) +
  
  stat_function(fun = prior_sd, aes(color = "Prior Distribution"), linewidth = 2) +
  
  labs(
    #title = 'Precision Parameter of Time Random Effect Estimation',
    title = 'Standard Deviation of Time Random Effect Estimation',
    
    #subtitle = 'Precision Parameter Samples from Chain 1 and Chain 2 with Prior Distribution',
    subtitle = 'Standard Deviation Samples from Chain 1 and Chain 2 with Prior Distribution',
    
    #x = 'Precision Parameter of Time Random Effect',
    x = 'Standard Deviation of Time Random Effect',
    y = 'Density',
    fill = 'Chain') +
  
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  
  scale_color_manual(values = c("Prior Distribution" = "black")) +
  #scale_linetype_manual(values = c("Prior Distribution" = "dashed")) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    legend.position = "bottom",                            
    legend.title = element_blank(),  # Removes the "Distribution" title from the legend                      
    legend.title.align = 0.5,                           
    legend.text = element_text(size = 12)                 
  )


# SUMMARY of all the hyper-parameters --------------------
results_table_hyperpriors <- data.frame(
  Statistic = c("Mean", "Standard Deviation", "Coefficient of Variation", "Skewness", "Kurtosis"),
  #mu_gender = round( c( mu_mu, sigma_mu, CV_mu, skew_mu, kurt_mu), 4 ),
  #tau_gender = round( c( mu_tau, sigma_tau, CV_tau, skew_tau, kurt_tau), 4 ),
  tau_tautime = round( c( mu_tautime, sigma_tautime, CV_tautime, skew_tautime, kurt_tautime), 4 ),
  tau_tauspace = round( c( mu_tauspace, sigma_tauspace, CV_tauspace, skew_tauspace, kurt_tauspace), 4 )
)
print(results_table_hyperpriors)


# Precision (beta-phi) ----------------------------------------------------
# DF
plot_data <- data.frame(
  Iteration = rep(iters, 2),                    
  betaphi = c(mcmc_samples_chain1[, "betaphi"], mcmc_samples_chain2[, "betaphi"]),
  Chain = rep(c("Chain 1", "Chain 2"), each = length(iters))  
)

de_min_max_normalize = function(x_norm, x_max, x_min){
  x = x_norm * (x_max-x_min) + x_min
  return(x)
}

phi_norm_samples = plot_data$betaphi
phi_norm_max = max(phi_norm_samples)
phi_norm_min = min(phi_norm_samples)

phi_samples = de_min_max_normalize(phi_norm_samples, phi_norm_max, phi_norm_min)

plot_data = plot_data %>% mutate(betaphi_original_scale = phi_samples)

colnames(plot_data) #= #c('mu_females', 'mu_males', 'precision', 'common_mu',
# 'variance_females', 'variance_males', 'variance_common_mu',
#'Chain', 'Iteration')
head(plot_data)

# Descriptive stats
# Calculate statistics for females and males
mu <- mean(plot_data$betaphi)
mu_orig <- mean(plot_data$betaphi_original_scale)

sigma <- sd(plot_data$betaphi)
sigma_orig <- sd(plot_data$betaphi_original_scale)

CV <- sigma / mu
CV_orig <- sigma_orig / mu_orig

skew <- skewness(plot_data$betaphi)
skew_orig <- skewness(plot_data$betaphi_original_scale)

kurt <- kurtosis(plot_data$betaphi)
kurt_orig <- kurtosis(plot_data$betaphi_original_scale)

# Create a data frame to hold the results
results_table_betaphi <- data.frame(
  Statistic = c("Mean", "Standard Deviation", "Coefficient of Variation", "Skewness", "Kurtosis"),
  betaphi = round( c( mu, sigma, CV, skew, kurt), 4 ),
  betaphi_orig = round( c( mu_orig, sigma_orig, CV_orig, skew_orig, kurt_orig), 4 )
)
# Display the table
print(results_table_betaphi)

plot_data_long_betaphi_norm <- plot_data %>% select(-betaphi_original_scale) %>%
  pivot_longer(cols = betaphi,
               names_to = "Scaling",
               values_to = "betaphi")

plot_data_long_betaphi_orig <- plot_data %>% select(-betaphi) %>% 
  pivot_longer(cols = betaphi_original_scale,
               names_to = "Scaling",
               values_to = "betaphi")

plot_data_long_betaphi <- plot_data %>%
  pivot_longer(cols = c(betaphi,betaphi_original_scale),
               names_to = "Scaling",
               values_to = "betaphi")

# MCMC CONVERGENCE PLOT
ggplot(plot_data_long_betaphi_norm, aes(x = Iteration, y = betaphi, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Precision Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )

# POSTERIOR DENSITIES NORM
ggplot(plot_data_long_betaphi_norm) +
  geom_density(aes(betaphi, fill = Chain), alpha = 0.5) +
  
  stat_function(fun = function(x) dbeta(x / 50, 1.1, 1.1) * (1 / 50), 
                aes(color = "Prior Distribution"), 
                linewidth = 1) +
  
  labs(title = 'Precision Parameter of the Beta Likelihood',
       subtitle = 'Precision Parameter Samples from Chain 1 and Chain 2 with Prior Distribution',
       x = 'Precision Parameter',
       y = 'Density',
       fill = 'Chain') +
  
  xlim(0,50) +
  
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  
  scale_color_manual(values = c("Prior Distribution" = "black")) +
  #scale_linetype_manual(values = c("Prior Distribution" = "dashed")) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    legend.position = "bottom",                            
    legend.title = element_blank(),  # Removes the "Distribution" title from the legend                      
    legend.title.align = 0.5,                           
    legend.text = element_text(size = 12)                 
  )

# POSTERIOR DENSITIES ORIG
ggplot(plot_data_long_betaphi_orig) +
  geom_density(aes(betaphi, fill = Chain), alpha = 0.5) +
  
  stat_function(fun = function(x) dbeta(x / 50^2, 1.1, 1.1) * (1 / 50^2), 
                aes(color = "Prior Distribution"), 
                linewidth = 1) +
  
  labs(title = 'Precision Parameter of the Beta Likelihood',
       subtitle = 'Precision Parameter Samples from Chain 1 and Chain 2 with Prior Distribution',
       x = 'Precision Parameter',
       y = 'Density',
       fill = 'Chain') +
  
  xlim(0,2500) +
  
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) +
  
  scale_color_manual(values = c("Prior Distribution" = "black")) +
  #scale_linetype_manual(values = c("Prior Distribution" = "dashed")) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    legend.position = "bottom",                            
    legend.title = element_blank(),  # Removes the "Distribution" title from the legend                      
    legend.title.align = 0.5,                           
    legend.text = element_text(size = 12)                 
  )







# betas -------------------------------------------------------------------
load('X_all_covariates_in_model')
p_temp = ncol(X)
df_betas_long = tibble(Iteration = c(), chain1 = c(), chain2 = c())
for(i in 1:p_temp){
  df_betas_long_provv = tibble(Iteration = iters,
                               chain1 = mcmc_samples_chain1[,paste0("beta[",i, "]")],
                               chain2 =  mcmc_samples_chain2[,paste0("beta[",i, "]")])  %>% 
    pivot_longer(cols = c(chain1, chain2), 
                 names_to = "Chain", 
                 values_to = 'beta') %>% 
    mutate(Chain = factor(Chain))
  
  
  
  df_betas_long_provv = df_betas_long_provv %>% mutate(Covariate = rep(colnames(X)[i], 20000))
  
  df_betas_long = rbind(df_betas_long, df_betas_long_provv)
}

df_betas_long = df_betas_long %>% mutate(Covariate_number = rep(seq(1,p_temp,1), each = 20000))

variable_names <- c(
  "Overweight",                    # corresponds to "Overweight"
  "Overweight of younger",          # corresponds to "Overweight_minor_age"
  "Population",                     # corresponds to "Population"
  "More than 65",           # corresponds to "More_65"
  "Foreigners",                     # corresponds to "Foreign_perc"
  "Foreigners from UE",             # corresponds to "Foreign_ue_perc"
  "More than 65 living alone",      # corresponds to "More_65_alone"
  "Unemployment",                   # corresponds to "unemployment_perc"
  "Cigarettes consumption",         # corresponds to "n_cigarettes"
  "Complete breakfast",             # corresponds to "Complete_breakfast_perc"
  "Daily cheese consumption",       # corresponds to "Daily_cheese_perc"
  "Daily vegetables consumption",   # corresponds to "Daily_vegetables_perc"
  "Dinner as principal meal",       # corresponds to "Dinner_principal_meal_perc"
  "Red meat consumption",           # corresponds to "Red_meat_more_weekly_perc"
  "Fish consumption",               # corresponds to "Fish_more_weekly_perc"
  "No sport participation",         # corresponds to "No_sport_perc"
  "Bad wealth",                     # corresponds to "bad_wealth_perc"
  "Life expectancy (LE)",           # corresponds to "life_exp"
  "LE in good wealth",              # corresponds to "life_exp_good_wealth"
  "LE without limitations",         # corresponds to "life_exp_no_limitations"
  "Drug consumption",               # corresponds to "drug_consumption_perc"
  "Smoking rate",                   # corresponds to "Smoking_perc"
  "Adequate nutrition",             # corresponds to "Adequate_nutrition_perc"
  "Alcohol consumption",            # corresponds to "Alcohol_cons_perc"
  "Life satisfaction",              # corresponds to "Life_satisfaction_perc"
  "Mean family components",         # corresponds to "Mean_family_components"
  "Wedding rates",                  # corresponds to "wedding_perc"
  "University (from Bachelor's to PhD)", # corresponds to "Education_5_more"
  "Gross Income",                   # corresponds to "Gross_income"
  "Health expenditure per capita",  # corresponds to "health_expenditure_by_pop"
  "PC1",                            # corresponds to "PC1"
  "PC2"                             # corresponds to "PC2"
)

length(variable_names) == length(colnames(X))

# variable_names <- paste(seq_along(variable_names), variable_names)

# df_betas_long$Covariate = factor(df_betas_long$Covariate, 
#                                         labels = variable_names)
df_betas_long$Covariate = rep(variable_names,each = 20000)
# <= 4
df_betas_long %>% filter(Covariate_number <= 4) %>% 
  ggplot(aes(x = Iteration, y = beta, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  facet_wrap(~Covariate) +
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Fixed Effect Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    strip.text = element_text(size = 12), 
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )
# %in% c(5,8)
df_betas_long %>% filter(Covariate_number %in% c(5:8)) %>% 
  ggplot(aes(x = Iteration, y = beta, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  facet_wrap(~Covariate) +
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Fixed Effect Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"), 
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )
# %in% c(9:12)
df_betas_long %>% filter(Covariate_number %in% c(9:12)) %>% 
  ggplot(aes(x = Iteration, y = beta, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  facet_wrap(~Covariate) +
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Fixed Effect Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    axis.title.x = element_text(size = 14),    
    strip.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )
# %in% c(13:16)
df_betas_long %>% filter(Covariate_number %in% c(13:16)) %>% 
  ggplot(aes(x = Iteration, y = beta, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  facet_wrap(~Covariate) +
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Fixed Effect Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )
# %in% c(17:20)
df_betas_long %>% filter(Covariate_number %in% c(17:20)) %>% 
  ggplot(aes(x = Iteration, y = beta, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  facet_wrap(~Covariate) +
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Fixed Effect Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )
# %in% c(21:24)
df_betas_long %>% filter(Covariate_number %in% c(21:24)) %>% 
  ggplot(aes(x = Iteration, y = beta, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  facet_wrap(~Covariate) +
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Fixed Effect Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )
# %in% c(25:28)
df_betas_long %>% filter(Covariate_number %in% c(25:28)) %>% 
  ggplot(aes(x = Iteration, y = beta, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  facet_wrap(~Covariate) +
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Fixed Effect Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )
# %in% c(29:32)
df_betas_long %>% filter(Covariate_number %in% c(29:32)) %>% 
  ggplot(aes(x = Iteration, y = beta, color = Chain)) +
  geom_line(linewidth = 1, alpha = 0.5) +  
  facet_wrap(~Covariate) +
  theme_minimal() +
  labs(
    title = "MCMC Convergence: Lineplot",       
    subtitle = "Sampling for Fixed Effect Coefficient",        
    x = "Iteration",                               
    y = 'Posterior Sample Values',                 
    color = "Chain"                             
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),   
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),                
    axis.title.y = element_text(size = 14),                
    legend.position = "bottom",                            
    legend.title = element_text(size = 12)                 
  )

####

mcmc_samples_all = rbind(mcmc_samples_chain1,mcmc_samples_chain2)
mcmc_samples_all %>% dim()

colnames(mcmc_samples_all)
grepl("gamma",colnames(mcmc_samples_all))
cols_idx_gamma<- which(grepl("gamma",colnames(mcmc_samples_all)))
post_gamma <-as.matrix(mcmc_samples_all[,cols_idx_gamma])
#sample mean, column by column
post_mean_gamma <- apply(post_gamma,2,"mean") 
# bar plot of the posterior inclusion probabilities
names(post_mean_gamma) <- colnames(X) # insert X if Overweight included, X_no_overweight if not 
# names(post_mean_gamma)[p+1] = 'sex' COMMENTED BC SEX IS THE INTERCEPT IN THIS MODEL
# Convert to data frame
variable_names <- c(
  "Overweight",                    # corresponds to "Overweight"
  "Overweight of younger",          # corresponds to "Overweight_minor_age"
  "Population",                     # corresponds to "Population"
  "More than 65",           # corresponds to "More_65"
  "Foreigners",                     # corresponds to "Foreign_perc"
  "Foreigners from UE",             # corresponds to "Foreign_ue_perc"
  "More than 65 living alone",      # corresponds to "More_65_alone"
  "Unemployment",                   # corresponds to "unemployment_perc"
  "Cigarettes consumption",         # corresponds to "n_cigarettes"
  "Complete breakfast",             # corresponds to "Complete_breakfast_perc"
  "Daily cheese consumption",       # corresponds to "Daily_cheese_perc"
  "Daily vegetables consumption",   # corresponds to "Daily_vegetables_perc"
  "Dinner as principal meal",       # corresponds to "Dinner_principal_meal_perc"
  "Red meat consumption",           # corresponds to "Red_meat_more_weekly_perc"
  "Fish consumption",               # corresponds to "Fish_more_weekly_perc"
  "No sport participation",         # corresponds to "No_sport_perc"
  "Bad wealth",                     # corresponds to "bad_wealth_perc"
  "Life expectancy",                # corresponds to "life_exp"
  "Life expectancy in good wealth", # corresponds to "life_exp_good_wealth"
  "LE without limitations",         # corresponds to "life_exp_no_limitations"
  "Drug consumption",               # corresponds to "drug_consumption_perc"
  "Smoking rate",                   # corresponds to "Smoking_perc"
  "Adequate nutrition",             # corresponds to "Adequate_nutrition_perc"
  "Alcohol consumption",            # corresponds to "Alcohol_cons_perc"
  "Life satisfaction",              # corresponds to "Life_satisfaction_perc"
  "Mean family components",         # corresponds to "Mean_family_components"
  "Wedding rates",                  # corresponds to "wedding_perc"
  "University (from Bachelor's to PhD)", # corresponds to "Education_5_more"
  "Gross Income",                   # corresponds to "Gross_income"
  "Health expenditure per capita",  # corresponds to "health_expenditure_by_pop"
  "PC1",                            # corresponds to "PC1"
  "PC2"                             # corresponds to "PC2"
)

post_mean_gamma_df <- data.frame(variable = names(post_mean_gamma), value = post_mean_gamma)
post_mean_gamma_df = post_mean_gamma_df %>% mutate(variable = variable_names)
ggplot(post_mean_gamma_df, aes(x = reorder(variable, -value), y = value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(y = "Inclusion Probability", 
       x = "Covariates", 
       title = ""
       #"Posterior Inclusion Probability"
  )+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,
                               size = 12),
    plot.title = element_text(size = 16, face = "bold"), 
    axis.title.x = element_text(size = 12),                
    axis.title.y = element_text(size = 14)                 
  )+
  scale_y_continuous(breaks = seq(0,1,0.15))

