# PRELIMINARIES TO RUN BEFORE ANYTHING ---------------------------------
library(nimble) 
# install.packages(
#   "https://cran.r-project.org/src/contrib/Archive/mcmcplots/mcmcplots_0.4.tar.gz",
#   repos = NULL,
#   type = "source"
# )
# library(mcmcplots) # to substitute with Bayes Plot if needed
library(tidyverse) 
library(ggridges)
library(ggrepel)
library(coda)
library(posterior)

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

# Function to compute tau for SSVS
compute_tau <- function(intersect, c_ss) {
  cc <- c_ss^2
  tau <- intersect / sqrt(2 * log(c_ss) * cc / (cc - 1))
  return(tau)
}
# Function for making each SSVS prior plot
plot_ssvs <- function(intersect, c_ss, limit) {
  tau_ss <- compute_tau(intersect, c_ss)
  
  spike_sd <- tau_ss
  slab_sd  <- tau_ss * c_ss
  
  # assuming a mixing weight of 0.5
  curve(
    0.5 * dnorm(x, 0, spike_sd) + 
      0.5 * dnorm(x, 0, slab_sd),
    from = -limit, to = limit,
    ylab = "Density", xlab = "x",
    main = paste0("Intersection = ", intersect),
    lwd = 2, col = "black"
  )
  curve(0.5 * dnorm(x, 0, spike_sd),
        from = -limit, to = limit, add = TRUE,
        col = "red", lty = 2, lwd = 2)
  
  curve(0.5 * dnorm(x, 0, slab_sd),
        from = -limit, to = limit, add = TRUE,
        col = "blue", lty = 2, lwd = 2)
}

# function to get the index of the test region (spatial crossvalidation)
get_region_id <- function(region_name) {
  which(reg_names == region_name)
}

# setting to NA the response value for region to test
make_spatial_cv_data <- function(y_norm, space_index, region_id) {
  y_cv <- y_norm
  y_cv[space_index == region_id] <- NA
  list(Y = y_cv)
}

# extracting predictions of the regions from models trained on all regions but  
# the one of interest (spatial crossvalidation)
extract_region_predictions <- function(
    res_obj,
    region_name,
    space_index
) {
  # Find test indices for region
  test_idx <- which(space_index == get_region_id(region_name))
  if (length(test_idx) == 0) {
    stop(paste("No observations found for region:", region_name))
  }
  # Extract posterior predictive samples from both chains
  pred_ch1 <- res_obj$samples[[1]][, paste0("pred_y[", test_idx, "]"), drop = FALSE]
  pred_ch2 <- res_obj$samples[[2]][, paste0("pred_y[", test_idx, "]"), drop = FALSE]
  # Safety checks
  if (ncol(pred_ch1) != length(test_idx)) {
    stop("Mismatch between test indices and extracted columns (chain 1).")
  }
  if (ncol(pred_ch2) != length(test_idx)) {
    stop("Mismatch between test indices and extracted columns (chain 2).")
  }
  return(list(
    region = region_name,
    test_idx = test_idx,
    pred_ch1 = pred_ch1,
    pred_ch2 = pred_ch2
  ))
}

# Extracting true values for the region
extract_region_truth <- function(
    y,
    space_index,
    region_name
) {
  
  idx <- which(space_index == get_region_id(region_name))
  
  if (length(idx) == 0) {
    stop(paste("No observations found for region:", region_name))
  }
  
  true_vals <- y[idx]
  
  return(list(
    region = region_name,
    idx = idx,
    true = true_vals
  ))
}

# evaluating the prediction of the regions from models trained on all regions but  
# the one of interest (spatial crossvalidation)
evaluate_bayes_predictions_region <- function(
    pred_samples,
    mcmc_samples,
    X,
    test_indices,
    y_true,
    y_min,
    y_max,
    cols_idx_betas,
    cols_idx_space,
    cols_idx_time,
    inv_logit,
    years = 2010:2022,
    region_name
) {
  
  S <- nrow(pred_samples)
  N <- ncol(pred_samples)
  sex_test = sex[test_indices]
  
  # Posterior predictive summaries
  pred_mean <- colMeans(pred_samples)
  pred_ci_lower <- apply(pred_samples, 2, quantile, probs = 0.05)
  pred_ci_upper <- apply(pred_samples, 2, quantile, probs = 0.95)
  
  # Denormalize
  pred_mean_denorm <- pred_mean * (y_max - y_min) + y_min
  pred_ci_lower_denorm <- pred_ci_lower * (y_max - y_min) + y_min
  pred_ci_upper_denorm <- pred_ci_upper * (y_max - y_min) + y_min
  
  true_region <- y_true[test_indices]
  
  # Metrics
  rmse <- sqrt(mean((pred_mean_denorm - true_region)^2))
  rmse_f <- sqrt(mean((pred_mean_denorm[1:13] - true_region[1:13])^2))
  rmse_m <- sqrt(mean((pred_mean_denorm[14:26] - true_region[14:26])^2))
  mae <- mean(abs(pred_mean_denorm - true_region))
  
  # Bayesian p-values
  y_scaled <- (true_region - y_min) / (y_max - y_min)
  bayes_pvals <- sapply(1:N, function(i) {
    mean(pred_samples[, i] > y_scaled[i])
  })
  mean_bayes_p_value <- mean(bayes_pvals)
  
  # Log predictive density
  log_densities <- matrix(NA, nrow = S, ncol = N)
  
  X_test <- t(X[test_indices, ])  # 32 x 26
  
  for (s in 1:S) {
    
    if ("gender_rand_eff[1]" %in% colnames(mcmc_samples)) {
      gender_eff <- mcmc_samples[s, grep("^gender_rand_eff\\[", colnames(mcmc_samples))]
      gender_component <- gender_eff[sex_test]
      } else {
        beta_sex <- mcmc_samples[s, "beta_sex"]
        gender_component <- beta_sex * (sex_test - 1)
    }
    
    lin_pred <- 
      mcmc_samples[s, "beta0"] +
      gender_component +
      mcmc_samples[s, cols_idx_betas] %*% X_test +
      mcmc_samples[s, cols_idx_space] +
      rep(mcmc_samples[s, cols_idx_time], 2)
    
    mu <- inv_logit(lin_pred)
    
    betaphi <- mcmc_samples[s, "betaphi"]
    
    alpha_param <- mu * betaphi
    beta_param  <- (1 - mu) * betaphi
    
    y_scaled_s <- y_scaled
    y_scaled_s[y_scaled_s <= 0] <- 0.001
    y_scaled_s[y_scaled_s >= 1] <- 0.999
    
    log_densities[s, ] <- dbeta(
      y_scaled_s,
      shape1 = alpha_param,
      shape2 = beta_param,
      log = TRUE
    )
  }
  
  # Log predictive density
  lpd_i <- log(colMeans(exp(log_densities)))  # log of mean predictive density per obs
  total_lpd <- sum(lpd_i)
  
  # Plot
  df_plot <- tibble(
    Year = rep(years, 2),
    Gender = c(rep("Female", 13), rep("Male", 13)),
    True = true_region,
    Predicted = pred_mean_denorm,
    lower = pred_ci_lower_denorm,
    upper = pred_ci_upper_denorm
  )
  
  plot_obj <- ggplot(df_plot, aes(x = True, y = Predicted, color = Gender)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #ggrepel::geom_text_repel(aes(label = Year), size = 3, show.legend = FALSE) +
    ggrepel::geom_text_repel(aes(label = Year), size = 3, max.overlaps = Inf,# Show all labels
      # box.padding = 0.4,        # Adjust spacing around text
      # point.padding = 0.3,
      # segment.size = 0.3,       # Line connecting text to point
      show.legend = FALSE
    )  +
    facet_wrap(~Gender) +
    labs(
      title = paste("Predicted vs Observed", region_name),
      # subtitle = paste0(
      #   "RMSE: ", round(rmse, 4),
      #   " | Female: ", round(rmse_f, 4),
      #   " | Male: ", round(rmse_m, 4)
      # ),
      x = "Observed",
      y = "Predicted"
    ) +
    theme_minimal()
  
  # Return everything
  return(list(
    rmse = rmse,
    rmse_female = rmse_f,
    rmse_male = rmse_m,
    mae = mae,
    bayesian_p_values = bayes_pvals,
    bayesian_p_value = mean_bayes_p_value,
    pred_mean = pred_mean_denorm,
    credible_intervals = list(
      lower = pred_ci_lower_denorm,
      upper = pred_ci_upper_denorm
    ),
    total_log_pred_density = total_lpd,
    plot = plot_obj
  ))
}


# Gtting bayesian p-values and plot
bayesian_pvalue_diagnostics <- function(
    pred_samples_denorm,
    true_values,
    years = 2010:2022,
    region_name,
    gender_labels = c("Female", "Male"),
    alpha_low = 0.05,
    alpha_high = 0.95
) {
  
  if (ncol(pred_samples_denorm) != length(true_values)) {
    stop("Number of prediction columns must match length of true values.")
  }
  
  S <- nrow(pred_samples_denorm)
  N <- ncol(pred_samples_denorm)
  n_years <- length(years)
  
  if (N != n_years * length(gender_labels)) {
    stop("N must equal years × genders (e.g. 13 × 2).")
  }
  
  # Bayesian p-values
  bayes_pvalues <- colMeans(
    pred_samples_denorm >
      matrix(true_values, nrow = S, ncol = N, byrow = TRUE)
  )
  
  mean_pvalue <- mean(bayes_pvalues)
  
  # Data for ggplot barplot
  df_plot <- data.frame(
    Year   = rep(years, times = length(gender_labels)),
    Gender = rep(gender_labels, each = n_years),
    pvalue = bayes_pvalues
  )
  
  #df_plot$flag <- df_plot$pvalue < alpha_low | df_plot$pvalue > alpha_high
  
  p <- ggplot(df_plot, aes(x = interaction(Gender, Year, sep = " – "),
                           y = pvalue,
                           fill = factor(Gender))) +
    geom_col(width = 0.8) +
    #scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "red")) +
    geom_hline(yintercept = c(alpha_low, alpha_high),
               linetype = "dashed",
               color = "blue") +
    geom_hline(yintercept = 0.5,
               linetype = "dashed",
               color = "red") +
    # geom_vline(xintercept = n_years + 0.5,
    #            linetype = "solid",
    #            color = "black",
    #            linewidth = 0.8) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = paste("Bayesian p-values per Year–Gender Unit"),
      subtitle = paste(region_name,
                       "– Mean p-value:",
                       round(mean_pvalue, 3)),
      x = "",
      y = "Bayesian p-value",
      fill = paste0("Outside [", alpha_low, ", ", alpha_high, "]")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.x = element_blank(),
      legend.position = "top"
    )
  
  print(p)
  
  # Return object
  return(list(
    bayes_pvalues = bayes_pvalues,
    mean_bayes_pvalue = mean_pvalue,
    plot = p
  ))
}

# creating dataframe to plot then predicted density vs observed value
create_long_predictions <- function(
    pred_matrix,
    true_values,
    years = 2010:2022,
    gender_labels = c("Female", "Male"),
    region_name
) {
  
  N_years <- length(years)
  N_draws <- nrow(pred_matrix)
  
  if (ncol(pred_matrix) != 2 * N_years) {
    stop("Number of columns must be 2 × number of years (Female + Male).")
  }
  
  df_long <- tibble(
    draw = rep(seq_len(N_draws), times = 2 * N_years),
    Year = rep(rep(years, each = N_draws), times = 2),
    Gender = rep(gender_labels, each = N_years * N_draws),
    prediction = as.vector(pred_matrix),
    Region = region_name
  )
  
  df_true <- tibble(
    Year = rep(years, times = 2),
    Gender = rep(gender_labels, each = N_years),
    true_value = true_values,
    Region = region_name
  )
  
  return(list(
    predictions = df_long,
    truth = df_true
  ))
}

# plotting predicted density vs observed value
plot_ppc_density <- function(
    long_predictions,
    truth_data,
    region_name,
    trim_quantiles = c(0.01, 0.99),
    spacing
) {
  
  # x_limits <- quantile(
  #   long_predictions$prediction,
  #   probs = trim_quantiles,
  #   na.rm = TRUE
  # )
  
  ggplot(long_predictions, aes(x = prediction)) +
    geom_density(fill = "steelblue", alpha = 0.5, linewidth = 0.4) +
    geom_vline(
      data = truth_data,
      aes(xintercept = true_value),
      color = "red",
      linewidth = 0.8
    ) +
    facet_grid(Gender ~ Year) +
    #coord_cartesian(xlim = x_limits) +
    # scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = seq(
      round(min(long_predictions$prediction),2),
      round(max(long_predictions$prediction),2),
      spacing)) +
    labs(
      title = paste(
        "Posterior Predictive Densities by Year and Gender –", region_name
      ),
      #subtitle = "Red line = observed value",
      x = "Predicted value",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      strip.text.x = element_text(angle = 90),
      panel.spacing = unit(0.6, "lines")
    )
}

plot_chain_comparison <- function(
    ch_df_list1,
    ch_df_list2,
    param,
    colors = c("black", "red", "blue")
) {
  
  nchains = length(ch_df_list1)
  
  par(mfrow = c(2, 2))
  
  ## Trace plots: first list
  n1 <- nrow(ch_df_list1[[1]])
  plot(
    1:n1, ch_df_list1[[1]][[param]],
    type = "l",
    col = colors[1],
    xlab = "Iteration",
    ylab = bquote(.(param)),
    main = "First list of chains"
  )
  for (i in 2:nchains) {
    lines(1:nrow(ch_df_list1[[i]]),
          ch_df_list1[[i]][[param]],
          col = colors[i])
  }
  
  ##  Trace plots: second list
  n2 <- nrow(ch_df_list2[[1]])
  plot(
    1:n2, ch_df_list2[[1]][[param]],
    type = "l",
    col = colors[1],
    xlab = "Iteration",
    ylab = bquote(.(param)),
    main = "Second list of chains"
  )
  for (i in 2:nchains) {
    lines(1:nrow(ch_df_list2[[i]]),
          ch_df_list2[[i]][[param]],
          col = colors[i])
  }
  
  ##  ACF: first list
  acf1 <- acf(ch_df_list1[[1]][[param]], plot = FALSE)
  
  plot(
    acf1$lag,
    acf1$acf,
    type = "l",
    col = colors[1],
    lwd = 2,
    xlab = "Lag",
    ylab = "ACF",
    main = "ACF: First list of chains",
    ylim = range(
      sapply(ch_df_list1, function(x)
        acf(x[[param]], plot = FALSE)$acf)
    )
  )
  
  for (i in 2:nchains) {
    acfi <- acf(ch_df_list1[[i]][[param]], plot = FALSE)
    lines(acfi$lag, acfi$acf, col = colors[i], lwd = 2)
  }
  
  ##  ACF: second list
  acf1 <- acf(ch_df_list2[[1]][[param]], plot = FALSE)
  
  plot(
    acf1$lag,
    acf1$acf,
    type = "l",
    col = colors[1],
    lwd = 2,
    xlab = "Lag",
    ylab = "ACF",
    main = "ACF: First list of chains",
    ylim = range(
      sapply(ch_df_list2, function(x)
        acf(x[[param]], plot = FALSE)$acf)
    )
  )
  
  for (i in 2:nchains) {
    acfi <- acf(ch_df_list2[[i]][[param]], plot = FALSE)
    lines(acfi$lag, acfi$acf, col = colors[i], lwd = 2)
  }
  
  par(mfrow = c(1, 1))
}

within_between_var <- function(chains, param) {
  
  m <- length(chains)
  n <- nrow(chains[[1]])
  
  if (n %% 2 != 0){
    print('Discarding first sample because odd number of samples for each chain')
    chains = lapply(chains, function(x) {
      x[2:nrow(x), ]
    })
  }
  
  results <- lapply(param, function(p) {
    
    ##  standard R-hat 
    mu_bar <- sapply(chains, function(ch) mean(ch[, p]))
    mu_bar_all <- mean(mu_bar)
    between_var_overN <- sum((mu_bar - mu_bar_all)^2) / (m - 1)
    s_bar <- sapply(chains, function(ch) var(ch[, p]))
    within_var <- mean(s_bar)
    overall_var <- ((n - 1) / n) * within_var + between_var_overN
    Rhat <- sqrt(overall_var / within_var)
    
    ## split R-hat 
    half <- n / 2
    split_chains <- lapply(chains, function(ch) {
      list(
        ch[1:half, p],
        ch[(half + 1):n, p]
      )
    })
    split_chains <- unlist(split_chains, recursive = FALSE)
    
    m_split <- length(split_chains)
    n_split <- length(split_chains[[1]])
    
    mu_bar_s <- sapply(split_chains, mean)
    mu_bar_all_s <- mean(mu_bar_s)
    
    B_overN_s <- sum((mu_bar_s - mu_bar_all_s)^2) / (m_split - 1)
    W_s <- mean(sapply(split_chains, var))
    
    overall_var_s <- ((n_split - 1) / n_split) * W_s + B_overN_s
    Rhat_split <- sqrt(overall_var_s / W_s)
    
    c(
      within_var = within_var,
      between_var_overN = between_var_overN,
      overall_var = overall_var,
      within_over_all = within_var / overall_var,
      Rhat = Rhat,
      Rhat_split = Rhat_split
    )
  })
  
  results <- do.call(rbind, results)
  rownames(results) <- param
  as.data.frame(results)
}

# loading and checking all the useful stuff
# weights matrix
W = readRDS('W')
# covariates and response
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

years = 2010:2022

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

# DATA & CONSTANTS ---------------------------------------
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





# Code for Reviewer 1 -------------------------------------------------

##  1.2 MODEL VALIDATION & DIAGNOSTICS --------------------------------
# comment/uncomment based on what you want

load('Paper_Model_rhobeta_sexfixed.Rdata') # chain 2 converged for sex and time 
# effects only after 3900 iterations more or less

# samples_list = readRDS('Paper_Model_rhobeta_sexfixed_moreiters_3chains.rds') 

# if loading the 200,000 observation list
# samples_list <- lapply(samples_list, function(x) {
#   x[(nrow(x) - 100000 + 1):nrow(x), ]
# })
# saveRDS(samples_list, file = "Paper_Model_rhobeta_sexfixed_moreiters_3chains_less_obs.rds")

# samples_list = readRDS('Paper_Model_rhobeta_sexfixed_moreiters_3chains_less_obs.rds') 

# samples_list <- lapply(samples_list, function(x) {
#   x[(nrow(x) - 50000 + 1):nrow(x), ]
# })

lapply(samples_list,dim)

mcmc_samples_chain1 <- samples_list[[1]] # for Paper_Model_rhobeta_sexfixed.Rdata
mcmc_samples_chain2 <- samples_list[[2]] # for Paper_Model_rhobeta_sexfixed.Rdata
#mcmc_samples_chain3 <- samples_list[[3]] # for model with 3 chains

mcmc_samples_chain2 <- mcmc_samples_chain2[3900:10000,] # for OLD main model

dim(mcmc_samples_chain1)
dim(mcmc_samples_chain2)
#dim(mcmc_samples_chain3)

mcmc_samples_all = rbind(mcmc_samples_chain1,mcmc_samples_chain2)
#mcmc_samples_all = rbind(mcmc_samples_chain1,mcmc_samples_chain2,mcmc_samples_chain3)

dim(mcmc_samples_all)

iters = 1:nrow(mcmc_samples_chain2)

# SEE Mod_eval_and_comp.R to load the four train model chains, to see the true2022
# and to perform some diagnostics. Do the same also for the spatial cross validation
# and for the main, complete model, see correlation in residual, all the diagnostics
# and other things of this last one. 

 
# chain3 <- as.mcmc(mcmc_samples_chain3)

chain1 <- as.mcmc(mcmc_samples_chain1[3900:10000,]) # for OLD main model
chain2 <- as.mcmc(mcmc_samples_chain2) # for OLD main model

# mcmc_list <- mcmc.list(chain1, chain2, chain3)
# mcmc_list <- mcmc.list(chain2, chain3)
mcmc_list <- mcmc.list(chain1, chain2)
# chains = list(chain1,chain2,chain3)
# chains = list(chain2,chain3)
chains = list(chain1,chain2)

lapply(mcmc_list,dim)
lapply(chains,dim)


# if want to discard more observations (this is old model) 
## START
# chain1_burn = as.mcmc(chain1[3000:nrow(chain1),])
# chain2_burn = as.mcmc(chain2[3000:nrow(chain2),])
# dim(chain1_burn)
# dim(chain2_burn)
# 
# mcmc_list <- mcmc.list(chain1_burn, chain2_burn)
# chains = list(chain1_burn,chain2_burn)
# 
# lapply(mcmc_list,dim)
# lapply(chains,dim)

## END

####

all_params <- colnames(chain1)


params_to_check <- all_params[!grepl("^pred_y\\[", all_params)]

blocks <- list(
  fixed_effect_coeffs = grep("^beta(\\[|$)", all_params, value = TRUE),
  
  intercepts = grep("^beta0$|^beta_sex$", all_params, value = TRUE),
  
  precision = grep("^betaphi", all_params, value = TRUE),
  
  spatial = grep("^space_rand_eff\\[", all_params, value = TRUE),
  
  temporal = grep("^time_rand_eff\\[", all_params, value = TRUE),
  
  precisions_randeff = grep("^tau_space$|^tau_time$", all_params, value = TRUE),
  
  gamma_ssvs = grep("^gamma\\[", all_params, value = TRUE),
  theta_ssvs = grep("^theta\\[", all_params, value = TRUE),
  variance_ssvs = grep("^sig2\\[", all_params, value = TRUE),
  
  rho = grep("^rho$", all_params, value = TRUE)
)

lapply(blocks, length)

### Convergence Metrics ----------------------------------------------
autocorr.plot(mcmc_list[,'beta0'])
autocorr.plot(mcmc_list[,'beta_sex'])
crosscorr.plot(mcmc_list[,blocks$intercepts])
crosscorr(mcmc_list[,blocks$intercepts])
autocorr.plot(mcmc_list[,'rho'])
autocorr.plot(mcmc_list[,'precision'])
crosscorr.plot(mcmc_list[,blocks$temporal])
crosscorr(mcmc_list[,blocks$temporal])
crosscorr.plot(mcmc_list[,blocks$spatial])
crosscorr(mcmc_list[,blocks$spatial])
crosscorr.plot(mcmc_list[,blocks$precisions_randeff])
crosscorr.plot(mcmc_list[,blocks$fixed_effect_coeffs])

densplot(mcmc_list[,'beta0'])
densplot(mcmc_list[,'beta_sex'])
densplot(mcmc_list[,'rho'])
densplot(mcmc_list[,'betaphi'])
densplot(mcmc_list[,'tau_time'])
densplot(mcmc_list[,'tau_space'])

par(mfrow = c(3, 3))
for (i in seq_len(n_reg)) {
  densplot(
    mcmc_list[, paste0("space_rand_eff[", i, "]")],
    main = paste0("space_rand_eff[", i, "]")
  )
  ## pause AFTER each full page or at the very end
  if (i %% 9 == 0 || i == n_reg) {
    cat("Press Enter to continue to the next page (or type 'quit' to exit): ")
    input <- readline()
    if (tolower(input) == "quit") break
    ## reset layout for next page
    par(mfrow = c(3, 3))
  }
}

for (i in seq_len(n_years)) {
  densplot(
    mcmc_list[, paste0("time_rand_eff[", i, "]")],
    main = paste0("time_rand_eff[", i, "]")
  )
  ## pause AFTER each full page or at the very end
  if (i %% 9 == 0 || i == n_reg) {
    cat("Press Enter to continue to the next page (or type 'quit' to exit): ")
    input <- readline()
    if (tolower(input) == "quit") break
    ## reset layout for next page
    par(mfrow = c(3, 3))
  }
}

par(mfrow = c(1, 1))

within_between_var(chains = chains, param = blocks$intercepts)
within_between_var(chains = chains, param = blocks$temporal)
within_between_var(chains = chains, param = blocks$spatial)
within_between_var(chains = chains, param = blocks$precision)

# if want to load already existing:
# ess_each_chain = lapply(blocks,function(params){
#   lapply(mcmc_list[,params], effectiveSize)
# })

ess <- lapply(blocks, function(params) {
  effectiveSize(mcmc_list[, params])
})

ssvs_params = c(blocks$fixed_effect_coeffs,blocks$gamma_ssvs,blocks$variance_ssvs,blocks$theta_ssvs)

names_blocks_for_diag = c()
for(nm in names(blocks)){
  if(any(blocks[[nm]] %in% ssvs_params)) next
  names_blocks_for_diag = c(names_blocks_for_diag, nm)
}
  
to_transform_in_log = c('rho', 'tau_space', 'tau_time')
gel_diagn = data.frame()
for (nm in names_blocks_for_diag) {
  cat("\n=== Block:", nm, "===\n")
  if(any(blocks[[nm]] %in% to_transform_in_log)){
    gel_diagn_ = cbind(
      gelman.diag(mcmc_list[, blocks[[nm]]], multivariate = F, autoburnin = F,
                  transform = T)[[1]],
      ess[[nm]]
    )
  }else{
    gel_diagn_ = cbind(
      gelman.diag(mcmc_list[, blocks[[nm]]], multivariate = F, autoburnin = F)[[1]],
      ess[[nm]]
    ) 
  }
  if(any(!row.names(gel_diagn_) %in% blocks[[nm]])) row.names(gel_diagn_) =  blocks[[nm]]
  gel_diagn_ = as.data.frame(gel_diagn_)
  gel_diagn_ = gel_diagn_ %>% rename(ESS=V3, Rhat = `Point est.`, Upper_Rhat = `Upper C.I.`)
  gel_diagn  = rbind(gel_diagn,gel_diagn_)
}

gel_diagn = gel_diagn %>% mutate(variable = row.names(gel_diagn)) %>% as_tibble()
gel_diagn %>% filter(variable %in% blocks$intercepts)
gel_diagn %>% filter(variable %in% blocks$rho)
gel_diagn %>% filter(variable %in% blocks$precision)
gel_diagn %>% filter(variable %in% blocks$precisions_randeff)
gel_diagn %>% filter(variable %in% blocks$temporal)
gel_diagn %>% filter(variable %in% blocks$spatial)

# saveRDS(gel_diagn, file = "gel_diagn_3chains_less_obs.rds") # 75000 obs
# saveRDS(gel_diagn, file = "gel_diagn_3chains_lessless_obs.rds") # 50000 obs
# saveRDS(gel_diagn, file = "gel_diagn_last2chains_lessless_obs.rds") # 50000 obs
# saveRDS(gel_diagn, file = "gel_diagn_old_main_model.rds") # 50000 obs
gel_diag = readRDS('')

### Vethari et al. 2021 convergence diagnostics ----------------
#chains_array <- as_draws_array(chains)
# class(chains_array)
# dim(chains_array)
chains_df <- as_draws_df(chains)
class(chains_df)
dim(chains_df)
colnames(chains_df)

ch1_df = chains_df %>% filter(.chain==1)
ch2_df = chains_df %>% filter(.chain==2)
ch3_df = chains_df %>% filter(.chain==3)

# thinnin
chains_thinned_df = thin_draws(chains_df, thin = 6)
dim(chains_thinned_df)

ch1_thin_df = chains_thinned_df %>% filter(.chain==1)
ch2_thin_df = chains_thinned_df %>% filter(.chain==2)
ch3_thin_df = chains_thinned_df %>% filter(.chain==3)


ch_df_list1 = list(ch1_df,ch2_df,ch3_df)
ch_df_list1 = list(ch1_df,ch2_df)
ch_df_list2 = list(ch1_thin_df,ch2_thin_df,ch3_thin_df)
ch_df_list2 = list(ch1_thin_df,ch2_thin_df)

# comparing visually thinned and not thinned chains (or more thinned than already thinned)
plot_chain_comparison(ch_df_list1, 
                      ch_df_list2,
                      param = 'beta0')

plot_chain_comparison(ch_df_list1, 
                      ch_df_list2,
                      param = 'beta_sex')

plot_chain_comparison(ch_df_list1, 
                      ch_df_list2,
                      param = 'tau_space')

plot_chain_comparison(ch_df_list1, 
                      ch_df_list2,
                      param = 'tau_time')

plot_chain_comparison(ch_df_list1, 
                      ch_df_list2,
                      param = 'betaphi')

plot_chain_comparison(ch_df_list1, 
                      ch_df_list2,
                      param = 'rho')

for (i in 1:n_reg) {
  plot_chain_comparison(ch_df_list1, 
                        ch_df_list2,
                        param = paste0('space_rand_eff[',i,']'))
  print(reg_names[i])
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}  

for (i in 1:n_years) {
  plot_chain_comparison(ch_df_list1, 
                        ch_df_list2,
                        param = paste0('time_rand_eff[',i,']'))
  print(years[i])
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}  


#chains_array_diagn = chains_array[,,!grepl("^pred_y\\[", colnames(chains[[1]]))]
chains_df_diagn = chains_df[,!grepl("rmse|^pred_y\\[", colnames(chains_df))]
colnames(chains_df_diagn)

chains_thinned_df_diagn = chains_thinned_df[,!grepl("rmse|^pred_y\\[", colnames(chains_df))]
colnames(chains_thinned_df_diagn)


# for methods see https://cran.r-project.org/web/packages/posterior/vignettes/posterior.html
# help("diagnostics", "posterior")

vehtari_diag <- summarise_draws(
  chains_df_diagn,
  mean,
  median,
  sd,
  ~quantile(., probs = c(0.05, 0.95)),
  rhat_basic,
  rhat,
  ess_basic,
  ess_bulk,
  ess_tail
)
vehtari_diag <- vehtari_diag %>%
  rename(
    q5  = `5%`,
    q95 = `95%`
  )
vehtari_diag = vehtari_diag %>% mutate(mean = round(mean, 2), 
                                       median = round(median,2), 
                                       q5 = round(q5,2), 
                                       sd = round(sd,2),
                                       q95 = round(q95,2),
                                       ess_over_samples = ess_basic/nrow(chains_df_diagn),
                                       essbulk_over_samples = ess_bulk/nrow(chains_df_diagn))
# saveRDS(vehtari_diag, file = "vehtari_diag_3chains_less_obs.rds")   # 75000 Not saved
# saveRDS(vehtari_diag, file = "vehtari_diag_3chains_lessless_obs.rds") # 50000
# saveRDS(vehtari_diag, file = "vehtari_diag_last2chains_lessless_obs.rds") # 50000
# saveRDS(vehtari_diag, file = "vehtari_diag_old_main_model.rds") 
vehtari_diag = readRDS('')

vehtari_thinned_diag <- summarise_draws(
  chains_thinned_df_diagn,
  mean,
  median,
  sd,
  ~quantile(., probs = c(0.05, 0.95)),
  rhat_basic,
  rhat,
  ess_basic,
  ess_bulk,
  ess_tail
)
vehtari_thinned_diag <- vehtari_thinned_diag %>%
  rename(
    q5  = `5%`,
    q95 = `95%`
  )
vehtari_thinned_diag = vehtari_thinned_diag %>% mutate(mean = round(mean, 4), 
                                       median = round(median,4), 
                                       q5 = round(q5,4), 
                                       sd = round(sd,4),
                                       q95 = round(q95,4),
                                       ess_over_samples = ess_basic/nrow(chains_thinned_df_diagn),
                                       essbulk_over_samples = ess_bulk/nrow(chains_thinned_df_diagn))

vehtari_diag %>% filter(variable %in% blocks$intercepts)
vehtari_thinned_diag %>% filter(variable %in% blocks$intercepts)
gel_diagn %>% filter(variable %in% blocks$intercepts)

vehtari_diag %>% filter(variable %in% blocks$precisions_randeff)
vehtari_thinned_diag %>% filter(variable %in% blocks$precisions_randeff)
gel_diagn %>% filter(variable %in% blocks$precisions_randeff)

vehtari_diag %>% filter(variable %in% blocks$rho)
vehtari_thinned_diag %>% filter(variable %in% blocks$rho)
gel_diagn %>% filter(variable %in% blocks$rho)

vehtari_diag %>% filter(variable %in% blocks$precision)
vehtari_thinned_diag %>% filter(variable %in% blocks$precision)
gel_diagn %>% filter(variable %in% blocks$precision)

vehtari_diag %>% filter(variable %in% blocks$spatial)
vehtari_thinned_diag %>% filter(variable %in% blocks$spatial)
gel_diagn %>% filter(variable %in% blocks$spatial)

vehtari_diag %>% filter(variable %in% blocks$temporal)
vehtari_thinned_diag %>% filter(variable %in% blocks$temporal)
gel_diagn %>% filter(variable %in% blocks$temporal)

vehtari_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$intercepts)
vehtari_thinned_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$intercepts)

vehtari_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$precision)
vehtari_thinned_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$precision)

vehtari_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$precisions_randeff)
vehtari_thinned_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$precisions_randeff)

vehtari_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$rho)
vehtari_thinned_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$rho)

vehtari_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$temporal)
vehtari_thinned_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$temporal)

vehtari_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$spatial)
vehtari_thinned_diag %>% inner_join(gel_diagn, by='variable') %>% filter(variable %in% blocks$spatial)

extract_block_diag <- function(
    vehtari_diag,
    vehtari_thinned_diag,
    gel_diagn,
    blocks,
    block_name,
    also_thinned = F
) {
  
  vars <- blocks[[block_name]]
  
  if(also_thinned){
    out = list(
      full = vehtari_diag %>%
        inner_join(gel_diagn, by = "variable") %>%
        filter(variable %in% vars),
      
      thinned = vehtari_thinned_diag %>%
        inner_join(gel_diagn, by = "variable") %>%
        filter(variable %in% vars)
    )
  }else{
    out = vehtari_diag %>%
      inner_join(gel_diagn, by = "variable") %>%
      filter(variable %in% vars)
  }
  return(out)
}

intercepts_diag <- extract_block_diag(vehtari_diag, #vehtari_thinned_diag,
                                      gel_diagn = gel_diagn,
                                      blocks = blocks,  block_name = "intercepts")

precision_diag <- extract_block_diag(vehtari_diag, #vehtari_thinned_diag,
                                     gel_diagn = gel_diagn,
                                     blocks = blocks,  block_name = 'precision')

rho_diag <- extract_block_diag(vehtari_diag, #vehtari_thinned_diag,
                                     gel_diagn = gel_diagn,
                                     blocks = blocks,  block_name = 'rho')

precision_randeff_diag <- extract_block_diag(vehtari_diag, #vehtari_thinned_diag,
                                     gel_diagn = gel_diagn,
                                     blocks = blocks,  block_name = 'precisions_randeff')

spatial_diag <- extract_block_diag(vehtari_diag, #vehtari_thinned_diag,
                                     gel_diagn = gel_diagn,
                                     blocks = blocks,  block_name = 'spatial')

temporal_diag <- extract_block_diag(vehtari_diag, #vehtari_thinned_diag,
                                     gel_diagn = gel_diagn,
                                     blocks = blocks,  block_name = 'temporal')

# output for table in paper representing ESS and Rhat according 
# to vehtari and gelman
intercepts_diag %>% select(c(variable, #mean, median, sd, q5, q95,
                             #rhat_basic, 
                             ess_basic, ess_bulk, ess_tail,
                             rhat,
                             ESS,
                             Rhat, Upper_Rhat))
precision_diag  %>% select(c(variable, #mean, median, sd, q5, q95,
                             #rhat_basic, 
                             ess_basic, ess_bulk, ess_tail,
                             rhat,
                             ESS,
                             Rhat, Upper_Rhat))
rho_diag  %>% select(c(variable, #mean, median, sd, q5, q95,
                       #rhat_basic, 
                       ess_basic, ess_bulk, ess_tail,
                       rhat,
                       ESS,
                       Rhat, Upper_Rhat))
precision_randeff_diag  %>% select(c(variable, #mean, median, sd, q5, q95,
                                     #rhat_basic, 
                                     ess_basic, ess_bulk, ess_tail,
                                     rhat,
                                     ESS,
                                     Rhat, Upper_Rhat))
temporal_diag  %>% select(c(variable, #mean, median, sd, q5, q95,
                            #rhat_basic, 
                            ess_basic, ess_bulk, ess_tail,
                            rhat,
                            ESS,
                            Rhat, Upper_Rhat))
spatial_diag %>% select(c(variable, #mean, median, sd, q5, q95,
                          #rhat_basic, 
                          ess_basic, ess_bulk, ess_tail,
                          rhat,
                          ESS,
                          Rhat, Upper_Rhat))


gelman.plot(mcmc_list[,blocks$intercepts], bin.width = round(nrow(chain1)/20))
gelman.plot(mcmc_list[,blocks$precisions_randeff], bin.width = round(nrow(chain1)/20))
gelman.plot(mcmc_list[,blocks$precision], bin.width = round(nrow(chain1)/20))
gelman.plot(mcmc_list[,blocks$rho], bin.width = round(nrow(chain1)/20))
gelman.plot(mcmc_list[,blocks$temporal], bin.width = round(nrow(chain1)/20))
gelman.plot(mcmc_list[,blocks$spatial], bin.width = round(nrow(chain1)/20))


### Residual Correlation ---------------------------------------------

cols_idx_pred <- which(grepl("pred_y\\[",colnames(mcmc_samples_all)))

# df of predictions from chain
pred_df = de_min_max_normalize(mcmc_samples_all[,cols_idx_pred],
                     x_max = y_max, x_min = y_min)
dim(pred_df)

pred_df_t = t(pred_df) %>% as.data.frame()

head(pred_df_t[,1:10])

pred_df_t.tb = as_tibble(bind_cols(X_til[,c(41,42,43)], pred_df_t))


# true_y_df = tibble(Observed = y_til, 
#                    sex = pred_df_t.tb$sex,
#                    space = pred_df_t.tb$space,
#                    time = pred_df_t.tb$time)

pred_df_t.tb$Observed = y_til

pred_df_t.tb = pred_df_t.tb %>%
  mutate(mean_predicted = rowMeans(across(starts_with("V"))),
         resid_mean = Observed - mean_predicted)

pred_df_t.tb <- pred_df_t.tb %>%
  select(Observed, mean_predicted, resid_mean, everything())

pred_df_t.tb <- pred_df_t.tb %>%
  mutate(space = factor(space, levels = 1:20, labels = reg_names),
         time = factor(time, levels = 1:13, labels = 2010:2022),
         sex = factor(sex, levels = 0:1, labels = c("female", "male")))

# Visual Analysis
# Residual Patterns
pred_df_t.tb %>%
  ggplot(aes(mean_predicted, resid_mean)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    x = "Mean predicted",
    y = "Residual"#,
    #title = "Residuals vs Mean Predicted"
  ) +
  theme_minimal()

# Histogram/Density Overall
pred_df_t.tb %>%
  ggplot(aes(resid_mean)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "lightblue",
    color = "black"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Mean Residuals",
    y = "Density"
  ) +
  theme_minimal()

# Histogram/Density by Gender, Space, Time
pred_df_t.tb %>%
  ggplot(aes(resid_mean)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "lightblue",
    color = "black"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ sex) +
  labs(
    x = "Mean Residuals",
    y = "Density"
  ) +
  theme_minimal()

pred_df_t.tb %>%
  ggplot(aes(resid_mean)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 13,
    fill = "lightblue",
    color = "black"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ space) +
  labs(
    x = "Mean Residuals",
    y = "Density"
  ) +
  theme_minimal()

pred_df_t.tb %>%
  ggplot(aes(resid_mean)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "lightblue",
    color = "black"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ time) +
  labs(
    x = "Mean Residuals",
    y = "Density"
  ) +
  theme_minimal()

# Boxplots# Boxtimeplots
pred_df_t.tb %>%
  ggplot(aes(sex, resid_mean)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Gender",
    y = "Residual"
  ) +
  theme_minimal()

pred_df_t.tb %>%
  ggplot(aes(space, resid_mean)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Region",
    y = "Residual"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
pred_df_t.tb %>%
  ggplot(aes(time, resid_mean)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Residual"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Residuals over time and space
pred_df_t.tb %>%
  ggplot(aes(time, resid_mean, group = space, color = factor(space))) +
  geom_line(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Time",
    y = "Residual",
    title = "Residuals Over Time by Space"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

pred_df_t.tb %>%
  ggplot(aes(time, resid_mean)) +
  geom_line(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ space, ncol = 5) +
  labs(
    x = "Time",
    y = "Residual",
    title = "Residuals Over Time by Space"
  ) +
  theme_minimal()


# heatmap space time
pred_df_t.tb %>%
  group_by(space, time) %>%
  summarise(mean_resid = mean(resid_mean), .groups = "drop") %>%
  ggplot(aes(time, factor(space), fill = mean_resid)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0) +
  labs(
    x = "Year",
    y = "Region",
    fill = "Residual"#,
   # title = "Residual Heatmap (Space × Time)"
  ) +
  theme_minimal()


# ACF
# resid_draws_long <- pred_df_t.tb %>%
#   select(space, time, Observed, starts_with("V")) %>%
#   pivot_longer(
#     cols = starts_with("V"),
#     names_to = "draw",
#     values_to = "pred"
#   ) %>%
#   mutate(resid = Observed - pred)
# 
# bayes_acf <- resid_draws_long %>%
#   arrange(space, draw, time) %>%
#   group_by(space, draw) %>%
#   summarise(
#     acf_obj = list(acf(resid, plot = FALSE, na.action = na.pass)),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     acf_df = map(acf_obj, ~ tibble(
#       lag = as.numeric(.x$lag),
#       acf = as.numeric(.x$acf)
#     ))
#   ) %>%
#   select(-acf_obj) %>%
#   unnest(acf_df)
# 
# bayes_acf_summary <- bayes_acf %>%
#   group_by(space, lag) %>%
#   summarise(
#     acf_mean = mean(acf),
#     acf_med = median(acf),
#     acf_lo = quantile(acf, 0.025),
#     acf_hi = quantile(acf, 0.975),
#     .groups = "drop"
#   )

# save(bayes_acf_summary,
#      file =
#        "C://Users//lucia//Desktop//STBetaBayes_Obesity_Italy//data_and_models//bayes_acf_summary.RData")
load("bayes_acf_summary.RData")
bayes_acf_summary %>% print(n=50)

bayes_acf_summary = bayes_acf_summary %>%
  mutate(space = factor(space, levels = 1:20, labels = reg_names))

bayes_acf_summary %>%
  ggplot(aes(lag, acf_mean)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_ribbon(aes(ymin = acf_lo, ymax = acf_hi), alpha = 0.3) +
  geom_line() +
  facet_wrap(~ space) +
  labs(
    #title = "Bayesian Residual Autocorrelation (95% Credible Bands)",
    y = "ACF"
  ) +
  theme_minimal()

bayes_acf_summary %>%
  ggplot(aes(x = lag, y = acf_mean)) +
  geom_hline(yintercept = 0, color = "grey50") +
  
  # 95% credible interval across posterior draws
  geom_ribbon(
    aes(ymin = acf_lo, ymax = acf_hi),
    alpha = 0.3
  ) +
  
  # ACF bars (spikes)
  geom_col(
    width = 0.6,
    fill = "steelblue",
    alpha = 0.8
  ) +
  
  # Point at the mean ACF
  geom_point(
    size = 1.5,
    color = "black"
  ) +
  
  facet_wrap(~ space) +
  labs(
    y = "ACF",
    x = "Lag"
  ) +
  theme_minimal()


bayes_acf_summary %>%
  ggplot(aes(x = lag, y = acf_mean)) +
  geom_hline(yintercept = 0, color = "grey50") +
  
  geom_ribbon(
    aes(ymin = acf_lo, ymax = acf_hi),
    alpha = 0.3
  ) +
  
  geom_segment(
    aes(xend = lag, y = 0, yend = acf_mean),
    linewidth = 0.6,
    color = "steelblue"
  ) +
  
  geom_point(size = 1.5) +
  
  facet_wrap(~ space) +
  labs(y = "ACF", x = "Lag") +
  theme_minimal()


# # Ljung-Box
# lb_test <- pred_df_t.tb %>%
#   group_by(space) %>%
#   summarise(
#     lb_pvalue = Box.test(resid_mean, lag = 10, type = "Ljung-Box")$p.value,
#     .groups = "drop"
#   )
# lb_test %>%
#   ggplot(aes(factor(space), lb_pvalue)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
#   labs(
#     x = "Space",
#     y = "Ljung–Box p-value",
#     title = "Residual Temporal Independence Test"
#   ) +
#   theme_minimal()

### Posterior Predictive Check (PPC) ------------------------------------
ppc_df <- pred_df_t.tb %>%
  mutate(
    pred_q025 = apply(across(starts_with("V")), 1, quantile, probs = 0.025),
    pred_q50 = apply(across(starts_with("V")), 1, quantile, probs = 0.50),
    pred_mean = apply(across(starts_with("V")), 1, mean),
    pred_q975 = apply(across(starts_with("V")), 1, quantile, probs = 0.975)
  )

X11()
ppc_df %>%
  mutate(outside = Observed < pred_q025 | Observed > pred_q975) %>%
  ggplot(aes(pred_mean, Observed)) +
  geom_errorbar(
    aes(ymin = pred_q025, ymax = pred_q975),
    alpha = 0.25
  ) +
  geom_point(
    aes(color = outside),
    alpha = 0.6,
    size = 2
  ) +
  scale_color_manual(
    values = c("FALSE" = "black", "TRUE" = "red"),
    labels = c("Inside 95% PI", "Outside 95% PI"),
    name = NULL
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Posterior mean prediction",
    y = "Observed"
  ) +
  theme_minimal()


ppc_df %>%
  mutate(covered = Observed >= pred_q025 & Observed <= pred_q975) %>%
  summarise(coverage = mean(covered))

(ppc_df %>% filter(Observed >= pred_q975 | Observed <= pred_q025))[,1:6] 
(ppc_df %>% filter(Observed >= pred_q975 | Observed <= pred_q025))[,1:6] %>% count(space) %>% arrange(n)
(ppc_df %>% filter(Observed >= pred_q975 | Observed <= pred_q025))[,1:6] %>% count(time) %>% arrange(n)


#### PPC: Credible Bands analysis --------------------
ppc_df %>%
  mutate(width = pred_q975 - pred_q025) %>%
  ggplot(aes(width)) +
  geom_histogram(bins = 30, fill = "grey70", col = 'black') +
  labs(
    title = "Distribution of 95% Predictive Interval Widths",
    x = "Interval width"
  ) +
  theme_minimal()

ppc_df %>%
  mutate(width = pred_q975 - pred_q025) %>%
  ggplot(aes(factor(space), width)) +
  geom_boxplot() +
  labs(
    #title = "Predictive Uncertainty by Region",
    x = "Region",
    y = "95% Interval Width"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ppc_df %>%
  mutate(width = pred_q975 - pred_q025) %>%
  ggplot(aes(factor(time), width)) +
  geom_boxplot() +
  labs(
    #title = "Predictive Uncertainty by Year",
    x = "Year",
    y = "95% Interval Width"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ppc_df %>%
  mutate(width = pred_q975 - pred_q025) %>%
  ggplot(aes(factor(sex), width)) +
  geom_boxplot() +
  labs(
    x = "Gender",
    y = "95% Interval Width"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#### PPC: space and time rmse and coverage -------------------
# # space
space_perf <- ppc_df %>%
  group_by(space) %>%
  summarise(
    RMSE = sqrt(mean((Observed - pred_q50)^2)),
    MAE  = mean(abs(Observed - pred_q50)),
    coverage_95 = mean(Observed >= pred_q025 & Observed <= pred_q975),
    .groups = "drop"
  )
space_perf %>%
  ggplot(aes(factor(space), RMSE)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Space",
    y = "RMSE",
    title = "Predictive RMSE by Space"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

space_perf %>%
  ggplot(aes(factor(space), coverage_95)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  ylim(0.8, 1) +
  labs(
    x = "Space",
    y = "95% Coverage",
    title = "Predictive Coverage by Region"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# # time 
time_perf <- ppc_df %>%
  group_by(time) %>%
  summarise(
    RMSE = sqrt(mean((Observed - pred_q50)^2)),
    MAE  = mean(abs(Observed - pred_q50)),
    coverage_95 = mean(Observed >= pred_q025 & Observed <= pred_q975),
    .groups = "drop"
  )
time_perf %>%
  ggplot(aes(factor(time), RMSE)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Time",
    y = "RMSE",
    title = "Predictive RMSE by Time"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

time_perf %>%
  ggplot(aes(factor(time), coverage_95)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  ylim(0.8, 1) +
  labs(
    x = "Space",
    y = "95% Coverage",
    title = "Predictive Coverage by Year"
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# # space x time 
space_time_perf <- ppc_df %>%
  group_by(space, time) %>%
  summarise(
    RMSE = sqrt(mean((Observed - pred_q50)^2)),
    coverage_95 = mean(Observed >= pred_q025 & Observed <= pred_q975),
    .groups = "drop"
  )
space_time_perf %>%
  ggplot(aes(time, space, fill = RMSE)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    x = "Year",
    y = "Region",
    fill = "RMSE",
    title = "Predictive RMSE by Region and Year"
  ) +
  theme_minimal()

space_time_perf$coverage_95 %>% table()

space_time_perf %>%
  ggplot(aes(time, space, fill = coverage_95)) +
  geom_tile() +
  scale_fill_gradient2(
    midpoint = 0.95,
    low = "firebrick",
    mid = "white",
    high = "steelblue"
  ) +
  labs(
    x = "Year",
    y = "Region",
    fill = "95% Coverage",
    title = "Predictive Coverage by Region and Year"
  ) +
  theme_minimal()


### SSVS prior shrinkage (see 1.3) -----------------------------------


##  1.3: SSVS ---------------------------------------------------------
### defining different intersections ----------------------------------
c_ss <- 4000
cc <- c_ss^2
intersections <- c(0.0001, 0.001, 0.01, 0.1, 1)
# Plot layout
# Extra bottom space for the legend
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(3, 0, 0, 0))
# Plot each panel
for (intersect in intersections) {
  plot_ssvs(intersect, c_ss, limit = 0.4)
}
# Empty 6th plot
plot.new()
# Single Legend (bottom center) 
par(xpd = NA)  # allow drawing outside plot area
legend("bottom",
       inset = c(-0.02, -0.42),   # shift right slightly & move further down
       legend = c("Mixture", "Spike", "Slab"),
       col = c("black", "red", "blue"),
       lwd = 2, lty = c(1, 2, 2),
       horiz = FALSE,  # vertical layout
       bty = "n",
       cex = 1.3)
# Focus on lower intersections
par(mfrow = c(2, 2))
for (intersect in intersections[1:4]) {
  plot_ssvs(intersect, c_ss, limit = 0.1)
}

par(mfrow = c(1,1)) 
# # SSVS used in the final specification 
# c_ss<-4000 
# cc = c_ss^2
# intersect<-0.001
# exp(0.001)-1 # % increase/decrease in the odds
# tau_ss<-intersect/sqrt(2*log(c_ss)*c_ss^2/(c_ss^2-1))
# tau_ss^2 
# tau_ss
# tau2 <- tau_ss^2
# (tau2 * cc) %>% sqrt()

taus <- sapply(intersections, compute_tau, c_ss)
tau2s = taus^2
# sd and var of different spikes
print('SDs of spikes');taus %>% round(10)
print('VARs of spikes');tau2s %>% round(10)
# sd and var of different slabs
print('SDs of slabs');(tau2s * cc) %>% sqrt()
print('SDs of slabs');(tau2s * cc) %>% round(5)

### initialization ---------------------------------------------------
# loading chain to choose smart initialization values for a short ssvs check
load('Paper_Model_rhobeta_sexfixed.Rdata') 
mcmc_samples_chain1 <- samples_list[[1]] # for Paper_Model_rhobeta_sexfixed.Rdata
mcmc_samples_chain2 <- samples_list[[2]] # for Paper_Model_rhobeta_sexfixed.Rdata
iters = 1:nrow(mcmc_samples_chain2)

beta0_post_init = mean(mean(mcmc_samples_chain1[,'beta0']),
                    mean(mcmc_samples_chain2[3900:10000,'beta0']))
beta_sex_post_init = mean(mean(mcmc_samples_chain1[,'beta_sex']),
                    mean(mcmc_samples_chain2[3900:10000,'beta_sex']))


space_rand_eff_post_mean_binded = rbind(
  colMeans(mcmc_samples_chain1[,grep("^space_rand_eff\\[", colnames(mcmc_samples_chain1))]),
  colMeans(mcmc_samples_chain2[3900:10000,grep("^space_rand_eff\\[", colnames(mcmc_samples_chain1))])
  )
space_rand_eff_post_init = colMeans(space_rand_eff_post_mean_binded)

time_rand_eff_post_mean_binded = rbind(
  colMeans(mcmc_samples_chain1[,grep("^time_rand_eff\\[", colnames(mcmc_samples_chain1))]),
  colMeans(mcmc_samples_chain2[3900:10000,grep("^time_rand_eff\\[", colnames(mcmc_samples_chain1))])
)
time_rand_eff_post_init = colMeans(time_rand_eff_post_mean_binded)

tau_space_post_init = mean(mean(mcmc_samples_chain1[,'tau_space']),
                    mean(mcmc_samples_chain2[3900:10000,'tau_space']))
tau_time_post_init = mean(mean(mcmc_samples_chain1[,'tau_time']),
                    mean(mcmc_samples_chain2[3900:10000,'tau_time']))

rho_post_init = mean(mean(mcmc_samples_chain1[,'rho']),
                    mean(mcmc_samples_chain2[3900:10000,'rho']))

pred_y_post_mean_binded = rbind(
  colMeans(mcmc_samples_chain1[, grep("^pred_y\\[", colnames(mcmc_samples_chain1))]),
  colMeans(mcmc_samples_chain2[3900:10000, grep("^pred_y\\[", colnames(mcmc_samples_chain1))])
)
pred_y_post_init = colMeans(pred_y_post_mean_binded)

betaphi_pre_chain1 = ( mcmc_samples_chain1[,'betaphi']-min(mcmc_samples_chain1[,'betaphi']) ) /
  ( max(mcmc_samples_chain1[,'betaphi'])-min(mcmc_samples_chain1[,'betaphi']) )
betaphi_pre_chain2 = ( mcmc_samples_chain2[,'betaphi']-min(mcmc_samples_chain2[,'betaphi']) ) /
  ( max(mcmc_samples_chain2[,'betaphi'])-min(mcmc_samples_chain2[,'betaphi']) )
betaphi_pre_post_init = mean(mean(betaphi_pre_chain1),
                             mean(betaphi_pre_chain2[3900:10000]))


### defining code for model ---------------------------------------------
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

### Storage object for all tau2 results ----------------------------------
# tau2s = tau2s[1:4] # doing this because it is not necessary to use the last one, very big
# # # sd and var of different spikes
# print('SDs of spikes');taus %>% round(10)
# print('VARs of spikes');tau2s %>% round(10)
# # sd and var of different slabs
# print('SDs of slabs');(tau2s * cc) %>% sqrt()
# print('SDs of slabs');(tau2s * cc) %>% round(5)
# 
# all_samples_ssvs <- vector("list", length(tau2s))
# names(all_samples_ssvs) <- paste0("Inters=0", c('.0001','.001','.01','.1'))
# all_samples_ssvs
# 
# iter = 20000
# nchain = 2
# burn = 5000
# thin = 1
# 
# counter <- 1
# 
# for (tau2_in_tau2s in tau2s) {
# 
#   cat("\nRunning intersection =", intersections[counter], "\n")
# 
#   constants_different_ssvs <- list(
#     X = X,
#     N = length(y_til), p = p_temp,
#     n_reg = n_reg, n_years = n_years,
#     space_index = space_index, time_index = time_index,
#     sex = sex,
#     adj = adj, L = L, num = num,
#     a = 50, a_phi = 0.1, b_phi = 0.1,
#     tau2 = tau2_in_tau2s, cc = cc
#   )
# 
#   set.seed(37) # just to have reproducibility of betas
#   inits_list <- list(
#     list(
#       beta0 = beta0_post_init,
#       beta_sex = beta_sex_post_init,
#       beta = rnorm(p_temp),
#       space_rand_eff = space_rand_eff_post_init,
#       time_rand_eff = time_rand_eff_post_init,
#       tau_space = tau_space_post_init,
#       tau_time = tau_time_post_init,
#       rho = rho_post_init,
#       pred_y = pred_y_post_init,
#       betaphi_pre = betaphi_pre_post_init,
#       gamma = rep(1,p_temp),
#       theta = rep(1,p_temp)
#     ),
#     list(
#       beta0 = beta0_post_init,
#       beta_sex = beta_sex_post_init,
#       beta = rnorm(p_temp),
#       space_rand_eff = space_rand_eff_post_init,
#       time_rand_eff = time_rand_eff_post_init,
#       tau_space = tau_space_post_init,
#       tau_time = tau_time_post_init,
#       rho = rho_post_init,
#       pred_y = pred_y_post_init,
#       betaphi_pre = betaphi_pre_post_init,
#       gamma = rep(1,p_temp),
#       theta = rep(1,p_temp)
#     )
#   )
# 
#   samples_list_ssv <- vector("list", 2)
# 
#   for (chain in 1:2) {
# 
#     model <- nimbleModel(
#       code,
#       constants = constants_different_ssvs,
#       data = data,
#       inits = inits_list[[chain]]
#     )
# 
#     print(model$calculate())  # diagnostic
# 
#     cModel <- compileNimble(model)
#     conf <- configureMCMC(model, monitors = params_to_save)
#     MCMC <- buildMCMC(conf)
#     cMCMC <- compileNimble(MCMC, project = cModel)
# 
#     samples_list_ssv[[chain]] <- runMCMC(
#       cMCMC,
#       niter = iter,
#       nburnin = burn,
#       thin = thin
#     )
#   }
# 
#   # ---- Save both chains for this tau2 ----
#   all_samples_ssvs[[counter]] <- list(
#     tau2 = tau2_in_tau2s,
#     samples_chain1 = samples_list_ssv[[1]],
#     samples_chain2 = samples_list_ssv[[2]]
#   )
# 
#   counter <- counter + 1
# }
# 
# saveRDS(
#   all_samples_ssvs,
#   file = "SSVS_chains_all_tau2s.rds"
# )

all_samples_ssvs = readRDS('SSVS_chains_all_tau2s.rds')

# all_samples_ssvs$`Inters=0.0001`$samples_chain1
# all_samples_ssvs$`Inters=0.0001`$samples_chain2
# all_samples_ssvs$`Inters=0.001`$samples_chain1
# all_samples_ssvs$`Inters=0.001`$samples_chain2
# all_samples_ssvs$`Inters=0.01`$samples_chain1
# all_samples_ssvs$`Inters=0.01`$samples_chain2
# all_samples_ssvs$`Inters=0.1`$samples_chain1
# all_samples_ssvs$`Inters=0.1`$samples_chain2

### ssvs plot ------------------------------------------------------
# choose one of the Inters=0.1, Inters=0.01, Inters=0.001, Inters=0.0001
mcmc_samples_chain1 <- all_samples_ssvs$`Inters=0.1`$samples_chain1
mcmc_samples_chain2 <- all_samples_ssvs$`Inters=0.1`$samples_chain2

dim(mcmc_samples_chain1)
dim(mcmc_samples_chain2)

mcmc_samples_all = rbind(mcmc_samples_chain1,mcmc_samples_chain2)

cols_idx_gamma<- which(grepl("gamma",colnames(mcmc_samples_all)))
post_gamma <-as.matrix(mcmc_samples_all[,cols_idx_gamma])
#sample mean, column by column
post_mean_gamma <- apply(post_gamma,2,"mean") 
# bar plot of the posterior inclusion probabilities
names(post_mean_gamma) <- colnames(X) 
# Convert to data frame
variable_names <- c(
  "Overweight",                     # corresponds to "Overweight"
  "Overweight of younger",          # corresponds to "Overweight_minor_age"
  "Population",                     # corresponds to "Population"
  "More than 65",                   # corresponds to "More_65"
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



# # this initialization to insert if want to run for longer time 
# set.seed(37)
# inits_list <- list(
#   list(
#     beta0 = runif(1,-50,50),
#     beta_sex = rnorm(1),
#     beta = rnorm(p_temp),
#     space_rand_eff = rnorm(n_reg),
#     time_rand_eff = rnorm(n_years),
#     tau_space = rgamma(1,shape = 0.5, rate = 0.0005), 
#     tau_time = rgamma(1,shape = 0.5, rate = 0.0005),
#     rho = runif(1, 0, 0.5),
#     pred_y = runif(N),
#     betaphi_pre = runif(1),
#     gamma = rbinom(p_temp,size = 1, prob=0.5),
#     theta = runif(p_temp)
#   ),
#   list(
#     beta0 = runif(1,-50,50),
#     beta_sex = rnorm(1),
#     beta = rnorm(p_temp),
#     space_rand_eff = rnorm(n_reg),
#     time_rand_eff = rnorm(n_years),
#     tau_space = rgamma(1,shape = 0.5, rate = 0.0005), 
#     tau_time = rgamma(1,shape = 0.5, rate = 0.0005),
#     rho = runif(1, 0, 0.5),
#     pred_y = runif(N),
#     betaphi_pre = runif(1),
#     gamma = rbinom(p_temp,size = 1, prob=0.5),
#     theta = runif(p_temp)
#   )
# )
# inits_list








# Code for Reviewer 2 ------------------------------------------------

## 2.3 FIXED EFFECT ONLY MODEL --------------------------------------
#' constants <- list(X = X, 
#'                   N = length(y_til), p = p_temp,
#'                   #n_reg = n_reg, 
#'                   n_years = n_years, 
#'                   #space_index = space_index,
#'                   time_index = time_index,
#'                   sex = sex, 
#'                   #adj = adj, L = L, num = num, # icar
#'                   a = 50, a_phi = 0.1, b_phi = 0.1, #betaphi
#'                   tau2 = tau2, cc = cc # ssvs
#' )
#' 
#' # inits <- list(beta0 = 0,
#' #               beta_sex = 0,
#' #               beta = rep(0, p_temp),
#' #               space_rand_eff = rep(0, n_reg), time_rand_eff = rep(0, n_years),
#' #               tau_space = 1, tau_time = 1, 
#' #               rho = 0, 
#' #               pred_y = array(0.5, dim = N),
#' #               betaphi_pre = 0.5,
#' #               gamma = rep(1,p_temp), theta = rep(1,p_temp)
#' # )
#' set.seed(37)
#' inits_list <- list(
#'   list(
#'     beta0 = runif(1,-10,10),
#'     beta_sex = rnorm(1),
#'     beta = rnorm(p_temp),
#'     #space_rand_eff = rnorm(n_reg),
#'     time_rand_eff = rnorm(n_years),
#'     #tau_space = rgamma(1,shape = 0.5, rate = 0.0005), 
#'     tau_time = rgamma(1,shape = 0.5, rate = 0.0005),
#'     rho = runif(1, 0, 0.5),
#'     pred_y = runif(N),
#'     betaphi_pre = runif(1),
#'     gamma = rbinom(p_temp,size = 1, prob=0.5),
#'     theta = runif(p_temp)
#'   ),
#'   list(
#'     beta0 = runif(1,-10,10),
#'     beta_sex = rnorm(1),
#'     beta = rnorm(p_temp),
#'     #space_rand_eff = rnorm(n_reg),
#'     time_rand_eff = rnorm(n_years),
#'     #tau_space = rgamma(1,shape = 0.5, rate = 0.0005), 
#'     tau_time = rgamma(1,shape = 0.5, rate = 0.0005),
#'     rho = runif(1, 0, 0.5),
#'     pred_y = runif(N),
#'     betaphi_pre = runif(1),
#'     gamma = rbinom(p_temp,size = 1, prob=0.5),
#'     theta = runif(p_temp)
#'   )
#' )
#' inits_list
#' 
#' params_to_save = c('beta0', 
#'                    'beta_sex',
#'                    "beta",
#'                    'betaphi',
#'                    #'space_rand_eff', 
#'                    'time_rand_eff',
#'                    #'tau_space', 
#'                    'tau_time', # 'sig_space', 'sig_time', 
#'                    'rho',
#'                    'rmse', 'pred_y',
#'                    'gamma', 'theta', 'sig2')
#' 
#' code_fixedeffect <- nimbleCode({
#'   ## sampling
#'   for(i in 1:N){
#'     Y[i] ~ dbeta(mu[i]*betaphi, (1-mu[i])*betaphi)
#'     
#'     pred_y[i] ~ dbeta(mu[i] * betaphi, (1 - mu[i]) * betaphi)
#'     
#'     # logit(mu[i]) <- inprod(X[i,1:p], beta[1:p]) + beta_sex * (sex[i] - 1) +
#'     #   space_rand_eff[space_index[i]] + time_rand_eff[time_index[i]] + beta0
#'     
#'     logit(mu[i]) <- inprod(X[i,1:p], beta[1:p]) + beta_sex * (sex[i] - 1) +
#'       #space_rand_eff[space_index[i]] + 
#'       time_rand_eff[time_index[i]] + beta0
#'     
#'     se[i] <- pow((Y[i] - pred_y[i]),2)
#'     rse[i] <- pow(se[i], 1/2)
#'   }
#'   
#'   # priors
#'   
#'   # space and time rand effects
#'   # for(k in 1:L){
#'   #   weights[k] <- 1
#'   # }
#'   # space_rand_eff[1:n_reg] ~ dcar_normal(adj[1:L], weights[1:L], num[1:n_reg], tau_space, zero_mean = 1)
#'   # tau_space ~ dgamma(shape = 0.5, rate = 0.0005)
#'   
#'   time_rand_eff[1] ~ dnorm(0,  sd = sig_time)
#'   for(t in 2:n_years){
#'     time_rand_eff[t] ~ dnorm(rho * time_rand_eff[t-1], sd = sig_time)
#'   }
#'   tau_time ~ dgamma(shape = 0.5, rate = 0.0005)
#'   sig_time <- 1 / sqrt(tau_time)
#'   
#'   #rho ~ dnorm(0, sd = 1)
#'   rho ~ dbeta(1,1)
#'   
#'   # phi parameters of the beta distrib
#'   # betaphi ~ dgamma(a_phi,b_phi)
#'   betaphi_pre ~ dbeta(1 + a_phi, 1 + b_phi)
#'   betaphi <- pow(a * betaphi_pre, 2)
#'   
#'   # intercept
#'   # beta0 ~ dflat()
#'   beta0 ~ dnorm(0, sd = 10)
#'   
#'   # beta_sex
#'   beta_sex ~ dnorm(0, sd = 10)
#'   
#'   # for(s in 1:n_sex){
#'   #   gender_rand_eff[s] ~ dnorm(mu_sex, sd = sig_sex_randeff)
#'   # }
#'   #mu_sex ~ dnorm(0, sd = 2)
#'   #sig_sex_randeff ~ dunif(1,10)
#'   
#'   # SSVS for betas 1:p
#'   for(j in 1:p){
#'     sig2[j] <- equals(gamma[j],0)*var_spike+equals(gamma[j],1)*var_slab 
#'     sig[j] <- pow(sig2[j], 1/2)
#'     beta[j] ~ dnorm(0, sd = sig[j]) 
#'     gamma[j] ~ dbern(theta[j])
#'   }
#'   var_spike <- tau2
#'   var_slab  <- cc*tau2
#'   for(j in 1:p){
#'     ## Two options:
#'     theta[j]~dunif(0,1)
#'   }
#'   
#'   # getting rmse
#'   rmse <- mean(rse[1:N])
#' })
#' 
#' # # steps to run nimble -------------------------------------------------
#' iter = 150000
#' nchain = 2
#' burn = 50000
#' thin = 10
#' 
#' samples_list <- list()
#' time_elapsed <- numeric(2)
#' 
#' for (chain in 1:2) {
#'   model <- nimbleModel(code_fixedeffect, constants = constants, data = data, inits = inits_list[[chain]])
#'   print(model$calculate()) # if returns NA some problems in model definition so check!
#'   cModel <- compileNimble(model)
#'   conf <- configureMCMC(model, monitors = params_to_save)
#'   MCMC <- buildMCMC(conf)
#'   cMCMC <- compileNimble(MCMC, project = cModel)
#'   
#'   start_time <- Sys.time()
#'   
#'   samples_list[[chain]] <- runMCMC(cMCMC, niter = iter, nburnin = burn, thin = thin)
#'   
#'   end_time <- Sys.time()
#'   
#'   time_elapsed[chain] <- as.numeric(difftime(end_time, start_time, units = "secs"))
#' }
#' 
#' time_elapsed

# > time_elapsed
# [1] 1339.618 1280.845

# saveRDS(samples_list, file = "Paper_Model_rhobeta_sexfixed_nospace.rds")
samples_list = readRDS('Paper_Model_rhobeta_sexfixed_nospace.rds')

lapply(samples_list,dim)

mcmc_samples_chain1 <- samples_list[[1]]
mcmc_samples_chain2 <- samples_list[[2]] 

dim(mcmc_samples_chain1)
dim(mcmc_samples_chain2)

mcmc_samples_all = rbind(mcmc_samples_chain1,mcmc_samples_chain2)

iters = 1:nrow(mcmc_samples_chain1)

# go to Diagnostics Section to run and see some results

## 2.8 SPATIAL CROSS VALIDATION --------------------------------------
# reg_names
# get_region_id("Lombardia")  # should be 3
# get_region_id("Toscana")    # should be 9
# get_region_id("Campania")   # should be 15

# # Lombardia CV
# data_LOMB <- make_spatial_cv_data(
#   y_norm = y_til_norm,
#   space_index = space_index,
#   region_id = get_region_id("Lombardia")
# )
# # Toscana CV
# data_TOSC <- make_spatial_cv_data(
#   y_norm = y_til_norm,
#   space_index = space_index,
#   region_id = get_region_id("Toscana")
# )
# # Campania CV
# data_CAMP <- make_spatial_cv_data(
#   y_norm = y_til_norm,
#   space_index = space_index,
#   region_id = get_region_id("Campania")
# )

# # SSVS params
# c_ss<-4000
# cc = c_ss^2
# intersect<-0.001
# tau_ss<-intersect/sqrt(2*log(c_ss)*c_ss^2/(c_ss^2-1))
# tau2 <- tau_ss^2
# (tau2 * cc) %>% sqrt()

# # Constants for FIXED EFFECT GENDER
# constants <- list(X = X,
#                   N = length(y_til), p = p_temp,
#                   n_reg = n_reg, n_years = n_years,
#                   space_index = space_index, time_index = time_index,
#                   sex = sex,
#                   adj = adj, L = L, num = num, # icar
#                   a = 50, a_phi = 0.1, b_phi = 0.1, #betaphi
#                   tau2 = tau2, cc = cc # ssvs
# )

# Constants for RANDOM EFFECT GENDER
# constants <- list(X = X,
#                   N = length(y_til), p = p_temp,
#                   n_reg = n_reg, n_years = n_years,
#                   space_index = space_index, time_index = time_index,
#                   sex = sex, n_sex = n_sex, 
#                   adj = adj, L = L, num = num, # icar
#                   a = 50, a_phi = 0.1, b_phi = 0.1, #betaphi
#                   tau2 = tau2, cc = cc # ssvs
# )

# # Initial values for FIXED or RANDOM EFFECT GENDER:
# # IF want FIXED => comment gender_rand_eff, mu_sex and sig_sex_randeff, 
# # If want RANDOM => comment beta_sex
# set.seed(37)
# inits_list <- list(
#   list(
#     beta0 = runif(1,-50,50),
#     beta_sex = rnorm(1),                # gender fixed effect
#     #gender_rand_eff = array(0, n_sex), # gender random effect
#     # mu_sex = 0, sig_sex_randeff = 2,  # gender random effect
#     beta = rnorm(p_temp),
#     space_rand_eff = rnorm(n_reg),
#     time_rand_eff = rnorm(n_years),
#     tau_space = rgamma(1,shape = 0.5, rate = 0.0005),
#     tau_time = rgamma(1,shape = 0.5, rate = 0.0005),
#     rho = runif(1, 0, 0.5),
#     pred_y = runif(N),
#     betaphi_pre = runif(1),
#     gamma = rbinom(p_temp,size = 1, prob=0.5),
#     theta = runif(p_temp)
#   ),
#   list(
#     beta0 = runif(1,-50,50),
#     beta_sex = rnorm(1),                # gender fixed effect
#     #gender_rand_eff = array(0, n_sex), # gender random effect
#     # mu_sex = 0, sig_sex_randeff = 2,  # gender random effect
#     beta = rnorm(p_temp),
#     space_rand_eff = rnorm(n_reg),
#     time_rand_eff = rnorm(n_years),
#     tau_space = rgamma(1,shape = 0.5, rate = 0.0005),
#     tau_time = rgamma(1,shape = 0.5, rate = 0.0005),
#     rho = runif(1, 0, 0.5),
#     pred_y = runif(N),
#     betaphi_pre = runif(1),
#     gamma = rbinom(p_temp,size = 1, prob=0.5),
#     theta = runif(p_temp)
#   )
# )
# inits_list

#' # Params to save for FIXED or RANDOM EFFECT GENDER:
#' # IF want FIXED => comment gender_rand_eff, mu_sex and sig_sex_randeff, 
#' # If want RANDOM => comment beta_sex
#' params_to_save = c('beta0',
#'                    'beta_sex',                                  # gender fixed effect
#'                    #'gender_rand_eff','mu_sex','sig_sex_randeff', # gender random effect
#'                    "beta",
#'                    'betaphi',
#'                    'space_rand_eff', 'time_rand_eff','tau_space', 'tau_time',
#'                    'rho',
#'                    'rmse', 'pred_y',
#'                    'gamma', 'theta', 'sig2')

# # # CODE FOR FIXED EFFECT GENDER
# code <- nimbleCode({
#   ## sampling
#   for(i in 1:N){
#     Y[i] ~ dbeta(mu[i]*betaphi, (1-mu[i])*betaphi)
# 
#     pred_y[i] ~ dbeta(mu[i] * betaphi, (1 - mu[i]) * betaphi)
# 
#     logit(mu[i]) <- inprod(X[i,1:p], beta[1:p]) + beta_sex * (sex[i] - 1) +
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
#   space_rand_eff[1:n_reg] ~ dcar_normal(adj[1:L], weights[1:L], num[1:n_reg], tau_space, zero_mean = 1)
#   tau_space ~ dgamma(shape = 0.5, rate = 0.0005)
# 
#   time_rand_eff[1] ~ dnorm(0,  sd = sig_time)
#   for(t in 2:n_years){
#     time_rand_eff[t] ~ dnorm(rho * time_rand_eff[t-1], sd = sig_time)
#   }
#   tau_time ~ dgamma(shape = 0.5, rate = 0.0005)
#   sig_time <- 1 / sqrt(tau_time)
# 
#   rho ~ dnorm(0, sd = 1)
#   #rho ~ dbeta(1,1)
# 
#   # phi parameters of the beta distrib
#   # betaphi ~ dgamma(a_phi,b_phi)
#   betaphi_pre ~ dbeta(1 + a_phi, 1 + b_phi)
#   betaphi <- pow(a * betaphi_pre, 2)
# 
#   # intercept
#   beta0 ~ dflat()
# 
#   # beta_sex
#   beta_sex ~ dnorm(0, sd = 10)
# 
#   # for(s in 1:n_sex){
#   #   gender_rand_eff[s] ~ dnorm(mu_sex, sd = sig_sex_randeff)
#   # }
#   #mu_sex ~ dnorm(0, sd = 2)
#   #sig_sex_randeff ~ dunif(1,10)
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

# # CODE FOR RANDOM EFFECT GENDER
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
#   rho ~ dnorm(0, sd = 1)
#   #rho ~ dbeta(1,1)
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
#### Run spatial cross-validation ------------------------------------------
# cv_data_list <- list(
#   Lombardia = data_LOMB,
#   Toscana   = data_TOSC,
#   Campania  = data_CAMP
# )

# run_spatial_cv <- function(region_name, data_cv) {
# 
#   cat("\n===== Running spatial CV for:", region_name, "=====\n")
# 
#   ## start total timer
#   t_start_total <- Sys.time()
# 
#   samples_list <- vector("list", nchain)
#   chain_time   <- numeric(nchain)
# 
#   for (chain in 1:nchain) {
# 
#     cat("  -> Chain", chain, "\n")
# 
#     t_start_chain <- Sys.time()
# 
#     model <- nimbleModel(
#       code = code,
#       constants = constants,
#       data = data_cv,
#       inits = inits_list[[chain]]
#     )
# 
#     cModel <- compileNimble(model)
#     conf   <- configureMCMC(model, monitors = params_to_save)
#     MCMC   <- buildMCMC(conf)
#     cMCMC  <- compileNimble(MCMC, project = cModel)
# 
#     samples_list[[chain]] <- runMCMC(
#       cMCMC,
#       niter    = iter,
#       nburnin = burn,
#       thin    = thin
#     )
# 
#     ## end chain timer
#     chain_time[chain] <- as.numeric(
#       difftime(Sys.time(), t_start_chain, units = "secs")
#     )
#   }
# 
#   ## end total timer
#   total_time <- as.numeric(
#     difftime(Sys.time(), t_start_total, units = "secs")
#   )
# 
#   ## object to save
#   output <- list(
#     region        = region_name,
#     samples       = samples_list,
#     runtime_sec   = total_time,
#     chain_runtime = chain_time,
#     settings      = list(
#       iter = iter,
#       burn = burn,
#       thin = thin,
#       nchain = nchain
#     )
#   )
# 
#   ## save
#   saveRDS(
#     output,
#     file = paste0("samples_CV_fixed_normal", region_name, ".rds")
#   )
#    # "samples_CV_"               # rho beta, sex fixed
#    # "samples_CV_fixed_normal"   # rho normal, sex fixed
#    # "samples_CV_random_beta"    # rho beta, sex random
#    # "samples_CV_random_normal"  # rho normal, sex random
# 
#   return(output)
# }


# iter  = 150000
# nchain = 2
# burn  = 50000
# thin  = 10

# cv_results <- list()

# for (reg in names(cv_data_list)) {
#   cv_results[[reg]] <- run_spatial_cv(
#     region_name = reg,
#     data_cv     = cv_data_list[[reg]]
#   )
# }

### Chains --------------------------------
# samples_CV_.rds              M1
# samples_CV_random_beta.rds   M3
# samples_CV_random_normal.rds M4
# samples_CV_fixed_normal.rds  M2

res_LOMB <- readRDS("samples_CV_fixed_normalLombardia.rds")
res_LOMB$runtime_sec / 3600  # hours
res_LOMB$chain_runtime
res_TOSC <- readRDS("samples_CV_fixed_normalToscana.rds")
res_TOSC$runtime_sec / 3600  # hours
res_TOSC$chain_runtime
res_CAMP <- readRDS("samples_CV_fixed_normalCampania.rds")
res_CAMP$runtime_sec / 3600  # hours
res_CAMP$chain_runtime

### Outputs for predictive of spatial cross validation ------------------------
test_idx_LOMB = which(space_index == get_region_id('Lombardia'))
test_idx_TOSC = which(space_index == get_region_id('Toscana'))
test_idx_CAMP = which(space_index == get_region_id('Campania'))

cols_idx_betas <- which(grepl("beta\\[",colnames(mcmc_samples_all)))
cols_idx_space <- which(grepl("space_rand_eff\\[",colnames(mcmc_samples_all)))
cols_idx_time <- which(grepl("time_rand_eff\\[",colnames(mcmc_samples_all)))

cols_idx_space[get_region_id("Lombardia")]  
cols_idx_space[get_region_id("Toscana")]    
cols_idx_space[get_region_id("Campania")]   

# this gives the starting indexes where the chain has converged
# convergence_map[[model]][[region]][[chain]] = start_iter
convergence_map <- list(
  
  M1 = list(
    Lombardia = c(chain1 = 6000, chain2 = 1),
    Toscana   = c(chain1 = 1,    chain2 = 2000),
    Campania  = c(chain1 = 1,    chain2 = 1)
  ),
  
  M2 = list(
    Lombardia = c(chain1 = 1, chain2 = 1),
    Toscana   = c(chain1 = 1, chain2 = 1),
    Campania  = c(chain1 = 1, chain2 = 1)
  ),
  
  M3 = list(
    Lombardia = c(chain1 = 1,    chain2 = 1),
    Toscana   = c(chain1 = 1,    chain2 = 3500),
    Campania  = c(chain1 = 1,    chain2 = 7500)
  ),
  
  M4 = list(
    Lombardia = c(chain1 = 1,    chain2 = 1),
    Toscana   = c(chain1 = 1,    chain2 = 2500),
    Campania  = c(chain1 = 1,    chain2 = 1)
  )
)

get_converged_mcmc <- function(res, model, region, conv_map) {
  
  s1 <- conv_map[[model]][[region]]["chain1"]
  s2 <- conv_map[[model]][[region]]["chain2"]
  
  chain1 <- res$samples[[1]][s1:nrow(res$samples[[1]]), , drop = FALSE]
  chain2 <- res$samples[[2]][s2:nrow(res$samples[[2]]), , drop = FALSE]
  
  rbind(chain1, chain2)
}

get_converged_preds <- function(pred_obj, model, region, conv_map) {
  
  s1 <- conv_map[[model]][[region]]["chain1"]
  s2 <- conv_map[[model]][[region]]["chain2"]
  
  p1 <- pred_obj$pred_ch1[s1:nrow(pred_obj$pred_ch1), , drop = FALSE]
  p2 <- pred_obj$pred_ch2[s2:nrow(pred_obj$pred_ch2), , drop = FALSE]
  
  rbind(p1, p2)
}

model_files <- list(
  M1 = "samples_CV_",
  M2 = "samples_CV_fixed_normal",
  M3 = "samples_CV_random_beta",
  M4 = "samples_CV_random_normal"
)

regions <- c("Lombardia", "Toscana", "Campania")
models  <- names(model_files)

summary_list <- list()
results_all  <- list()

for (model in models) {
  for (region in regions) {
    cat("Model:", model, "| Region:", region, "\n")
    
    # ---- Load results
    res <- readRDS(paste0(model_files[[model]], region, ".rds"))
    
    # ---- Get converged samples
    mcmc_samples <- get_converged_mcmc(
      res, model, region, convergence_map
    )
    
    # ---- Region-specific predictions from converged samples
    pred_obj <- extract_region_predictions(res, region, space_index)
    
    pred_samples <- get_converged_preds(
      pred_obj, model, region, convergence_map
    )
    
    # ---- Indices
    test_idx <- which(space_index == get_region_id(region))
    
    cols_idx_betas <- which(grepl("beta\\[", colnames(mcmc_samples)))
    cols_idx_space <- which(grepl("space_rand_eff\\[", colnames(mcmc_samples)))
    cols_idx_time  <- which(grepl("time_rand_eff\\[", colnames(mcmc_samples)))
    
    # ---- Evaluation
    res_eval <- evaluate_bayes_predictions_region(
      pred_samples = pred_samples,
      mcmc_samples = mcmc_samples,
      X = X,
      test_indices = test_idx,
      y_true = y_til,
      y_min = y_min,
      y_max = y_max,
      cols_idx_betas = cols_idx_betas,
      cols_idx_space = cols_idx_space[get_region_id(region)],
      cols_idx_time = cols_idx_time,
      inv_logit = inv_logit,
      region_name = region
    )
    
    
    long_ppc <- create_long_predictions(
      pred_matrix = de_min_max_normalize(pred_samples, y_max, y_min),
      true_values = y_til[test_idx],
      region_name = region)
    
    p_density <- plot_ppc_density(
      long_predictions = long_ppc$predictions,
      truth_data = long_ppc$truth,
      region_name = region,
      spacing = 0.03
    )
    
    res_eval$density_plot <- p_density
    
    # ---- Store
    results_all[[model]][[region]] <- res_eval
    
    summary_list[[length(summary_list) + 1]] <- tibble(
      Model = model,
      Region = region,
      RMSE = res_eval$rmse,
      RMSE_Female = res_eval$rmse_female,
      RMSE_Male = res_eval$rmse_male,
      MAE = res_eval$mae,
      Mean_Bayes_p = res_eval$bayesian_p_value,
      Total_LPD = res_eval$total_log_pred_density
    )
    
  }
}

summary_list 
summary_df <- bind_rows(summary_list)
summary_df <- summary_df %>% mutate(
    Region = factor(Region, levels = c("Lombardia", "Toscana", "Campania"))
  ) %>% arrange(Region,Model)
summary_df
summary_df$Total_LPD


# for(region in regions){
#   for(model in models){
#     print(results_all[[model]][[region]]$plot)
#     cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
#     input <- readline()
#     if (tolower(input) == "quit") {
#       break
#     }
#     dev.off() 
#   }
# }

# for(region in regions){
#   for(model in models){
#     print(results_all[[model]][[region]]$density_plot)
#     cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
#     input <- readline()
#     if (tolower(input) == "quit") {
#       break
#     }
#     dev.off() 
#   }
# }

credible_df <- map_dfr(models, function(model) {
  map_dfr(regions, function(region) {
    
    res <- results_all[[model]][[region]]
    
    tibble(
      Region = region,
      Model = model,
      Year = rep(2010:2022, times = 2),
      Gender = rep(c("Female", "Male"), each = 13),
      low = res$credible_intervals$lower,
      high = res$credible_intervals$upper,
      post_mean = res$pred_mean,
      observed = y_til[space_index == get_region_id(region)]
    )
  })
})
credible_df %>% print(n=26*2)

credible_df <- credible_df %>%
  mutate(
    Region = factor(
      Region,
      levels = c("Lombardia", "Toscana", "Campania")
    )
  )

credible_df %>%
  ggplot(aes(x = Year)) +
  geom_ribbon(aes(ymin = low, ymax = high),
              fill = "grey80", alpha = 0.6) +
  geom_line(aes(y = post_mean), linewidth = 0.9) +
  geom_point(aes(y = observed), color = "red", size = 1.8) +
  facet_grid(Region ~ Model + Gender) +
  labs(
    y = "Predictions",
    title = "Posterior predictive intervals vs observed values"
  ) +
  scale_x_continuous(breaks = seq(2010,2022,3)) +
  theme_minimal(base_size = 11)+
  theme(
    axis.text.x = element_text(size = 10, angle = 30, hjust = 1),
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text.y = element_text(size = 13),
    strip.text.x = element_text(size = 12)
  )




# It follows some more code not necessary, that adds interesting things, like 
# - bayesian pvalues for each year and gender

# # Predictions (normalized scale)
# LOMB_pred <- extract_region_predictions(res_LOMB,"Lombardia",space_index)
# TOSC_pred <- extract_region_predictions(res_TOSC,"Toscana",space_index)
# CAMP_pred <- extract_region_predictions(res_CAMP,"Campania",space_index)
# # Observed values (NìOriginal scale)
# LOMB_true = extract_region_truth(y_til, space_index, 'Lombardia')
# TOSC_true = extract_region_truth(y_til, space_index, 'Toscana')
# CAMP_true = extract_region_truth(y_til, space_index, 'Campania')

# Denormalize
# pred_bind = rbind(LOMB_pred$pred_ch2, LOMB_pred$pred_ch1[6000:10000,]) # M1
LOMB_pred_denorm <- de_min_max_normalize(pred_bind, y_max, y_min)

#pred_bind = rbind(TOSC_pred$pred_ch1, TOSC_pred$pred_ch2[2000:10000,]) # M1
TOSC_pred_denorm <- de_min_max_normalize(pred_bind, y_max, y_min)

# pred_bind = rbind(CAMP_pred$pred_ch1, CAMP_pred$pred_ch2)             # M1
CAMP_pred_denorm <- de_min_max_normalize(pred_bind, y_max, y_min)


# Seeing Bayesian p-values
bayesian_pvalue_diagnostics(LOMB_pred_denorm, LOMB_true$true,
                            region_name = 'Lombardia')
bayesian_pvalue_diagnostics(TOSC_pred_denorm, TOSC_true$true,
                            region_name = 'Toscana')
bayesian_pvalue_diagnostics(CAMP_pred_denorm, CAMP_true$true,
                            region_name = 'Campania')



## 2.9 PREDICTIVE UNCERTAINTY ----------------------------------------
# see Main_code

## 2.10 SSVS ---------------------------------------------------------
# see 1.3 Section of this script

## 2.18 INCLUDING LEGEND OF PLOT -------------------------------------






# Diagnostics -------------------------------------------------------------

# intercept (dflat) 
plot(iters, mcmc_samples_chain1[,"beta0"], type = "l", xlab = "beta0", 
     ylab = expression(beta[0]))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "beta0"], col = 'red')
#lines(iters, mcmc_samples_chain3[, "beta0"], col = 'blue')
acf(mcmc_samples_chain1[, "beta0"])
acf(mcmc_samples_chain2[, "beta0"])
#acf(mcmc_samples_chain3[, "beta0"])
acf(ch1_df[, "beta0"])
acf(ch2_df[, "beta0"])
acf(ch3_df[, "beta0"])

# beta_sex (gender fixed effect) by sex
plot(iters, mcmc_samples_chain1[, "beta_sex"], type = "l", xlab = "Iterations", 
     ylab = expression(delta[1]))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "beta_sex"], col = 'red')
lines(iters, mcmc_samples_chain3[, "beta_sex"], col = 'blue')

# gender rand eff
plot(iters, mcmc_samples_chain1[, "beta0"] + mcmc_samples_chain1[, "gender_rand_eff[1]"], 
     type = "l", xlab = "Iterations", 
     ylab = expression(delta[1]))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "beta0"] + mcmc_samples_chain2[, "gender_rand_eff[1]"], col = 'red')

plot(iters, mcmc_samples_chain1[, "beta0"] + mcmc_samples_chain1[, "gender_rand_eff[2]"], 
     type = "l", xlab = "Iterations", 
     ylab = expression(delta[1]))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "beta0"] + mcmc_samples_chain2[, "gender_rand_eff[2]"], col = 'red')

# whole intercept with sex fixed
plot(iters, mcmc_samples_chain1[,"beta0"]+mcmc_samples_chain1[, "beta_sex"], 
     type = "l", xlab = "beta0", 
     ylab = expression(beta[0]))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "beta0"]+mcmc_samples_chain2[, "beta_sex"], col = 'red')


# space and time precisions
plot(iters, mcmc_samples_chain1[, "tau_space"], type = "l", xlab = "tau_space",
     ylab = expression(tau_space))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "tau_space"], col = 'red')
lines(iters, mcmc_samples_chain3[, "tau_space"], col = 'blue')
acf(mcmc_samples_chain1[, "tau_space"])
acf(mcmc_samples_chain2[, "tau_space"])

plot(iters, mcmc_samples_chain1[, "tau_time"], type = "l", xlab = "tau_time",
     ylab = expression(tau_time))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "tau_time"], col = 'red')
lines(iters, mcmc_samples_chain3[, "tau_time"], col = 'blue')
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
lines(iters, mcmc_samples_chain3[, "rho"], col = 'blue')
acf(mcmc_samples_chain1[, "rho"])
acf(mcmc_samples_chain2[, "rho"])

# root mse
plot(iters, mcmc_samples_chain1[, "rmse"], type = "l", xlab = "rmse", 
     ylab = expression(rmse))#, ylim = c(-20, 20))
lines(iters, mcmc_samples_chain2[, "rmse"], col = 'red')
lines(iters, mcmc_samples_chain3[, "rmse"], col = 'blue')
acf(mcmc_samples_chain1[, "rmse"])
acf(mcmc_samples_chain2[, "rmse"])

# betas 
# function for plotting beta traceplots
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
plot_space_randeff <- function(chain, i, xlab, ylab, third_chain = F) {
  plot(iters, mcmc_samples_chain1[, paste0('space_rand_eff[',i,']')], type = "l", 
       xlab = xlab, ylab = ylab)
  lines(iters, mcmc_samples_chain2[, paste0('space_rand_eff[', i,']')], col = 'red')
  if(third_chain)lines(iters, mcmc_samples_chain3[, paste0('space_rand_eff[', i,']')], col = 'blue')
}
for (i in 1:n_reg) {
  plot_space_randeff("chain2", i, reg_names_zones[i], expression(space_rand_eff),
                     third_chain = T)
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}  

# random effects: TIME
plot_time_randeff <- function(chain, i, xlab, ylab, third_chain = F) {
  plot(iters, mcmc_samples_chain1[, paste0('time_rand_eff[',i,']')], type = "l", 
       xlab = xlab, ylab = ylab)
  lines(iters, mcmc_samples_chain2[, paste0('time_rand_eff[', i,']')], col = 'red')
  if(third_chain)lines(iters, mcmc_samples_chain3[, paste0('time_rand_eff[', i,']')], col = 'blue')
}
for (i in 1:n_years) {
  plot_time_randeff("chain2", i, seq(2010,2022,1)[i], expression(time_rand_eff),
                    third_chain = F)
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}  

### ssvs plot ------------------------------------------------------
cols_idx_gamma<- which(grepl("gamma",colnames(mcmc_samples_all)))
post_gamma <-as.matrix(mcmc_samples_all[,cols_idx_gamma])
#sample mean, column by column
post_mean_gamma <- apply(post_gamma,2,"mean") 
# bar plot of the posterior inclusion probabilities
names(post_mean_gamma) <- colnames(X) 
# Convert to data frame
variable_names <- c(
  "Overweight",                     # corresponds to "Overweight"
  "Overweight of younger",          # corresponds to "Overweight_minor_age"
  "Population",                     # corresponds to "Population"
  "More than 65",                   # corresponds to "More_65"
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

# Betas
cols_idx_betas <- which(grepl("beta\\[",colnames(mcmc_samples_all)))
post_betas <-as.matrix(mcmc_samples_all[,cols_idx_betas])
post_mean_betas <- apply(post_betas,2,"mean") 
names(post_mean_betas) <- colnames(X) 

post_betas = as_tibble(post_betas)
colnames(post_betas) = variable_names
post_betas = post_betas %>% 
  mutate(Chain = c(rep(1,nrow(mcmc_samples_chain1)),
                   rep(2,nrow(mcmc_samples_chain2))))
post_betas$Chain = as.factor(post_betas$Chain)
dim(post_betas)

post_betas[,c((post_mean_gamma_df$value>0.5),TRUE)] 
post_betas_significant = post_betas[,c((post_mean_gamma_df$value>0.5),TRUE)] 

post_betas_significant 

post_betas_significant_long <- post_betas_significant %>%
  pivot_longer(
    cols = -Chain,
    names_to = "coefficient",
    values_to = "value"
  )

ggplot(post_betas_significant_long) +
  geom_histogram(aes(x = value, fill = Chain), alpha = 0.6) +
  facet_wrap(~ coefficient, scales = "free") +
  labs(
    x = "Coefficient value",
    y = "Posterior density"
  ) +
  theme_minimal()


# Space correlation of significant X
plot_map <- function(data, fill_var, year, lim_min, lim_max, legend_title,
                     sex) {
  plot <- data %>%
    ggplot() +
    geom_sf(aes(fill = !!sym(fill_var))) +
    ggtitle(paste0("Maps of ", fill_var, ", year: ", year, ", ", sex)) +
    theme_bw() +
    scale_fill_gradient(low = 'yellow', high = 'red', limits = c(lim_min, lim_max)) +
    labs(fill = legend_title) +  # Adding custom legend title
    theme(
      legend.position = 'right',
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(face = 'bold', size = 13, hjust = 0.25)
    ) 
  return(plot)
}

library(sf)
library(gridExtra)
shape = st_read("C:\\Users\\lucia\\Desktop\\Tirocinio\\codici\\italia_maps",
                layer = 'Reg01012023_g_WGS84')

X_df = bind_cols(X,X_til[,c(41,42,43)])
colnames(X_df)
X_df %>% filter(sex==0) %>% summarise(mean(Overweight))
X_df %>% filter(sex==1) %>% summarise(mean(Overweight))
years = 2010:2022
dfs <- list()
# Loop through years from 2010 to 2022
for (i in 1:n_years) {
  # Filter data for male and female for the current year
  df_f_20xx <- X_df %>% filter(time == i, sex == 0) %>% select(-c(time, sex))
  df_m_20xx <- X_df %>% filter(time == i, sex == 1) %>% select(-c(time, sex))
  
  # Store dataframes in the list with the year as its name
  dfs[[paste0('Year:', years[i])]] <- list(df_m = df_m_20xx, 
                                       df_f = df_f_20xx)
}
dfs_indexes = names(dfs)

X_df_female = X_df %>% filter(sex==0)
X_df_male = X_df %>% filter(sex==1)

X_df_female_means <- X_df_female %>%
  group_by(space) %>%
  summarise(across(where(is.numeric), ~ mean(.x)))
X_df_male_means <- X_df_male %>%
  group_by(space) %>%
  summarise(across(where(is.numeric), ~ mean(.x)))


s_all <- list()
for (i in 1:n_years) {
  # Filter data for male and female for the current year
  s_f_20xx <- shape %>% left_join(dfs[[dfs_indexes[i]]]$df_f, by = join_by(COD_REG == space)) 
  s_m_20xx <- shape %>% left_join(dfs[[dfs_indexes[i]]]$df_m, by = join_by(COD_REG == space)) 
  # Store dataframes in the list with the year as its name
  s_all[[paste0('Year:', years[i])]] <- list(s_f = s_f_20xx, 
                                         s_m = s_m_20xx)
}

s_mean_all <- list(s_f = shape %>% 
                     left_join(X_df_female_means, by = join_by(COD_REG == space)), 
                   s_m = shape %>% 
                     left_join(X_df_male_means, by = join_by(COD_REG == space)))

significant_vars = colnames(X)[post_mean_gamma_df$value>0.5]
significant_vars_names = variable_names[post_mean_gamma_df$value>0.5]

X_df$Overweight %>% min()
X_df$Overweight %>% max()

X_df[,significant_vars[1]]

# Visualizing temporal mean of significant variables
for (i in 1:length(significant_vars)) {
  
  lim_min = min(X_df[,significant_vars[i]])
  lim_max = max(X_df[,significant_vars[i]])
  
  pm <- s_mean_all$s_m %>%
    ggplot() +
    geom_sf(aes(fill = !!sym(significant_vars[i]))) +
    ggtitle(paste0("Maps of ", significant_vars_names[i], ", male")) +
    theme_bw() +
    scale_fill_gradient(low = 'yellow', high = 'red', limits = c(lim_min, lim_max)) +
    labs(fill = "") +  # Adding custom legend title
    theme(
      #legend.position = 'right',
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(face = 'bold', size = 13, hjust = 0.25)
    ) +
    theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(1.45, 1.45, 1.45, 1.45, "cm")
    )
  
  pf <- s_mean_all$s_f %>%
    ggplot() +
    geom_sf(aes(fill = !!sym(significant_vars[i]))) +
    ggtitle(paste0("Maps of ", significant_vars_names[i], ", female")) +
    theme_bw() +
    scale_fill_gradient(low = 'yellow', high = 'red', limits = c(lim_min, lim_max)) +
    labs(fill = "") +  # Adding custom legend title
    theme(
      legend.position = 'right',
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(face = 'bold', size = 13, hjust = 0.25)
    ) +
    theme(
      #legend.position = "none",
      plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
  
  grid.arrange(grobs = list(pm, pf), ncol = 2)
  
  print(c(lim_min, lim_max))
  
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  
  if (tolower(input) == "quit") {
    break
  }
  
  dev.off()
}

s_mean_all$s_f$Mean_family_components
s_mean_all$s_m$Mean_family_components

round(X_til[1:260,'health_expenditure_by_pop']-X_til[261:520,'health_expenditure_by_pop'],3)

s_mean_all$s_f %>%
  ggplot() +
  geom_sf(aes(fill = !!sym(significant_vars[9]))) +
  ggtitle(paste0("Maps of ", significant_vars_names[9])) +
  theme_bw() +
  scale_fill_gradient(low = 'yellow', high = 'red', limits = c(lim_min, lim_max)) +
  labs(fill = "") +  # Adding custom legend title
  theme(
    legend.position = 'right',
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = 'bold', size = 13, hjust = 0.25)
  ) +
  theme(
    #legend.position = "none",
    plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
  )



# Visualizing values for each region at each year of significant variable
plot_graphs <- function(var_name,
                        s_all,
                        dfs_indexes,
                        years,
                        X_df,
                        n_years) {
  
  # compute limits once
  lim_min <- min(X_df[[var_name]])
  lim_max <- max(X_df[[var_name]])
  
  for (i in seq_len(n_years)) {
    
    pm <- plot_map(
      s_all[[dfs_indexes[i]]]$s_m,
      fill_var = var_name,
      year = years[i],
      lim_min = lim_min,
      lim_max = lim_max,
      legend_title = " standardized",
      sex = "Male"
    ) +
      theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(1.55, 1.55, 1.55, 1.55, "cm")
      )
    
    pf <- plot_map(
      s_all[[dfs_indexes[i]]]$s_f,
      fill_var = var_name,
      year = years[i],
      lim_min = lim_min,
      lim_max = lim_max,
      legend_title = " standardized",
      sex = "Female"
    )
    
    grid.arrange(grobs = list(pm, pf), ncol = 2)
    
    cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
    input <- readline()
    
    if (tolower(input) == "quit") {
      break
    }
    
    dev.off()
  }
}

plot_graphs(
  var_name    = significant_vars[1],
  s_all       = s_all,
  dfs_indexes = dfs_indexes,
  years       = years,
  X_df        = X_df,
  n_years     = n_years
)
