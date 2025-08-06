# preliminaries -------------
library(nimble) 
library(mcmcplots)
library(tidyverse) 
library(ggridges)
library(moments)
library(ggrepel)

setwd("C:\\Users\\lucia\\Desktop\\STBetaBayes_Obesity_Italy\\data_and_models")

min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

de_min_max_normalize = function(x_norm, x_max, x_min){
  x = x_norm * (x_max-x_min) + x_min
  return(x)
}

W = readRDS('W')

load('X_til') 
load('y_til')


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

# Y and X
y_til_norm = min_max_normalize(y_til)
y_til_norm[order(y_til_norm)]

y_til_norm[y_til_norm==1] = 0.999
y_til_norm[y_til_norm==0] = 0.001

y_max = y_til[which.max(y_til)]
y_min = y_til[which.min(y_til)]

# Y
y_til_norm[1:260] %>% mean()
y_til_norm[261:520] %>% mean()

# X
load('X_all_covariates_in_model') 
colnames(X)
X[1:260,1]%>%mean()
X[261:520,1]%>%mean()

p_temp = ncol(X)


# ALL MODELS (training) ---------------------------------------------------
#load('Train_rhonormal_yesintercept_sexfixed')
load('Train_rhobeta_yesintercept_sexfixed')
# load('Train_rhobeta_yesintercept_sexrandom')
#load('Train_rhonormal_yesintercept_sexrandom')

mcmc_samples_chain1 <- mcmc(samples$chain1)
mcmc_samples_chain2 <- mcmc(samples$chain2)
iters = 1:nrow(mcmc_samples_chain2)


# SPLIT TRAIN / TEST -----------------------------------
# Separate 2022 test indices
train_indices <- which(X_til[,43] <= 12)  # Years 1 to 12
test_indices <- which(X_til[,43] == 13)   # Year 13 = 2022

# Training set
X_train <- X[train_indices,] # NB: use X and not X_til because we trained on X
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

# test set
X_test <- X[test_indices,]
true_2022 <- y_til[test_indices]
#y_test_norm = min_max_normalize(true_2022)
sex_test <- sex[test_indices]
space_test <- space_index[test_indices]
time_test <- rep(13, length(test_indices))  # year 13 (2022)

# Posterior Prediction function
# NB in row where we calculate the linear predictor, use 
# time_eff[12] * rho if you're dealing with training model, use
# time_eff[time_test] if you're dealing with test model
evaluate_model_predictions <- function(samples, X_test, space_test, time_test, sex_test, 
                                       true_y, y_min, y_max, model_name = "model", n_draws = 1000) {
  inv_logit <- function(x) 1 / (1 + exp(-x))
  
  # Sample from posterior
  S <- n_draws
  N <- nrow(X_test)
  sample_indices <- sample(1:nrow(samples), S)
  
  pred_mat <- matrix(NA, nrow = S, ncol = N)
  log_densities <- matrix(NA, nrow = S, ncol = N)
  
  for (s in 1:S) {
    idx <- sample_indices[s]
    
    beta0 <- samples[idx, "beta0"]
    betaphi <- samples[idx, "betaphi"]
    beta <- samples[idx, grep("^beta\\[", colnames(samples))]
    space_eff <- samples[idx, grep("^space_rand_eff\\[", colnames(samples))]
    time_eff <- samples[idx, grep("^time_rand_eff\\[", colnames(samples))]
    rho <- samples[idx, 'rho']
    
    if ("gender_rand_eff[1]" %in% colnames(samples)) {
      gender_eff <- samples[idx, grep("^gender_rand_eff\\[", colnames(samples))]
      gender_component <- gender_eff[sex_test]
    } else {
      beta_sex <- samples[idx, "beta_sex"]
      gender_component <- beta_sex * (sex_test - 1)
    }
    
    linpred <- X_test %*% beta + beta0 + gender_component +
      space_eff[space_test] +
      time_eff[12] * rho # time_eff[time_test] 
    
    mu <- inv_logit(linpred)
    
    alpha_param <- mu * betaphi
    beta_param <- (1 - mu) * betaphi
    
    # Predictive samples
    pred_mat[s, ] <- rbeta(N, shape1 = alpha_param, shape2 = beta_param)
    
    # Log predictive density
    y_scaled <- (true_y - y_min) / (y_max - y_min)
    y_scaled[y_scaled == 1] <- 0.999
    y_scaled[y_scaled == 0] <- 0.001
    log_densities[s, ] <- dbeta(y_scaled, shape1 = alpha_param, shape2 = beta_param, log = TRUE)
  }
  
  # Posterior predictive summaries
  pred_mean <- colMeans(pred_mat)
  pred_ci_lower <- apply(pred_mat, 2, quantile, probs = 0.025)
  pred_ci_upper <- apply(pred_mat, 2, quantile, probs = 0.975)
  
  # Denormalize
  pred_mean_denorm <- pred_mean * (y_max - y_min) + y_min
  pred_ci_lower_denorm <- pred_ci_lower * (y_max - y_min) + y_min
  pred_ci_upper_denorm <- pred_ci_upper * (y_max - y_min) + y_min
  
  pred_samples_denorm <- pred_mat * (y_max - y_min) + y_min
  
  
  # Metrics
  rmse <- sqrt(mean((pred_mean_denorm - true_y)^2))
  rmse_f <- sqrt(mean((pred_mean_denorm[1:20] - true_y[1:20])^2))
  rmse_m <- sqrt(mean((pred_mean_denorm[21:40] - true_y[21:40])^2))
  mae <- mean(abs(pred_mean_denorm - true_y))
  bayes_pvalues <- sapply(1:N, function(i) mean(pred_mat[, i] > (true_y[i] - y_min) / (y_max - y_min)))
  
  # metrics sample by sample
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
  
  
  # Log predictive density
  lpd_i <- log(colMeans(exp(log_densities)))  # log of mean predictive density per obs
  total_lpd <- sum(lpd_i)
  
  # Output summary
  summary <- list(
    model = model_name,
    RMSE = rmse,
    RMSE_female = rmse_f,
    RMSE_male = rmse_m,
    RMSE_all_samples = rmse_overall,
    RMSE_all_samples_female = rmse_overall_f,
    RMSE_all_samples_male = rmse_overall_m,
    MAE = mae,
    mean_pval = mean(bayes_pvalues),
    total_log_pred_density = total_lpd
  )

  df_plot.tb <- tibble(
    Region = rep(reg_names,2),
    Sex = c(rep("Female", times = 20),rep("Male", times = 20)),
    True = true_2022,
    Predicted = pred_mean_denorm,
    lower = pred_ci_lower_denorm,
    upper = pred_ci_upper_denorm,
    p_value = bayes_pvalues
  )
  
  list(summary = summary, pred_draws = pred_mat, plot_data = df_plot.tb)
}


results_list <- list()

load('Train_rhobeta_yesintercept_sexfixed')
samples_model1 = rbind(samples$chain1,samples$chain2)
load('Train_rhonormal_yesintercept_sexfixed')
samples_model2 = rbind(samples$chain1,samples$chain2)
load('Train_rhobeta_yesintercept_sexrandom')
samples_model3 = rbind(samples$chain1,samples$chain2)
load('Train_rhonormal_yesintercept_sexrandom')
samples_model4 = rbind(samples$chain1,samples$chain2)

# run the following if you want to go back to a specific model
#load('Train_rhonormal_yesintercept_sexfixed')
load('Train_rhobeta_yesintercept_sexfixed')
# load('Train_rhobeta_yesintercept_sexrandom')
#load('Train_rhonormal_yesintercept_sexrandom')

# getting all the evaluations
results_list[["rho_beta_fixed"]] <- evaluate_model_predictions(samples_model1, X_test, space_test, time_test, sex_test, true_2022, y_min, y_max, model_name = "rho_beta_fixed")
results_list[["rho_normal_fixed"]] <- evaluate_model_predictions(samples_model2, X_test, space_test, time_test, sex_test, true_2022, y_min, y_max, model_name = "rho_normal_fixed")
results_list[["rho_beta_random"]] <- evaluate_model_predictions(samples_model3, X_test, space_test, time_test, sex_test, true_2022, y_min, y_max, model_name = "rho_beta_random")
results_list[["rho_normal_random"]] <- evaluate_model_predictions(samples_model4, X_test, space_test, time_test, sex_test, true_2022, y_min, y_max, model_name = "rho_normal_random")

# Compare summaries
summary_table <- do.call(rbind, lapply(results_list, function(x) x$summary))
print(summary_table)
# NB here RMSE_all_samples and the same but by gender just are calculated for each 
# region, but in the paper not reported because they lead to the same conclusions

# names(results_list)
#results_list[['rho_beta_fixed']]$plot_data
for (name in names(results_list)) {
  df <- results_list[[name]]$plot_data
  print(
    ggplot(df, aes(x = True, y = Predicted, color = Sex)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_text_repel(aes(label = Region), size = 3, show.legend = FALSE) +
      labs(title = "Predicted vs Actual Obesity Rate (2022)",
           subtitle = paste0('Model: ',name)
             ) +
      theme_minimal()+
      facet_wrap(~Sex)
    )
  
  cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
  input <- readline()
  if (tolower(input) == "quit") {
    break
  }
  dev.off()
}

# # to save images
# output_dir <- 
#   "your directory"
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
# # Loop through each result and save the plot
# mod_index = 1
# for (name in names(results_list)) {
#   df <- results_list[[name]]$plot_data
#   
#   p <- ggplot(df, aes(x = True, y = Predicted, color = Sex)) +
#     geom_point(size = 2) +
#     geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.7) +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#     geom_text_repel(aes(label = Region), size = 3, show.legend = FALSE) +
#     labs(title = "Predicted vs Actual Obesity Rate (2022)",
#          subtitle = paste0('Model ', mod_index)) +
#     theme_minimal() +
#     facet_wrap(~Sex)
#   mod_index = mod_index + 1
#   
#   # Define the file path for saving
#   file_path <- file.path(output_dir, paste0("pred_vs_actual_", name, ".png"))
#   
#   # Save the plot
#   ggsave(filename = file_path, plot = p,
#          width = 10, height = 6, dpi = 300,
#          device = "png", bg = "white")
#   
#   message("Saved plot: ", file_path)
# }
