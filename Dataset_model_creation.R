# Preliminaries --------------------------------------------
library(readxl)
library(tidyverse)
library(GGally)
library(ggcorrplot)

min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

de_min_max_normalize = function(x_norm, x_max, x_min){
  x = x_norm * (x_max-x_min) + x_min
  return(x)
}

setwd("C:\\Users\\lucia\\Desktop\\STBetaBayes_Obesity_Italy\\data_and_models")

df = read_csv("panel_regions.csv")
df_m_f = read_csv("panel_regions_by_sex.csv")

df = df %>% mutate(Geo_zones = fct_recode(Geo_zones, "North West" = "Nord Ovest",
                                          "North East" = "Nord Est", 
                                          "Centre" = "Centro", 
                                          "South" = "Sud", 
                                          "Islands" = "Isole"))
df_m_f = df_m_f %>% mutate(Geo_zones = fct_recode(Geo_zones, 
                                                  "North West" = "Nord Ovest",
                                                  "North East" = "Nord Est", 
                                                  "Centre" = "Centro", 
                                                  "South" = "Sud", 
                                                  "Islands" = "Isole"))

df_m_f$Sex = as.factor(df_m_f$Sex)

n_years = df %>% count(Year) %>% nrow()
n_geo_zones = df %>% count(Geo_zones) %>% nrow()
n_regions = df %>% count(Region_name) %>% nrow()

years = as.factor(df$Year) %>% levels()
region_names = as.factor(df$Region_name) %>% levels

vars = colnames(df_m_f)
initial_vars = c(vars[1:3],vars[5])
fix_vars = c(initial_vars,vars[47])
mortalities_vars = vars[16:24]
vars_names = vars


df_m_f$Medical_specialists_perc
df_m_f$Municipal_waste_generated %>% hist()
df_m_f %>% ggplot()+
  geom_point(aes(Medical_specialists_perc, Obesity))

# removing Ageing index, Air_quality_PM2.5_perc, Separate_collection_municipal_waste_perc,
# Municipal_waste_generated, Early_childhood_services_perc, Medical_specialists_perc,
# Urban_green
# and AtLeast_1stGr_2ndry_school_perc, More_university_perc (because including them later)
# and Avg_annual_wage (bc having gross income)
# and Geo_zones_Weekly_alcol_perc, Geo_zones_beer05more_daily_perc, Geo_zones_wine05more_daily_perc
vars_to_exclude = c('Ageing_index', 'Air_quality_PM2.5_perc', 
                    'Separate_collection_municipal_waste_perc',
                    'Municipal_waste_generated', 
                    'Early_childhood_services_perc', 
                    'Medical_specialists_perc',
                    'Urban_green',
                    'AtLeast_1stGr_2ndry_school_perc',
                    'More_university_perc',
                    'Avg_annual_wage',
                    'Geo_zones_Weekly_alcol_perc',
                    'Geo_zones_beer05more_daily_perc',
                    'Geo_zones_wine05more_daily_perc')
#df_model = df_m_f %>% mutate(Obesity = Obesity/100, Population = Population/100000)
vars_to_include = c(vars[c(2:5)], "Overweight", "Overweight_minor_age",
                    'Population', 'More_65', 'Foreign_perc', 'Foreign_ue_perc',
                    'unemployment_perc',
                    'Cigarettes_perc', 'Complete_breakfast_perc',
                    'Daily_cheese_perc',
                    'Daily_vegetables_perc',"Dinner_principal_meal_perc",
                    "Red_meat_more_weekly_perc","Fish_more_weekly_perc",
                    "No_sport_perc",
                    "Regular_sport_perc","Smoking_perc","Adequate_nutrition_perc",
                    "Alcohol_cons_perc","Life_satisfaction_perc","Sex")
df_model_all <- df_m_f %>% select(-all_of(c(vars_names[1],vars_to_exclude)))%>% 
  rename('n_cigarettes' = 'Cigarettes_perc')

colnames(df_model_all)
geo = as.integer(factor(df_model_all$Geo_zones))
sex = as.integer(factor(df_model_all$Sex))
years = as.integer(factor(df_model_all$Year))
regions = df_model_all$Region_name %>% unique()
n_years = length(years %>% unique())
n_reg = length(df_model_all$Region_name %>% unique())
n_sex = 2
reg_id <- rep(1:n_reg, each = n_years)

reg_names_zones = c('NO-Piemonte',"NO-Valle d'Aosta",'NO-Lombardia','NO-Liguria','NO-Emilia-Romagna',
                    'NE-Trentino-Alto Adige','NE-Veneto','NE-Friuli-Venezia Giulia',
                    'C-Toscana','C-Umbria','C-Marche','C-Lazio',
                    'S-Abruzzo','S-Molise','S-Campania','S-Puglia','S-Basilicata','S-Calabria',
                    'SI-Sicilia','SI-Sardegna')
years_20xx = seq(2010,2022, 1)



# Normalizing population --------------------------------------------------

# Population weights (by sex) ------------------------------------------------------
pop = read_excel("dati\\Pop_eurostat.xlsx", 
                 sheet = "Sheet 1")

trento_pop = pop %>% filter(Region_name == 'Provincia Autonoma di Trento')
bolzano_pop = pop %>% filter(Region_name == 'Provincia Autonoma di Bolzano/Bozen')

trento_m_weight = trento_pop$Male/(trento_pop$Male + bolzano_pop$Male)
bolzano_m_weight = bolzano_pop$Male/(trento_pop$Male + bolzano_pop$Male)

trento_f_weight = trento_pop$Female/(trento_pop$Female + bolzano_pop$Female)
bolzano_f_weight = bolzano_pop$Female/(trento_pop$Female + bolzano_pop$Female)

# Population weights (total) ----------------------------------------------
pop = pop %>% mutate(Tot = Male+Female)
trento_pop_tot = pop %>% filter(Region_name == 'Provincia Autonoma di Trento') %>% select(Tot)
bolzano_pop_tot = pop %>% filter(Region_name == 'Provincia Autonoma di Bolzano/Bozen') %>% select(Tot)

trento_weight_tot = trento_pop_tot/(trento_pop_tot+bolzano_pop_tot)
bolzano_weight_tot = bolzano_pop_tot/(trento_pop_tot+bolzano_pop_tot)

# Education ---------------------------------------------------------------
edu = read_excel("dati\\education_5_8.xlsx", 
                 sheet = "Sheet 1")
edu = edu[1:273,]

edu %>% filter(Region_name == 'Provincia Autonoma di Trento'|
                 Region_name == 'Provincia Autonoma di Bolzano/Bozen' ) %>% 
  pivot_longer(cols = c(Male, Female),
               names_to = "Gender",
               values_to = "Percentage") %>% 
  ggplot(aes(x = as.integer(Year), y = Percentage))+
  geom_line(aes(col = factor(Region_name)))+
  facet_wrap(~factor(Gender))

trento_m_edu = edu %>% filter(Region_name == 'Provincia Autonoma di Trento') %>% 
  select(Male)

trento_f_edu = edu %>% filter(Region_name == 'Provincia Autonoma di Trento') %>% 
  select(Female)

bolzano_m_edu = edu %>% filter(Region_name == 'Provincia Autonoma di Bolzano/Bozen') %>% 
  select(Male)

bolzano_f_edu = edu %>% filter(Region_name == 'Provincia Autonoma di Bolzano/Bozen') %>% 
  select(Female)

trento_bolzano_m_weghted_mean = trento_m_edu*trento_m_weight + bolzano_m_edu*bolzano_m_weight
trento_bolzano_f_weghted_mean = trento_f_edu*trento_f_weight + bolzano_f_edu*bolzano_f_weight

trento_bolzano_edu = tibble(Region_name = rep('Trentino-Alto Adige', 13),
                            Year = seq(2010,2022,1),
                            trento_bolzano_m_weghted_mean,
                            trento_bolzano_f_weghted_mean)

edu$Year = as.double(edu$Year)

edu = bind_rows(edu, trento_bolzano_edu)

edu %>% filter(Region_name == 'Provincia Autonoma di Trento'|
                 Region_name == 'Provincia Autonoma di Bolzano/Bozen'|
                 Region_name == 'Trentino-Alto Adige') %>% 
  pivot_longer(cols = c(Male, Female),
               names_to = "Gender",
               values_to = "Percentage") %>% 
  ggplot(aes(x = Year, y = Percentage))+
  geom_line(aes(col = factor(Region_name)))+
  facet_wrap(~factor(Gender))

edu_to_join = edu %>% 
  filter(!Region_name %in% c('Provincia Autonoma di Trento',
                             'Provincia Autonoma di Bolzano/Bozen'))

edu_to_join = edu_to_join %>% pivot_longer(cols = c(Male, Female),
                                           names_to = "Sex",
                                           values_to = "Education_5_more")

edu_to_join = edu_to_join %>% 
  mutate(Region_name = ifelse(Region_name == "Valle d’Aosta/Vallée d’Aoste",
                              "Valle d'Aosta", Region_name))

df_model_all = inner_join(df_model_all, edu_to_join, by = c('Region_name', 'Year', 'Sex'))
df_model_all %>% colnames()



# Gross Income  -----------------------------
gi = read_excel("dati\\gross_income_eurostat.xlsx", 
                sheet = "Sheet 1")
gi = gi[1:273,] 
gi$Gross_income = as.double(gi$Gross_income)

gi
trento_weight_tot
bolzano_weight_tot

trento_gi = gi %>% filter(Region_name == 'Provincia Autonoma di Trento') %>% 
  select(Gross_income)

bolzano_gi = gi %>% filter(Region_name == 'Provincia Autonoma di Bolzano/Bozen') %>% 
  select(Gross_income)

trento_bolzano_weghted_mean_gi = trento_gi*trento_weight_tot + bolzano_gi*bolzano_weight_tot

trento_bolzano_gi = tibble(Region_name = rep('Trentino-Alto Adige', 13),
                           Year = seq(2010,2022,1),
                           trento_bolzano_weghted_mean_gi)

gi$Year = as.double(gi$Year)

gi = bind_rows(gi, trento_bolzano_gi)

gi %>% filter(Region_name == 'Provincia Autonoma di Trento'|
                Region_name == 'Provincia Autonoma di Bolzano/Bozen'|
                Region_name == 'Trentino-Alto Adige') %>% 
  ggplot(aes(x = Year, y = Gross_income))+
  geom_line(aes(col = factor(Region_name)))

gi_to_join = gi %>% 
  filter(!Region_name %in% c('Provincia Autonoma di Trento',
                             'Provincia Autonoma di Bolzano/Bozen'))

gi_to_join = gi_to_join %>% 
  mutate(Region_name = ifelse(Region_name == "Valle d’Aosta/Vallée d’Aoste",
                              "Valle d'Aosta", Region_name))
gi_to_join = rbind(gi_to_join,gi_to_join)
gi_to_join = gi_to_join %>% mutate(ID = seq_along(Gross_income))

gi_to_join = gi_to_join %>% mutate(Sex = as.factor(ifelse(ID <= 260, 'Male', 'Female'))) %>% 
  select(-ID)

df_model_all = inner_join(df_model_all, gi_to_join, by = c('Region_name', 'Year', 'Sex'))


# some column cleaning ---------
df_model_all = df_model_all %>% select(!c(Geo_zones, family_health_vs_gdp_expend_perc,
                                          health_vs_gdp_expend_perc))
vars_final_all = colnames(df_model_all)
vars_final_all

# Overweight subtraction --------------------------------------------------
df_model_all = df_model_all %>% mutate(Overweight = Overweight-Obesity)

# Regional health expenditure -------------------------------------
reg_health_exp_df = read_excel("dati\\Spesa_salute_regioni.xlsx")
reg_health_exp_df = reg_health_exp_df %>% pivot_longer(-Region_name,
                                                       names_to = 'Year',
                                                       values_to = 'health_expenditure')
reg_health_exp_df$Year = as.double(reg_health_exp_df$Year)
# making expenditure for Trentino-Alto Adige
reg_health_exp_df$Region_name %>% unique()

trentinoaltoadige_exp = ((reg_health_exp_df %>% filter(Region_name == 'Provincia autonoma di Trento') %>% 
                            select(health_expenditure))*trento_weight_tot +
                           (reg_health_exp_df %>% filter(Region_name == 'Provincia autonoma di Bolzano')%>% 
                              select(health_expenditure))*bolzano_weight_tot)

trento_bolzano_exp_df = tibble(Region_name = rep('Trentino-Alto Adige', 13),
                               Year = seq(2010,2022,1),
                               trentinoaltoadige_exp)

reg_health_exp_df = bind_rows(reg_health_exp_df, trento_bolzano_exp_df)

reg_health_exp_df %>% filter(Region_name == 'Provincia autonoma di Trento'|
                               Region_name == 'Provincia autonoma di Bolzano'|
                               Region_name == 'Trentino-Alto Adige') %>% 
  ggplot(aes(x = Year, y = health_expenditure))+
  geom_line(aes(col = factor(Region_name)))

reg_health_exp_df = reg_health_exp_df %>% 
  filter(!Region_name %in% c('Provincia autonoma di Trento','Provincia autonoma di Bolzano'))

reg_health_exp_df 

reg_health_exp_df = reg_health_exp_df %>% 
  mutate(Region_name = ifelse(Region_name == 'Friuli Venezia Giulia',
                              'Friuli-Venezia Giulia', 
                              ifelse(Region_name == 'Emilia Romagna',
                                     'Emilia-Romagna', Region_name)))
desired_order <- unique(df_model_all$Region_name)
desired_order
unique(reg_health_exp_df$Region_name)

reg_health_exp_df <- reg_health_exp_df %>%
  mutate(Region_name = factor(Region_name, levels = desired_order)) %>%
  arrange(Region_name)

reg_health_exp_df = bind_rows(reg_health_exp_df, reg_health_exp_df)
reg_health_exp_df$Sex = df_model_all$Sex

df_model_all = inner_join(df_model_all, reg_health_exp_df, by = c('Region_name', 'Year', 'Sex'))

df_model_all = df_model_all %>% mutate(health_expenditure_by_pop = 
         (health_expenditure / Population))

df_model_all = df_model_all %>% select(!health_expenditure)

df_model_all %>% filter(Sex == 'Male') %>% 
  mutate(health_expenditure_by_pop_std = 
           (health_expenditure_by_pop - mean(health_expenditure_by_pop)) / sd(health_expenditure_by_pop)) %>% 
  ggplot()+
  geom_line(aes(x = Year, y = health_expenditure_by_pop_std))+
  facet_wrap(~factor(Region_name))+
  ggtitle('Health expenditure')

colnames(df_model_all)


# saving final dataset --------------------------------------------------
# save(df_model_all, file='df_model_all')


# X_til and y_til creation ----------------------------------------------

## DATA IN ARRAY FORM, if want to, but not used in model ----------------
sex = as.integer(factor(df_model_all$Sex))
years = as.integer(factor(df_model_all$Year))
regions = df_model_all$Region_name %>% unique()
n_years = length(years %>% unique())
n_reg = length(df_model_all$Region_name %>% unique())
n_sex = 2
reg_id <- rep(1:n_reg, each = n_years)

#males
df_model_all_m = df_model_all %>% filter(Sex == 'Male')
df_model_all_f = df_model_all %>% filter(Sex == 'Female')
# Obesity
y_m <-  df_model_all_m$Obesity/100
y_f <-  df_model_all_f$Obesity/100

# exogenous covariates m
X_m = df_model_all_m %>% select(!c(Obesity, Region_name, Year, Sex))
X_m_cols = colnames(X_m)
# exogenous covariates f
X_f = df_model_all_f %>% select(!c(Obesity, Region_name, Year, Sex))
X_f_cols = colnames(X_f)

X_f_cols == X_m_cols

# p number of covariates
p = length(X_f_cols)

# transforming into N x T
Y_m = structure(.Data = y_m, .Dim = c(n_years, n_reg))
Y_m = t(Y_m)
# transforming into N x T
Y_f = structure(.Data = y_f, .Dim = c(n_years, n_reg))
Y_f = t(Y_f)

# same for exog covariates
transform_vectors_into_NxT_matrices <- function(data, n_years, n_regions) {
  # Convert data to matrix
  matrix <- as.matrix(data)
  
  # Initialize a list to store vectors
  matrices <- list()
  
  # Loop through columns (covariates)
  for (i in 1:ncol(matrix)) {
    # Extract the vector for each covariate
    covariate_vector <- matrix[, i]
    # Structure the vector as T X N
    TxN_matrix <- structure(.Data = covariate_vector, .Dim = c(n_years, n_regions))
    # Transpose the vector to make it N X T
    NxT_matrix <- t(TxN_matrix)
    # Store the transposed vector in the list
    matrices[[i]] <- NxT_matrix
  }
  
  return(matrices)
}

X_m <- transform_vectors_into_NxT_matrices(X_m,
                                           n_years, n_reg)

X_f <- transform_vectors_into_NxT_matrices(X_f,
                                           n_years, n_reg)

X_f
X_m

# make a unique big array: 20x13, 
# for each of the 25 included variables, 
# for each of the two sex (female indexed by 1, male indexed by 2)
array_f <- array(unlist(X_f), dim = c(20, 13, p))
array_m <- array(unlist(X_m), dim = c(20, 13, p))

# Combine the arrays into a single multidimensional array
X <- array(c(array_f, array_m), dim = c(20, 13, p, 2))
dim(X)

X[1,1,1,]

# Y
Y = array(0, dim = c(20, 13, 2))
Y[,,1] = Y_f
Y[,,2] = Y_m

# save(Y, file = 'Y')
# save(X, file = 'X')
# save(X_f_cols, file = 'Exogenous_vars_names_X')

## DATA IN LONGITUDINAL FORM, the one used in model ------------------
y_til = vector(length = 2*n_reg*n_years)
X_til = matrix(nrow = 2*n_reg*n_years, ncol = p+1+2)

sogg = 0
for(s in 1:n_sex){
  for(i in 1:n_reg){
    for(t in 1:n_years){
      sogg = sogg+1
      y_til[sogg] = Y[i,t,s]
      X_til[sogg,] = c(X[i,t,1:p,s], s-1, i, t)
    }
  }
}

colnames(X_til) = c(X_f_cols, 'sex', 'space', 'time')

# getting na's ----------------------------------
X_til
nrow(X_til)
X_til[,1][1:260] %>% mean() # females
X_til[,1][261:520] %>% mean() # males
NA_indexes = which(is.na(X_til), arr.ind = TRUE) %>% as_tibble()
NA_cols = NA_indexes$col %>% unique()
NA_rows = NA_indexes$row %>% unique()
X_til[NA_rows,NA_cols]
X_til[,NA_cols]%>% head(15)

X_f_cols[NA_cols] 

# plotting mortality rates variables over time ---------------------------
# for(i in 1:length(X_f_cols[NA_cols])){
#   p = X_til %>% as_tibble() %>% select(all_of(NA_cols),space,time, sex) %>% 
#     pivot_longer(cols = -c(space, time, sex), 
#                  names_to = "desease", 
#                  values_to = "value") %>% 
#     filter(desease == X_f_cols[NA_cols][i]) %>% 
#     ggplot()+
#     geom_line(aes(x = time, y = value, col = factor(sex)))+
#     facet_wrap(~factor(space))+
#     ggtitle(paste0('Variable: ', X_f_cols[NA_cols][i]))
#   print(p)
#   cat("Press enter to continue to the next plot (or type 'quit' to exit): ")
#   input <- readline()
#   if (tolower(input) == "quit") {
#     break
#   }
# }

# LOCF imputation ------------------------------
library(zoo)
# Convert the matrix to a data frame for easier manipulation
X_til_df <- as.data.frame(X_til)
# Apply LOCF imputation
X_til_df_LOCF <- as.data.frame(lapply(X_til_df, function(x) na.locf(x, na.rm = FALSE)))

# converting to matrix array the X_til_df_LOCF dataframe
class(X_til)
class(X_til_df_LOCF)

X_til_LOCF = as.matrix(X_til_df_LOCF)
class(X_til_LOCF)

X_til_LOCF[,NA_cols]%>% head(15)

rows_list <- apply(X_til_LOCF, 1, as.list)
duplicated_rows <- duplicated(rows_list)
rows_with_duplicates <- which(duplicated_rows)

cols_list <- apply(X_til_LOCF, 2, as.list)
duplicated_cols <- duplicated(cols_list)
cols_with_duplicates <- which(duplicated_cols)

list(
  duplicated_rows = rows_with_duplicates,
  duplicated_columns = cols_with_duplicates
)

# LOCF scaling -----------------------------------------------
X_til_LOCF[,1:p] = scale(X_til_LOCF[,1:p])
head(X_til_LOCF)
# verify proper scaling
apply(X_til_LOCF, 2, mean) %>% round(4)   # = 0
apply(X_til_LOCF, 2, sd) %>% round(4)     # = 1

# (X^t X)
X_prova = t(X_til_LOCF[,1:(p+1)]) %*% X_til_LOCF[,1:(p+1)]
dim(X_prova)
# check if na's
na_indices <- which(is.na(X_prova), arr.ind = TRUE)
if (length(na_indices) > 0) {
  print("NA values found at the following indices:")
  print(na_indices)
} else {
  print("No NA values found.")
}
# Check the rank
rank_X_prova <- qr(X_prova)$rank
cat("Rank of X_prova: ", rank_X_prova, "\n")
cat("Dimensions of X_prova: ", dim(X_prova), "\n")


# multicolllinearity --------------------------------------------
# Compute correlation matrix
cor_matrix <- cor(X_til_LOCF[, 1:(p+1)])
# Find pairs of columns with high correlation
high_cor_pairs <- which(abs(cor_matrix) > 0.8, arr.ind = TRUE)
high_cor_pairs <- high_cor_pairs[high_cor_pairs[, 1] < high_cor_pairs[, 2], ]

if (nrow(high_cor_pairs) > 0) {
  cat("Highly correlated column pairs:\n")
  print(high_cor_pairs)
} else {
  cat("No highly correlated columns found.\n")
}

# some checks
colnames(X_prova)[7]
X_til %>% ggplot()+
  geom_point(aes(Overweight, More_65_alone))
X_til %>% ggplot()+
  geom_point(aes(Overweight, mortality_cancer_digestive_perc))
X_til %>% ggplot()+
  geom_boxplot(aes(Overweight, y = factor(sex)))

# given that most correlation seems to be given by sex now subsetting
mask = (X_til_LOCF[,41] == 0)
X_til_LOCF_f = X_til_LOCF[mask,]
X_til_LOCF_m = X_til_LOCF[!mask,]

# recalculating high corr: 
# corr by sex
cor_matrix_f <- cor(X_til_LOCF_f[, 1:p])
cor_matrix_m <- cor(X_til_LOCF_m[, 1:p])
# Find pairs of columns with high correlation
high_cor_pairs_f <- which(abs(cor_matrix_f) > 0.8, arr.ind = TRUE)
high_cor_pairs_m <- which(abs(cor_matrix_m) > 0.8, arr.ind = TRUE)

high_cor_pairs_f <- high_cor_pairs_f[high_cor_pairs_f[, 1] < high_cor_pairs_f[, 2], ]
high_cor_pairs_m <- high_cor_pairs_m[high_cor_pairs_m[, 1] < high_cor_pairs_m[, 2], ]

high_cor_pairs_f
high_cor_pairs_m
# for males a lot less variables are nearly collinear!

# males checks
high_cor_pairs_m
colnames(X_prova)[13]
X_til_LOCF_m %>% ggplot()+
  geom_point(aes(unemployment_perc, No_sport_perc))
X_til_LOCF_m %>% ggplot()+
  geom_point(aes(mortality_diabetes_perc, No_sport_perc))
X_til_LOCF_m %>% ggplot()+
  geom_point(aes(No_sport_perc, Regular_sport_perc))
X_til_LOCF_m %>% ggplot()+
  geom_point(aes(Daily_vegetables_perc, Adequate_nutrition_perc))
X_til_LOCF_m %>% ggplot()+
  geom_point(aes(unemployment_perc, No_sport_perc))
X_til_LOCF_m %>% ggplot()+
  geom_point(aes(No_sport_perc, Gross_income))
X_til_LOCF_m %>% ggplot()+
  geom_point(aes(Regular_sport_perc, Gross_income))

# females checks
high_cor_pairs_f
colnames(X_prova)[33]
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(unemployment_perc, mortality_diabetes_perc))
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(mortality_diabetes_perc, mortality_blood_perc))
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(Foreign_perc, Daily_vegetables_perc))
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(unemployment_perc, No_sport_perc))
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(mortality_diabetes_perc, No_sport_perc))
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(No_sport_perc, Regular_sport_perc))
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(Daily_vegetables_perc, Adequate_nutrition_perc))
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(unemployment_perc, Gross_income))
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(No_sport_perc, Gross_income))
X_til_LOCF_f %>% ggplot()+
  geom_point(aes(Regular_sport_perc, Gross_income))


# saving all data ---------------------------------------------------------
NA_cols # go to # "getting na's ---" section to see: these are mortality cols
X_til
X_til_LOCF
y_til

# save(X_til, file='X_til')
# save(X_til_LOCF, file='X_til_LOCF')
# save(y_til, file='y_til')
# save(NA_cols, file='NA_cols')


# PCA for mortalities -----------------------------------------------------

# load('X_til')
# load('X_til_LOCF')
# load('y_til')
# load('NA_cols')
# 
# # load('X_all_covariates_in_model') # see under # ONLY EXOGENOUS COVARIATES-- section,
# # and under DATA & CONSTANTS-- section and under X
# load('df_model_all')

colnames(X_til)

# reg_names <- c('Piemonte', "Valle d'Aosta", 'Lombardia', 'Liguria', 'Emilia-Romagna',
#                'Trentino-Alto Adige', 'Veneto', 'Friuli-Venezia Giulia',
#                'Toscana', 'Umbria', 'Marche', 'Lazio',
#                'Abruzzo', 'Molise', 'Campania', 'Puglia', 'Basilicata', 'Calabria',
#                'Sicilia', 'Sardegna')
# 
# reg_names_zones = c('NO-Piemonte',"NO-Valle d'Aosta",'NO-Lombardia','NO-Liguria','NO-Emilia-Romagna',
#                     'NE-Trentino-Alto Adige','NE-Veneto','NE-Friuli-Venezia Giulia',
#                     'C-Toscana','C-Umbria','C-Marche','C-Lazio',
#                     'S-Abruzzo','S-Molise','S-Campania','S-Puglia','S-Basilicata','S-Calabria',
#                     'SI-Sicilia','SI-Sardegna')

p = ncol(X_til) - 3 # - space and time and sex
N = 2*n_reg*n_years

NA_cols
# Correlation matrix
check_corr_mat = cbind(y_til, X_til_LOCF[,NA_cols])
check_corr_mat = cor(check_corr_mat)
corrplot::corrplot(check_corr_mat, method = "color", type = "upper",
                   tl.col = "black", tl.srt = 45)
ggcorrplot(check_corr_mat,
           method = "square",
           type = "upper",
           lab = TRUE)

# Find pairs of columns with high correlation
high_cor_pairs <- which(abs(check_corr_mat) > 0.8, arr.ind = TRUE)
high_cor_pairs <- high_cor_pairs[high_cor_pairs[, 1] < high_cor_pairs[, 2], ]
high_cor_pairs

colnames(X_til_LOCF[,NA_cols])

# one example 
X_til_LOCF[,NA_cols] %>% as_tibble() %>% ggplot()+
  geom_point(aes(mortality_blood_perc,mortality_cancer_stomach_perc))

# pca mortalities ----------------------------------------
# keeping the first 2 pcs (PC1: obesity deseases, PC2 mental deseases + somatization)
pca_mortalities <- prcomp(X_til_LOCF[,NA_cols], scale. = TRUE)
pca_mortalities
pcs_mortalities <- pca_mortalities$x

# ggpairs(X_til_LOCF[,NA_cols])

# variance explained
pve <- pca_mortalities$sdev^2 / sum(pca_mortalities$sdev^2)
# Create a data frame for plotting
pve_df <- data.frame(Principal_Component = seq_along(pve), Variance_Explained = pve)
# Create the scree plot
ggplot(pve_df, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(y = cumsum(Variance_Explained)), color = "red", size = 1) +
  geom_point(aes(y = cumsum(Variance_Explained)), color = "red", size = 2) +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained") +
  theme_minimal()

X_temp = cbind(X_til_LOCF[,-NA_cols], pcs_mortalities[,c(1,2)])

X_temp %>% as_tibble() %>% ggplot()+
  geom_point(aes(PC1, PC2, col = factor(sex)))

# X
X_temp = cbind(X_til_LOCF[,-NA_cols], pcs_mortalities[,c(1,2)])
(X_temp %>% colnames())

X_temp[,-c(17,32,33,34)] %>% colnames() # removing Regular_sport_perc, sex, space, time
X = X_temp[,-c(17,32,33,34)]
colnames(X)

# save(X, file = 'X_all_covariates_in_model')
