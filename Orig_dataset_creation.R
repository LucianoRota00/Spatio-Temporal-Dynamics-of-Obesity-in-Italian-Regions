library(tidyverse)
library(readxl)

setwd("C:\\Users\\lucia\\Desktop\\STBetaBayes_Obesity_Italy\\data_and_models\\dati")

create_panel = function(df_input, var_name, joining = F, df_to_enrich = NULL){
  df_input = read_excel(df_input)
  df_input = df_input %>% filter(Aree < 21)
  df_input = df_input %>% rename(Region_name = "...2")
  df_input = df_input %>% rename(Area = "Aree")
  df_input = df_input %>% 
      pivot_longer(cols = -c(Area, Region_name), 
                   names_to = "Year") 
  df_out = df_input %>% mutate({{var_name}} := value) %>% select(!value)
  if(joining == T){
    return(df_to_enrich %>% inner_join(df_out))
  }else{
    return(df_out)
    }
}

create_panel_macro_zones = function(df_input, var_name, left_joining = F, df_to_enrich = NULL){
  df_input = read_excel(df_input)
  df_input = df_input %>% filter(Aree > 21 & Aree < 29)
  df_input = df_input %>% rename(Geo_zones = "...2")
  df_input = df_input %>% rename(Macro_area = "Aree")
  df_input = df_input %>% 
    pivot_longer(cols = -c(Macro_area, Geo_zones), 
                 names_to = "Year") 
  df_out = df_input %>% mutate({{var_name}} := value) %>% select(!c(value,Macro_area))
  if(left_joining == T){
    return(df_to_enrich %>% left_join(df_out, by=c("Geo_zones","Year")))
  }else{
    return(df_out)
  }
}

# Y

df = create_panel("Y.xlsx", "Obesity")

# Geo_zones
df = df %>% mutate(Geo_zones = ifelse(
    Region_name == "Piemonte"|
    Region_name == "Lombardia"|
    Region_name == "Liguria"|
    Region_name == "Valle d'Aosta",
    "Nord Ovest",
  ifelse(
    Region_name == "Trentino-Alto Adige"|
    Region_name == "Friuli-Venezia Giulia"|
    Region_name == "Veneto"|
    Region_name == "Emilia-Romagna",
    "Nord Est",
  ifelse(
    Region_name == "Toscana"|
    Region_name == "Umbria"|
    Region_name == "Lazio"|
    Region_name == "Marche",
    "Centro",
  ifelse(
    Region_name == "Abruzzo"|
    Region_name == "Molise"|
    Region_name == "Campania"|
    Region_name == "Puglia"|
    Region_name == "Basilicata"|
    Region_name == "Calabria",
    "Sud",
    "Isole"
  )
  )
  )
))

df = create_panel("Ym.xlsx", "Obesity_m", joining = T, df_to_enrich = df)
df = create_panel("Yf.xlsx", "Obesity_f", joining = T, df_to_enrich = df)

df = create_panel("sovrappeso_perc.xlsx", "Overweight", joining = T, df_to_enrich = df)
df = create_panel("sovrappeso_perc_m.xlsx", "Overweight_m", joining = T, df_to_enrich = df)
df = create_panel("sovrappeso_perc_f.xlsx", "Overweight_f", joining = T, df_to_enrich = df)

df = create_panel("sovrappeso_minori_6_17.xlsx", "Overweight_minor_age", joining = T, df_to_enrich = df)
df = create_panel("sovrappeso_minori_6_17_m.xlsx", "Overweight_minor_age_m", joining = T, df_to_enrich = df)
df = create_panel("sovrappeso_minori_6_17_f.xlsx", "Overweight_minor_age_f", joining = T, df_to_enrich = df)

# X
#01 - contesto socio-demografico
df = create_panel("Pop.xlsx", "Population", joining = T, df_to_enrich = df)
df = create_panel("Pop_m.xlsx", "Population_m", joining = T, df_to_enrich = df)
df = create_panel("Pop_f.xlsx", "Population_f", joining = T, df_to_enrich = df)
df = create_panel("Indice_invecchiamento.xlsx", "Ageing_index", joining = T, df_to_enrich = df)
df = create_panel("perc_pop_resid_65oltre.xlsx", "More_65", joining = T, df_to_enrich = df)
df = create_panel("perc_pop_resid_65oltre_m.xlsx", "More_65_m", joining = T, df_to_enrich = df)
df = create_panel("perc_pop_resid_65oltre_f.xlsx", "More_65_f", joining = T, df_to_enrich = df)
df = create_panel("perc_stranieri_residenti.xlsx", "Foreign_perc", joining = T, df_to_enrich = df)
df = create_panel("perc_stranieri_residenti_m.xlsx", "Foreign_perc_m", joining = T, df_to_enrich = df)
df = create_panel("perc_stranieri_residenti_f.xlsx", "Foreign_perc_f", joining = T, df_to_enrich = df)
df = create_panel("perc_stranieri_ue.xlsx", "Foreign_ue_perc", joining = T, df_to_enrich = df)
df = create_panel("perc_stranieri_ue_m.xlsx", "Foreign_ue_perc_m", joining = T, df_to_enrich = df)
df = create_panel("perc_stranieri_ue_f.xlsx", "Foreign_ue_perc_f", joining = T, df_to_enrich = df)
df = create_panel("perc_65oltre_soli.xlsx", "More_65_alone", joining = T, df_to_enrich = df)
df = create_panel("perc_65oltre_soli_m.xlsx", "More_65_alone_m", joining = T, df_to_enrich = df)
df = create_panel("perc_65oltre_soli_f.xlsx", "More_65_alone_f", joining = T, df_to_enrich = df)
# df = create_panel("Stranieri.xlsx", "Foreigners", joining = T, df_to_enrich = df)
# df = create_panel("Stranieri_m.xlsx", "Foreigners_m", joining = T, df_to_enrich = df)
# df = create_panel("Stranieri_f.xlsx", "Foreigners_f", joining = T, df_to_enrich = df)
df = create_panel("Numero_medio_componenti_famiglia.xlsx", "Mean_family_components", joining = T, df_to_enrich = df)
df = create_panel("Almeno_scuole_medie.xlsx", "AtLeast_1stGr_2ndry_school_perc", joining = T, df_to_enrich = df)
df = create_panel("Almeno_scuole_medie_m.xlsx", "AtLeast_1stGr_2ndry_school_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Almeno_scuole_medie_f.xlsx", "AtLeast_1stGr_2ndry_school_perc_f", joining = T, df_to_enrich = df)
df = create_panel("Università_o_più.xlsx", "More_university_perc", joining = T, df_to_enrich = df)
df = create_panel("Università_o_piu_m.xlsx", "More_university_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Università_o_piu_f.xlsx", "More_university_perc_f", joining = T, df_to_enrich = df)
df = create_panel("tasso_nuzialità.xlsx", "wedding_perc", joining = T, df_to_enrich = df)
df = create_panel("Tasso_disoccupazione.xlsx", "unemployment_perc", joining = T, df_to_enrich = df)
df = create_panel("Tasso_disoccupazione_m.xlsx", "unemployment_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Tasso_disoccupazione_f.xlsx", "unemployment_perc_f", joining = T, df_to_enrich = df)

#02 - mortalità
df = create_panel("tasso_mortalità_tumori_apparato_digerente.xlsx", "mortality_cancer_digestive_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_tumori_apparato_digerente_m.xlsx", "mortality_cancer_digestive_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_tumori_apparato_digerente_f.xlsx", "mortality_cancer_digestive_perc_f", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_tumori_stomaco.xlsx", "mortality_cancer_stomach_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_tumori_stomaco_m.xlsx", "mortality_cancer_stomach_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_tumori_stomaco_f.xlsx", "mortality_cancer_stomach_perc_f", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_diabete_mellito.xlsx", "mortality_diabetes_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_diabete_mellito_m.xlsx", "mortality_diabetes_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_diabete_mellito_f.xlsx", "mortality_diabetes_perc_f", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_disturbi_psichici.xlsx", "mortality_mental_disorders_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_disturbi_psichici_m.xlsx", "mortality_mental_disorders_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_disturbi_psichici_f.xlsx", "mortality_mental_disorders_perc_f", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_sistema_circolatorio.xlsx", "mortality_blood_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_sistema_circolatorio_m.xlsx", "mortality_blood_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_sistema_circolatorio_f.xlsx", "mortality_blood_perc_f", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_ischemiche_cuore.xlsx", "mortality_heart_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_ischemiche_cuore_m.xlsx", "mortality_heart_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_ischemiche_cuore_f.xlsx", "mortality_heart_perc_f", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_apparato_digerente.xlsx", "mortality_digestive_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_apparato_digerente_m.xlsx", "mortality_digestive_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_apparato_digerente_f.xlsx", "mortality_digestive_perc_f", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_fegato.xlsx", "mortality_liver_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_fegato_m.xlsx", "mortality_liver_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_malattie_fegato_f.xlsx", "mortality_liver_perc_f", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_suicidio_autolesione.xlsx", "mortality_suicide_simil_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_suicidio_autolesione_m.xlsx", "mortality_suicide_simil_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_mortalità_suicidio_autolesione_f.xlsx", "mortality_suicide_simil_perc_f", joining = T, df_to_enrich = df)

#03 - stili di vita
df = create_panel("Sigarette.xlsx", "Cigarettes_perc", joining = T, df_to_enrich = df)
df = create_panel("Sigarette_m.xlsx", "Cigarettes_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Sigarette_f.xlsx", "Cigarettes_perc_f", joining = T, df_to_enrich = df)
df = create_panel("Colazione_adeguata.xlsx", "Complete_breakfast_perc", joining = T, df_to_enrich = df)
df = create_panel("Colazione_adeguata_m.xlsx", "Complete_breakfast_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Colazione_adeguata_f.xlsx", "Complete_breakfast_perc_f", joining = T, df_to_enrich = df)
df = create_panel("Formaggio_giornalmente.xlsx", "Daily_cheese_perc", joining = T, df_to_enrich = df)
df = create_panel("Formaggio_giornalmente_m.xlsx", "Daily_cheese_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Formaggio_giornalmente_f.xlsx", "Daily_cheese_perc_f", joining = T, df_to_enrich = df)
df = create_panel("Verdura_giornalmente.xlsx", "Daily_vegetables_perc", joining = T, df_to_enrich = df)
df = create_panel("Verdura_giornalmente_m.xlsx", "Daily_vegetables_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Verdura_giornalmente_f.xlsx", "Daily_vegetables_perc_f", joining = T, df_to_enrich = df)
df = create_panel("Pasto_principale_cena.xlsx", "Dinner_principal_meal_perc", joining = T, df_to_enrich = df)
df = create_panel("Pasto_principale_cena_m.xlsx", "Dinner_principal_meal_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Pasto_principale_cena_f.xlsx", "Dinner_principal_meal_perc_f", joining = T, df_to_enrich = df)

df = create_panel("Carne_bovina_piu_volte_settimana.xlsx", "Red_meat_more_weekly_perc", joining = T, df_to_enrich = df)
df = create_panel("Carne_bovina_piu_volte_settimana_m.xlsx", "Red_meat_more_weekly_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Carne_bovina_piu_volte_settimana_f.xlsx", "Red_meat_more_weekly_perc_f", joining = T, df_to_enrich = df)
df = create_panel("Pesce_piu_volte_settimana.xlsx", "Fish_more_weekly_perc", joining = T, df_to_enrich = df)
df = create_panel("Pesce_piu_volte_settimana_m.xlsx", "Fish_more_weekly_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Pesce_piu_volte_settimana_f.xlsx", "Fish_more_weekly_perc_f", joining = T, df_to_enrich = df)


df = create_panel("No_sport.xlsx", "No_sport_perc", joining = T, df_to_enrich = df)
df = create_panel("No_sport_m.xlsx", "No_sport_perc_m", joining = T, df_to_enrich = df)
df = create_panel("No_sport_f.xlsx", "No_sport_perc_f", joining = T, df_to_enrich = df)
df = create_panel("Sport_abituale.xlsx", "Regular_sport_perc", joining = T, df_to_enrich = df)
df = create_panel("Sport_abituale_m.xlsx", "Regular_sport_perc_m", joining = T, df_to_enrich = df)
df = create_panel("Sport_abituale_f.xlsx", "Regular_sport_perc_f", joining = T, df_to_enrich = df)

df = create_panel_macro_zones("Alcol_abituale.xlsx", "Geo_zones_Weekly_alcol_perc",
                              left_joining = T, df_to_enrich = df)
df = create_panel_macro_zones("Alcol_abituale_m.xlsx", "Geo_zones_Weekly_alcol_perc_m",
                              left_joining = T, df_to_enrich = df)
df = create_panel_macro_zones("Alcol_abituale_f.xlsx", "Geo_zones_Weekly_alcol_perc_f",
                              left_joining = T, df_to_enrich = df)
df = create_panel_macro_zones("Birra_05l_piu_giorno.xlsx", "Geo_zones_beer05more_daily_perc",
                              left_joining = T, df_to_enrich = df)
df = create_panel_macro_zones("Birra_05l_piu_giorno_m.xlsx", "Geo_zones_beer05more_daily_perc_m",
                              left_joining = T, df_to_enrich = df)
df = create_panel_macro_zones("Birra_05l_piu_giorno_f.xlsx", "Geo_zones_beer05more_daily_perc_f",
                              left_joining = T, df_to_enrich = df)
df = create_panel_macro_zones("Vino_05l_piu_giorno.xlsx", "Geo_zones_wine05more_daily_perc",
                              left_joining = T, df_to_enrich = df)
df = create_panel_macro_zones("Vino_05l_piu_giorno_m.xlsx", "Geo_zones_wine05more_daily_perc_m",
                              left_joining = T, df_to_enrich = df)
df = create_panel_macro_zones("Vino_05l_piu_giorno_f.xlsx", "Geo_zones_wine05more_daily_perc_f",
                              left_joining = T, df_to_enrich = df)

# 04+
df = create_panel("tasso_persone_cattiva_salute.xlsx", "bad_wealth_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_persone_cattiva_salute_m.xlsx", "bad_wealth_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_persone_cattiva_salute_f.xlsx", "bad_wealth_perc_f", joining = T, df_to_enrich = df)
df = create_panel("speranza_di_vita_m.xlsx", "life_exp_m", joining = T, df_to_enrich = df)
df = create_panel("speranza_di_vita_f.xlsx", "life_exp_f", joining = T, df_to_enrich = df)

df = df %>% mutate(life_exp = (life_exp_m + life_exp_f)/2)

df = create_panel("speranza_di_vita_buon_salute_m.xlsx", "life_exp_good_wealth_m", joining = T, df_to_enrich = df)
df = create_panel("speranza_di_vita_buon_salute_f.xlsx", "life_exp_good_wealth_f", joining = T, df_to_enrich = df)

df = df %>% mutate(life_exp_good_wealth = (life_exp_good_wealth_m + life_exp_good_wealth_f)/2) 


df = create_panel("speranza_di_vita_no_limitazioni_m.xlsx", "life_exp_no_limitations_m", joining = T, df_to_enrich = df)
df = create_panel("speranza_di_vita_no_limitazioni_f.xlsx", "life_exp_no_limitations_f", joining = T, df_to_enrich = df)

df = df %>% mutate(life_exp_no_limitations = (life_exp_no_limitations_m + life_exp_no_limitations_f)/2) 

df = create_panel("tasso_consumo_farmaci.xlsx", "drug_consumption_perc", joining = T, df_to_enrich = df)
df = create_panel("tasso_consumo_farmaci_m.xlsx", "drug_consumption_perc_m", joining = T, df_to_enrich = df)
df = create_panel("tasso_consumo_farmaci_f.xlsx", "drug_consumption_perc_f", joining = T, df_to_enrich = df)
df = create_panel("perc_spesa_sanitaria_vs_pil.xlsx", "health_vs_gdp_expend_perc", joining = T, df_to_enrich = df)
df = create_panel("perc_spesa_sanitaria_famiglie_vs_pil.xlsx", "family_health_vs_gdp_expend_perc", joining = T, df_to_enrich = df)


# adding bes data ---------------------------------------------------------
df$Year = as.double(df$Year)

df = df %>% left_join(read_excel('bes_urban_green.xlsx'))
df = df %>% left_join(read_excel('bes_wage_all.xlsx'))
df = df %>% left_join(read_excel('bes_medical_specialists.xlsx'))
df = df %>% left_join(read_excel('bes_early_childhood_services.xlsx'))
df = df %>% left_join(read_excel('bes_municipal_waste_generated.xlsx'))
df = df %>% left_join(read_excel('bes_separate_collection_municipal_waste.xlsx'))

df = df %>% left_join(read_excel('bes_smoking_rate_all.xlsx'))
df = df %>% left_join(read_excel('bes_adequate_nutrition_all.xlsx'))
df = df %>% left_join(read_excel('bes_alcohol_all.xlsx'))
df = df %>% left_join(read_excel('bes_air_quality.xlsx'))
df = df %>% left_join(read_excel('bes_life_satisfaction_all.xlsx'))


# Unique dataset for males and females ------------------------------------

# variables, variables only present unisex, variable names without _sex
# common identification variables
variables = colnames(df)
only_unisex_vars = c('Ageing_index',
                     'Mean_family_components', 
                     'wedding_perc', 
                     'health_vs_gdp_expend_perc', 
                     'family_health_vs_gdp_expend_perc',
                     'Urban_green',
                     'Medical_specialists_perc',
                     'Early_childhood_services_perc',
                     'Municipal_waste_generated',
                     'Separate_collection_municipal_waste_perc',
                     'Air_quality_PM2.5_perc')

not_by_sex_vars = variables[!str_detect(variables, "_(m|f)$")]
identification_variables = c(variables[1:3],variables[5])

#sex-variables
sex_vars_m = variables[str_detect(variables, "_(m)$")]
sex_vars_m = c(identification_variables,sex_vars_m)

sex_vars_f = variables[str_detect(variables, "_(f)$")]
sex_vars_f = c(identification_variables,sex_vars_f)

not_in_m = setdiff(not_by_sex_vars, sub("_m$", "", sex_vars_m))
not_in_f = setdiff(not_by_sex_vars, sub("_f$", "", sex_vars_f))

sex_vars_m = c(sex_vars_m, not_in_m)
sex_vars_f = c(sex_vars_f, not_in_f)

# three differenced datasets
df_not_by_sex = df %>% select(all_of(not_by_sex_vars))

df_m = df %>% select(all_of(sex_vars_m))
df_f = df %>% select(all_of(sex_vars_f))

# getting name vars to assign that must be the same
only_unisex_vars_regex = paste(only_unisex_vars,collapse = '|')
var_names_for_binded_df_m_and_f = not_by_sex_vars[!str_detect(
  not_by_sex_vars,only_unisex_vars_regex)]

# removing columns for which there is no sex detail
df_m_no_only_unisex_vars = df_m %>% select(-c(all_of(only_unisex_vars)))
df_f_no_only_unisex_vars = df_f %>% select(-c(all_of(only_unisex_vars)))

# reordering to have obesity after Year like in the orignial dataframe
df_m_no_only_unisex_vars = df_m_no_only_unisex_vars[,
                              c(1,2,3,5,4,6:ncol(df_m_no_only_unisex_vars))]
df_f_no_only_unisex_vars = df_f_no_only_unisex_vars[,
                              c(1,2,3,5,4,6:ncol(df_f_no_only_unisex_vars))]

# equalling colnames
colnames(df_m_no_only_unisex_vars) = var_names_for_binded_df_m_and_f
colnames(df_f_no_only_unisex_vars) = var_names_for_binded_df_m_and_f

# adding Sex variable
df_m_no_only_unisex_vars = df_m_no_only_unisex_vars %>% mutate(Sex = "Male")
df_f_no_only_unisex_vars = df_f_no_only_unisex_vars %>% mutate(Sex = "Female")

# After variables Sex, adding all columns that are present in df but not in df_m_f
df_only_unisex = df %>% select(all_of(c('Region_name', 'Year', only_unisex_vars)))

df_m_no_only_unisex_vars = df_m_no_only_unisex_vars %>% left_join(df_only_unisex)
df_f_no_only_unisex_vars = df_f_no_only_unisex_vars %>% left_join(df_only_unisex)
# Creating df_m_f
df_m_f = bind_rows(df_m_no_only_unisex_vars, df_f_no_only_unisex_vars)

df_nosex = df %>% select(all_of(not_by_sex_vars))

# saving (NB: the NOTbysex are not included in the data shared as they are never used
# but we keep here if anyone is interested)

#setwd("your path")
# write.csv(df, file = "panel_regions.csv", row.names = FALSE)
# library(openxlsx)
# write.xlsx(df, file = "panel_regions.xlsx", rowNames = FALSE)
# 
# write.csv(df_m_f, file = "panel_regions_by_sex.csv", row.names = FALSE)
# write.xlsx(df_m_f, file = "panel_regions_by_sex.xlsx", rowNames = FALSE)
# 
# write.csv(df_m_f, file = "panel_regions_by_sex.csv", row.names = FALSE)
# write.xlsx(df_m_f, file = "panel_regions_by_sex.xlsx", rowNames = FALSE)
# write.csv(df_nosex, file = "panel_regions_NOTby_sex.csv", row.names = FALSE)
# write.xlsx(df_nosex, file = "panel_regions_NOTby_sex.xlsx", rowNames = FALSE)
# 
