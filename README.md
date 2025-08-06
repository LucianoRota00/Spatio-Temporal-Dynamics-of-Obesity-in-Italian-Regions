# Modeling Spatio-Temporal Dynamics of Obesity in Italian Regions Via Bayesian Beta Regression Repository

This repository contains all the code, data, and visualizations related to the spatial models and analyses reported in our paper.

## Folder Structure
- `data_and_models/`: Contains all the data used to fit the models (see `Main_code.R` and `Dataset_model_creation.R`).
- `dati/`: Contains all the raw data used to build the original dataset and some data used for feature engineering (see `Orig_dataset_creation.R` and `Dataset_model_creation.R`).
- `Images_paper/`: All figures used in the paper.

## R Scripts Overview

1. **Main_code.R**  
   Code to fit both training and complete models, and includes some predictive analysis.

2. **Viz_models_params.R**  
   Contains posterior summary statistics and visualizations, including some convergence diagnostics.

3. **Italy_map_post_mean_spatial.R**  
   Generates the map visualization of the posterior mean of the spatial random effect.

4. **Mod_eval_and_comp.R**  
   Compares the training models, as reported in Appendix B of the paper.

5. **Model_diagnostics.R**  
   Visualizations to assess convergence of the MCMC algorithm.

6. **Dataset_model_creation.R**  
   Builds the dataset used for model fitting, including preprocessing and feature engineering, starting from dataset built on "Orig_dataset_creation.R".

7. **Orig_dataset_creation.R**  
   Combines raw data sources to build the original dataset (see Section 2 – Data of the paper).

## How to Cite or Use
Please cite our paper if you use any of the code or data.

## Contacts
For any information please write to lucianorota37@gmail.com or l.rota43@campus.unimib.it .
