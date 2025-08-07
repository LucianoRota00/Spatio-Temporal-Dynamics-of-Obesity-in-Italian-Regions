# Modeling Spatio-Temporal Dynamics of Obesity in Italian Regions via Bayesian Beta Regression

This repository contains all the code, data and visualizations related to the spatio-temporal Bayesian Beta regression models described in our paper.

---

## Folder Structure

- `data_and_models/`  
  Contains all the data used to fit the models and the corresponding outputs.  
  - Files starting with `Train` are the training models (see Appendix B).
  - The four complete models (covering all years) follow the specifications described in Appendix B: the primary model discussed in the paper (see Section 3 – Methods and Appendix A) is:  
    **`Paper_Model_rhobeta_sexfixed.RData`**, the other three are reported for completeness.

- `dati.zip`  
  Includes all raw data used to build the original dataset and data used for feature engineering and creation of dataset used in the models 
  (see `Orig_dataset_creation.R` and `Dataset_model_creation.R`).

- `Images_paper.zip`  
  Contains the figures used in the paper.

---

## R Script Descriptions

1. **`Main_code.R`**  
   Fits both the training and complete models, and performs predictive analysis.

2. **`Viz_models_params.R`**  
   Generates posterior summary statistics and visualizations, including some convergence diagnostics.

3. **`Italy_map_post_mean_spatial.R`**  
   Creates map visualizations of the posterior mean of the spatial random effect.

4. **`Mod_eval_and_comp.R`**  
   Compares training models as presented in Appendix B.

5. **`Model_diagnostics.R`**  
   Visual checks and plots for assessing MCMC convergence.

6. **`Dataset_model_creation.R`**  
   Builds the dataset used in the models, including feature engineering and preprocessing  
   (based on output from `Orig_dataset_creation.R`).

7. **`Orig_dataset_creation.R`**  
   Combines raw data sources to create the original dataset (see Section 2 – Data of the paper).

---

## Citation & Use

If you use this repository, code or data, please cite the original paper.

---

## Contact

For any questions or collaborations, feel free to contact:

- 📧 lucianorota37@gmail.com  
- 📧 l.rota43@campus.unimib.it

