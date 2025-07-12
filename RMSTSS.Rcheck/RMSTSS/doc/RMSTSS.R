## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
   message = FALSE,
      eval = FALSE,
   warning = FALSE,
   comment = NA,
   fig.align = 'center',
   fig.width = 7,
   fig.height = 5,
   table.width = "100%",
   table.align = "center",
   collapse = TRUE
)
packages = c("survival", "dplyr", "tidyr", "knitr", "ggplot2", "mgcv", "kableExtra", "bibtex")
lapply(packages, require, character.only = TRUE)

library(RMSTSS)

## ----veteran_data_prep, echo=FALSE--------------------------------------------
# vet <- veteran %>%
#   mutate(
#     arm = ifelse(trt == 1, 0, 1),
#     status = status
#   )
# head(vet)

## ----veteran_power_calc-------------------------------------------------------
# power_results_vet <- linear.power.analytical(
#   pilot_data = vet,
#   time_var = "time",
#   status_var = "status",
#   arm_var = "arm",
#   linear_terms = "karno",
#   sample_sizes = c(100, 150, 200, 250),
#   L = 270
# )

## ----veteran_table_plot, echo=FALSE-------------------------------------------
# 
# kbl(power_results_vet$results_data , caption = "Power Analysis for Veteran Dataset") %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
# 

## ----veteran_ss_calc----------------------------------------------------------
# ss_results_vet <- linear.ss.analytical(
#   pilot_data = vet,
#   time_var = "time",
#   status_var = "status",
#   arm_var = "arm",
#   target_power = 0.40,
#   linear_terms = "karno",
#   L = 365,
#   n_start = 1000, n_step = 250, max_n_per_arm = 5000
# )

## ----veteran_ss_table, echo=FALSE---------------------------------------------
# 
# kbl(ss_results_vet$results_summary, caption = "Estimated Effect from Pilot Data") %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
# 
# ss_results_vet$results_plot +
#   theme_bw(base_size = 14)
# 

## ----linear_boot_example------------------------------------------------------
# power_boot_vet <- linear.power.boot(
#   pilot_data = vet,
#   time_var = "time",
#   status_var = "status",
#   arm_var = "arm",
#   linear_terms = "karno",
#   sample_sizes = c(150, 200, 250),
#   L = 365,
#   n_sim = 200
# )

## ----echo=FALSE---------------------------------------------------------------
# power_boot_vet$results_plot

## -----------------------------------------------------------------------------
# ss_boot_vet <- linear.ss.boot(
#   pilot_data = vet,
#   time_var = "time",
#   status_var = "status",
#   arm_var = "arm",
#   target_power = 0.5,
#   linear_terms = "karno",
#   L = 180,
#   n_sim = 500,
#   patience = 5
# )

## ----echo=FALSE---------------------------------------------------------------
# 
# ss_boot_vet$results_plot +
#   theme_bw(base_size = 14)

## ----colon_data_prep, echo=FALSE----------------------------------------------
# colon_death <- colon %>%
#   filter(etype == 2) %>%
#   select(time, status, rx, extent) %>%
#   na.omit() %>%
#   mutate(
#     arm = ifelse(rx == "Obs", 0, 1),
#     status = status,
#     strata = factor(extent)
#   )
# head(colon_death)

## ----colon_ss_calc------------------------------------------------------------
# ss_results_colon <- additive.ss.analytical(
#   pilot_data = colon_death,
#   time_var = "time", status_var = "status", arm_var = "arm", strata_var = "strata",
#   target_power = 0.60,
#   L = 1825,
#   n_start = 100, n_step = 100, max_n_per_arm = 10000
# )

## ----colon_ss_table, echo=FALSE-----------------------------------------------
# 
# kbl(ss_results_colon$results_summary , caption = "Estimated Effect from Pilot Data") %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
# final_n_colon <- ss_results_colon$results_data$Required_N_per_Stratum
# power_at_final_n_colon <- ss_results_colon$results_plot$data %>%
#   filter(N_per_Stratum == final_n_colon) %>% pull(Power)
# 
# ss_results_colon$results_plot
# 

## ----additive_power_calc------------------------------------------------------
# power_results_colon <- additive.power.analytical(
#   pilot_data = colon_death,
#   time_var = "time",
#   status_var = "status",
#   arm_var = "arm",
#   strata_var = "strata",
#   sample_sizes = c(1000, 3000, 5000),
#   L = 1825 # 5 years
# )
# 

## ----additive_power_table_plot, echo=FALSE------------------------------------
# kbl(power_results_colon$results_data, caption = "Power for Additive Stratified Colon Trial") %>%
#  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
# 
# power_results_colon$results_plot +
#   geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
#   labs(title = "Power Curve for Additive Stratified Model") +
#   theme_bw(base_size = 14)

## ----ms_power_analytical_example----------------------------------------------
# power_ms_analytical <- MS.power.analytical(
#    pilot_data = colon_death,
#    time_var = "time", status_var = "status", arm_var = "arm", strata_var = "strata",
#    sample_sizes = c(300, 400, 500),
#    L = 1825
# )

## ----echo=FALSE---------------------------------------------------------------
# 
# kbl(power_ms_analytical$results_data, caption = "Power for Multiplicative Stratified Model") %>%
#   kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")

## ----ms_ss_analytical_example-------------------------------------------------
# ms_ss_results_colon <- MS.ss.analytical(
#    pilot_data = colon_death, time_var = "time", status_var = "status", arm_var = "arm", strata_var = "strata",
#    target_power = 0.6,L = 1825)

## ----echo=FALSE---------------------------------------------------------------
# 
# kbl(ms_ss_results_colon$results_summary, caption = "Sample Size for Multiplicative Stratified Model") %>%
#   kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
# 
# ms_ss_results_colon$results_plot +
#    theme_bw(base_size = 14)

## ----ms_power_boot_example----------------------------------------------------
# power_ms_boot <- MS.power.boot(
#    pilot_data = colon_death,
#    time_var = "time",
#    status_var = "status",
#    arm_var = "arm",
#    strata_var = "strata",
#    sample_sizes = c(100, 300, 500),
#    L = 1825,
#    n_sim = 100,
#    parallel.cores = 10
# )

## ----echo=FALSE---------------------------------------------------------------
# kbl(power_ms_boot$results_summary, caption = "Power for Multiplicative Stratified Model (Bootstrap)") %>%
#   kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
# power_ms_boot$results_plot

## ----ms_ss_boot_example-------------------------------------------------------
# ss_ms_boot <- MS.ss.boot(
#    pilot_data = colon_death,
#    time_var = "time",
#    status_var = "status",
#    arm_var = "arm",
#    strata_var = "strata",
#    target_power = 0.5,
#    L = 1825,
#    n_sim = 100,
#    n_start = 100,
#    n_step = 50,
#    patience = 4,
#    parallel.cores = 10
# )

## ----echo=FALSE---------------------------------------------------------------
# 
# kbl(ss_ms_boot$results_summary, caption = "Sample Size for Multiplicative Stratified Model (Bootstrap)") %>%
#   kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")

## ----gbsg_data_prep, echo=FALSE-----------------------------------------------
# gbsg_prepared <- gbsg %>%
#    mutate(
#       arm = ifelse(hormon == "no", 0, 1)
#    )
# head(gbsg_prepared)

## ----gbsg_power_calc----------------------------------------------------------
# power_gam <- GAM.power.boot(
#    pilot_data = gbsg_prepared,
#    time_var = "rfstime",
#    status_var = "status",
#    arm_var = "arm",
#    smooth_terms = "pgr", # Model pgr with a smooth term
#    sample_sizes = c(50, 200, 400),
#    L = 2825, # 5 years
#    n_sim = 500,
#    parallel.cores = 10
# )
# 
# print(power_gam$results_plot)

## -----------------------------------------------------------------------------
# ss_gam <- GAM.ss.boot(
#    pilot_data = gbsg_prepared,
#    time_var = "rfstime",
#    status_var = "status",
#    arm_var = "arm",
#    target_power = 0.95,
#    L = 182,
#    n_sim = 500,
#    patience = 5,
#    parallel.cores = 10
# )

## ----echo=FALSE---------------------------------------------------------------
# ss_gam$results_plot +
#    theme_bw(base_size = 14)

## ----mgus2_data_prep, echo=FALSE----------------------------------------------
# mgus_prepared <- mgus2 %>%
#    mutate(
#       event_primary = ifelse(pstat == 1, 1, 0),
#       event_dependent = ifelse(pstat == 0 & death > 0, 1, 0),
#       arm = ifelse(sex == "M", 1, 0)
#    ) %>%
#    rename(time = futime)
# head(mgus_prepared)

## ----dc_power_calc------------------------------------------------------------
# dc_power_results <- DC.power.analytical(
#    pilot_data = mgus_prepared,
#    time_var = "time",
#    status_var = "event_primary",
#    arm_var = "arm",
#    dep_cens_status_var = "event_dependent",
#    sample_sizes = c(100, 250, 500),
#    linear_terms = "age",
#    L = 120 # 10 years
# )

## ----echo=FALSE---------------------------------------------------------------
# kbl(dc_power_results$results_summary, caption = "Power Analysis for MGUS Progression Study") %>%
#   kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
# 
# dc_power_results$results_plot

## ----mgus2_ss_calc------------------------------------------------------------
# ss_dc_mgus <- DC.ss.analytical(
#    pilot_data = mgus_prepared,
#    time_var = "time",
#    status_var = "event_primary",
#    arm_var = "arm",
#    dep_cens_status_var = "event_dependent",
#    target_power = 0.80,
#    linear_terms = "age",
#    L = 120, # 10 years
#    n_start = 100, n_step = 50, max_n_per_arm = 5000
# )
# 

## ----mgus2_table, echo=FALSE--------------------------------------------------
# 
# kbl(ss_dc_mgus$results_summary, caption = "Estimated Effect from Pilot Data") %>%
#   kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
# 
# ss_dc_mgus$results_plot +
#    theme_bw(base_size = 14)
# 

## ----eval=FALSE---------------------------------------------------------------
# RMSTSS::run_app()

