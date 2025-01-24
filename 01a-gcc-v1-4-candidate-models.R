# Iterative search among candidate models
# Useful info on excluding / including terms: 
# https://rdrr.io/cran/MuMIn/man/dredge.html
# NOTE: Each run can day hours to DAYS depending on # of model terms. 
# NOTE 6.17 - Can't finish run with lat, lon at k = 14 (takes way too long). Reduced to k = 10 

library(here)
source(here("00-load-cc-libraries.R"))
source(here("00-load-cc-functions.R")) # functions
library(arm)
library(optimx)
library(mgcv)
library(mgcViz) # better GAM visualizations
conflict_prefer("view", "tibble")

#install_version("MuMIn", version = "1.46.0", repos = "http://cran.us.r-project.org")
library(MuMIn)
library(parallel)


# HOW MANY DREDGE VARIABLES? 
min_vars <- 9 # 4
max_vars <- 9 # 9 INCLUDING 4 included in all models -- region, mgmt, method, te(lat,lon)

ccmod_df <-  read_rds(here("outputs",
                 "04a_ccmod_df.RDS"))

gam_vars <- read_csv(here("intermediate-files","gam_vars.csv"))

gam_vars <- gam_vars %>% 
  filter(!value %in% c("latitude","longitude","mgmt_highest","method_cat","region")) %>% 
  pull(value)
  

# Setup dredge table ----
# 1671 -> 1658 
gam01_df <- ccmod_df %>% 
  dplyr::select(objectid, country,
                pct_hardcoral_transform, latitude, longitude, 
                mgmt_highest, method_cat, region,
                all_of(gam_vars)) %>% 
  drop_na()

# 1658
# gam01_df %>% skim()

# Setup dredge_01 'global model' ----
# this 'global model' includes correlated terms. remove these models from consideration
# it DOES NOT include lat, lon. we want to explain as much as possible with 
# biophysical/social covariates before adding these in.

gam01 <- gam(pct_hardcoral_transform ~ 
               te(longitude,latitude, bs = "gp", k = c(10, 10), m = 2) + 
               s(sst_annualtrend, k=6, bs = "cr")+               #
               s(sst_kurtosis, k = 6, bs = "cr")+                  # 
               s(sst_max, k = 6, bs = "cr")+                       # 
               s(sst_range, k = 6, bs = "cr")+                     #  
               s(sst_skewness, k = 6, bs = "cr") +                 # 
               s(sst_var, k = 6, bs = "cr")+                       #
               s(calcite, k = 6, bs = "cr")+                       #  
               s(currents_velocity_mean, k = 6, bs = "cr")+        #
               s(dhw_max_cumul, k = 6, bs = "cr") +                #
               s(diffuse_atn_max, k = 6, bs = "cr") +              #
               s(dissolved_oxygen, k = 6, bs = "cr") +             #
               s(gebco_depth, k = 6, bs = "cr") +                  #
               s(par_max, k = 6, bs = "cr")+                       # 
               s(ph_mean, k = 6, bs = "cr") +                      # 
               s(score_cn, k = 6, bs = "cr")+                      #
               s(score_cy, k = 6, bs = "cr")+                      #             
               mgmt_highest +                                    # all models
               s(method_cat, bs = 're') +                        #
               s(region, bs = 're'),                             #
             data = gam01_df, 
             gamr = 1.4, # higher penalty for complexity (default is 1)
             family = "betar", 
             link = "logit",# 
             method = "REML",
             na.action = "na.fail") 

# NOTE: MuMIn insists that you use na.action = na.fail, in order to ensure that the same data set is used for every model


## Run dredge  ----
Sys.setenv('R_MAX_VSIZE'='7Gb') # set below limit of physical memory

for(this_max in seq(min_vars,max_vars)){
  dredge_cluster = makeCluster(6, type = "SOCK")  ## also need snow installed
  set.seed(1986)
  clusterExport(dredge_cluster,"gam01_df")
  clusterEvalQ(dredge_cluster, library(mgcv))
  
  # Exclude models with variables correlated > 0.7
  # e.g. exclude models containing both X1 and X2 at the same time
  # dredge(fm1, subset = !(X1 && X2))
  # dredge(fm1, m.lim = c(0, 2))
  
  print(paste(Sys.time(), ": Running dredge for",this_max,"variables..." ))
  
  gam01_dredge <- dredge(gam01, 
                         cluster = dredge_cluster, 
                         subset = !(`s(sst_range, k = 6, bs = "cr")` && `s(sst_kurtosis, k = 6, bs = "cr")`),
                         fixed = ~ te(longitude, latitude, bs = "gp", k = c(10, 10), m = 2) + 
                           mgmt_highest + 
                           s(method_cat, bs = 're') +                #
                           s(region, bs = 're'),                             #
                         m.lim = c(this_max, this_max), 
                         extra = c("R^2", Dev_exp = function(x)
                           summary(x)$dev.expl*100))
  
  stopCluster(dredge_cluster)
  
  gam01_dredge %>% as_tibble() %>% 
    write_csv(here("intermediate-files",paste0("gam_dredge_",this_max,".csv")))
  
  print(paste(Sys.time(), ": Dredge for",this_max,"variables is complete! See output file." ))
}


