# gcc-tutorial.R -------------------------------------------------------#
# 
# This code was created with conservation practitioners in mind. 
# It is not part of the scripts developed for Friedman & Darling, 2025; 
# But builds on that code-base to provide an example of how this model 
# can be used along with site-based monitoring to evaluate conservation 
# and management efforts. 
#
# In this script, we: 
# Load observed CSV from MERMAID. 
# Load global model results
# Create maps
# Create lollipop of differences
# Create box plot by management type
# 
# W. Friedman & E. Darling, 2025
# -----------------------------------------------------------------------#

# 0. Load libraries ----
library(here)
library(tidyverse)
library(sf)
library(skimr)
library(ggpubr)

# colors for plotting coral cover 
map_cols <- c('#92032a','#fee08b','#ffffbf',
              "#7fcdbb","#41b6c4","#1d91c0", "#225ea8","#0c2c84",
              '#2c054b')

# ---------------------------------------#
# Enter the country you're working in 
# This will be used to filter the global 5km coral predictions; 
# making smaller files that will make this file faster to run. 

this_country <- "Belize"

# ---------------------------------------#

# 1. Load and review site-based data output from MERMAID ----
# @EMILY: add code to pull directly from MERMAID?
# Or, use a file of just 1 countries site data?

# Load the survey data
surveys <- read_rds(here("data","cc_surveys_combn.RDS")) %>% 
  filter(country == this_country) %>% 
  rename(mgmt_highest = mgmt_simple) %>% 
  mutate(mgmt_highest = if_else(is.na(mgmt_highest), "open access", mgmt_highest), 
         mgmt_highest = factor(mgmt_highest, levels = c("no take", "restricted", "open access"))) %>% 
  # For sites with multiple surveys - keep the most recent
  # Alternatively, you may want to take an overall median for recent survey years
  # however you choose to do this, you need to get to one value per row (site) here
  arrange(-year) %>% 
  distinct(site, .keep_all = T) %>% 
  # Convert to a spatial data frame
  st_as_sf(coords = c("longitude","latitude"), remove = F,
           crs = 4326)

# Review survey data
surveys %>% head()
surveys %>% select(-geometry) %>% skim()
surveys %>% tabyl(mgmt_highest)

# Plot survey data
surveys %>% ggplot(aes(x = mgmt_highest, y = pct_hardcoral, fill = mgmt_highest))+
  geom_boxplot()+
  scale_fill_manual(values = c("dodgerblue","purple","grey"))+
  ggtitle("Survey data")+
  ylab("Percent live hardcoral (observed)")+
  xlab("Management")+
  theme_light()


# ---------------------------------------------------------------------------- #
# 2. Align the site-based survey data with the 5km coral grids and associated data ----

# Load the GCC model data, filter to this country:
lrp_gcc_sf <- read_rds(here("outputs", "04a_lrp_gcc_sf.RDS")) %>% 
  filter(country == this_country)

# Load GCC model variables:
gcc_vars <- read_csv(here("outputs","gam_vars_final.csv")) %>% 
  pull(variable)

# Join the surveys with the 5km coral grids and associated 
# climate, biophysical and human pressure variables for those grids:
surveys_5km <- surveys %>% 
  st_join(lrp_gcc_sf %>% select(objectid, 
                                all_of(gcc_vars), 
                                -c(mgmt_highest, method_cat)), 
          join = st_intersects) %>% 
  select(-method)

surveys_5km # WAS: cc_allreefs

# ---------------------------------------------------------------------------- #
# 3. Use the GCC model to predict coral cover for these sites ----
#
# Make counterfactual predictions for site-level data, for all site types: 
# no-take, restricted & open-access
#
# NOTE: site-level predictions are based on 5km resolution climate, 
# biophysical and human pressure variables for those grids. 

## 3.1. Prep data ----
# Create the a table representing the counterfactual prediction of "no management":
counterfac_sites_df <- surveys_5km %>% 
  as_tibble() %>% 
  select(site, country, latitude, longitude, pct_hardcoral, all_of(gcc_vars)) %>% 
  mutate(pct_hardcoral = pct_hardcoral/100) %>% 
  # Save the observed management type
  mutate(mgmt_highest_orig = mgmt_highest) %>% 
  # Make all sites "open access" to represent the counterfactual condition
  mutate(mgmt_highest = "open access", 
         mgmt_highest = factor(mgmt_highest, 
                               levels = c("no take", "restricted", "open access")))

# Checks
# This should show ALL open access sites:
counterfac_sites_df %>% tabyl(mgmt_highest) 
# This should show the original distirubution of site types:
counterfac_sites_df %>% tabyl(mgmt_highest_orig) 

# Review data
counterfac_sites_df%>% skim()

## 3.2. Predict ----
# Load the GCC model
gcc_mdl <- read_rds(here("outputs","gcc_v1_4_gam.RDS"))

# Predict
counterfac_sites_predict <- predict.gam(gcc_mdl, 
                                        type = "response",
                                        newdata=counterfac_sites_df, 
                                        se.fit = T)

# Save predictions and standard error (SE)
counterfac_sites_df$pct_hardcoral_counterfac <- counterfac_sites_predict[[1]]
counterfac_sites_df$pct_hardcoral_counterfac_se <- counterfac_sites_predict[[2]]

# Review
counterfac_sites_df %>% head()

# 4. Plot differences between observed and predicted ----
country_df_pred <- counterfac_sites_df %>% 
  mutate(cc_obs = pct_hardcoral * 100, 
         cc_pred = pct_hardcoral_counterfac * 100, 
         cc_pred_se = pct_hardcoral_counterfac_se *100,
         cc_pred_max = cc_pred + cc_pred_se, 
         cc_pred_min = cc_pred - cc_pred_se)

site_plt <- country_df_pred %>%
  ggplot(aes(y = reorder(site, cc_obs), x = cc_obs, shape = mgmt_highest_orig))+
  # segment from obs to predicted:
  geom_segment(aes(y = reorder(site, cc_obs), xend = cc_pred), 
               colour = "gray60", lwd = 0.4)+
  # segment from pred_min to pred_max
  geom_segment(aes(y = reorder(site, cc_obs), x = cc_pred_min, 
                   xend = cc_pred_max), 
               lwd = 0.5, colour = "black")+
  # add predicted points
  geom_point(aes(y = reorder(site, cc_obs), x = cc_pred), size = 2, color = "black", fill = "black", stroke = 0)+
  # add observed points, colored by % coral 
  geom_point(aes(fill = cc_obs), size = 2, color = "gray50", stroke = 0.2)+
  scale_fill_gradientn(colours = map_cols, limits = c(0,100), na.value = "gray90",
                       name = "% Live hard coral observed")+
  scale_shape_manual(values = c(21, 24, 22))+
  guides(shape = "none")+
  facet_wrap(~mgmt_highest_orig, nrow = 3, scales = "free_y",
             strip.position = "right")+
  xlab("% Live hard coral")+
  xlim(0,85)+
  scale_y_discrete(position = "right")+
  ggtitle(label = "", subtitle = "Impact Assessment")+
  theme_light()+
  theme(panel.grid.major = element_line(linewidth =.1, color="gray90"), 
        axis.text.y = element_text(size = 5), 
        axis.title.y = element_blank(),
        legend.position = "bottom")

site_plt


# 5. Create maps showing observed and predicted coral cover ----

## 5.1 Load files ----
# Load world boundaries
world <- read_sf(here("map-data","worldbank_hi_res","WB_countries_Admin0_10m.shp"))

# Load GCC 5km Baseline prediction
allreefs_pred <- read_rds(here("outputs","allreefs_pred_cc.RDS")) %>% 
  filter(country == this_country)

# Load GCC 5km Counterfactual prediction
allreefs_counterfac <- read_rds(here("outputs","allreefs_counterfac.RDS")) %>% 
  filter(country == this_country)

# Join by 'objectid' (grid cell), restrict to this country
predict_df <- allreefs_pred %>% 
  left_join(allreefs_counterfac %>% select(objectid, 
                                           pct_hardcoral_counterfac,
                                           pct_hardcoral_counterfac_se)) %>% 
  # replace "pct_hardcoral_obs" from the model with these observations %>%
  select(-pct_hardcoral_obs) %>% 
  left_join(surveys_5km %>% select(objectid, site, pct_hardcoral), by = "objectid") %>% 
  mutate(pct_hardcoral_obs = pct_hardcoral/100) %>% 
  select(-pct_hardcoral)

# Remove these; we don't need them anymore:
rm(allreefs_pred,allreefs_counterfac)

# Review
predict_df %>% head()

## 5.2 Create maps ----

# Create a bounding box for the focal region (lon, lat)
# xmin, xmax, ymin, ymax
bbox <- c(89, 86.9, 16.0, 18.0)

country_df <- predict_df %>%
  mutate(cc_pred = pct_hardcoral_predicted*100, 
         cc_obs = pct_hardcoral_obs*100, 
         cc_counterfac = pct_hardcoral_counterfac*100)

# convert country data to sf
country_sf <- country_df %>% 
  pivot_longer(cols = c(cc_obs, cc_pred, cc_counterfac), names_to = "var",
               values_to = "pct_hardcoral") %>% 
  mutate(var = factor(var, levels = c("cc_obs","cc_pred","cc_counterfac")), 
         var = recode_factor(var, 
                             "cc_obs" = "Observed",
                             "cc_pred" = "Predicted (observed conditions)", 
                             "cc_counterfac" = "Predicted (no mgmt)")) %>% 
  filter(var != "Predicted (no mgmt)") %>% 
  st_as_sf(crs = 4326)

# observed, predicted-base, predicted-nomgmt
country_plot <- ggplot(country_sf) +
  geom_sf(aes(fill = pct_hardcoral), lwd = 0.1, color = "gray70")+
  scale_fill_gradientn(colours = map_cols, limits = c(0,100), na.value = "gray90",
                       name = "% Live hard coral")+
  geom_sf(data = world, fill = "gray30")+
  xlim(bbox[1],bbox[2])+
  ylim(bbox[3],bbox[4])+
  facet_wrap(~var, ncol = 3)+
  theme(legend.position = "right", 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black", size = 8, hjust = 0))

country_plot

## 5.3 Make summary figure ----
# Combine the lollipop plots and map into a single figure
summary_fig <- ggarrange(country_plot, 
          site_plt+ 
            theme(plot.margin = unit(c(1,1,1,0.5), 'lines'), 
                  legend.position = "none"), 
          nrow = 2, 
          heights = c(1,1.5))

ggsave(here("plots","example-evaluation.png"),
       plot = summary_fig,
       height = 10, width = 8)

# -------------------------------------------------------------------------#

# 6. Summarise differences ----

# Set a threshold for the differential between predicted and observed that will
# be used to determine whether a site is under/over performing based on expectations. 
# We use 2x the global standard error of GCC predictions.
se2 <- .0554 # global SE for predictions

# Alternative: uncomment here to calculate SE for this country / these predictions
# counterfac_sites_df$pct_hardcoral_counterfac_se %>% mean(na.rm=T)
# counterfac_sites_df$pct_hardcoral_counterfac_se %>% median(na.rm=T)
# se2 <- counterfac_sites_df$pct_hardcoral_counterfac_se %>% mean(na.rm=T) *2

# Create a table identifying the sites that fall above or below 
# the predicted coral cover

conterfac_summary <- counterfac_sites_df %>% 
  mutate(obs_v_pred = pct_hardcoral - pct_hardcoral_counterfac,
         above_below_pred = case_when(obs_v_pred > se2 ~ "above", 
                                      obs_v_pred < -1*se2 ~ "below", 
                                      TRUE ~ "neither"))

# Review
conterfac_summary %>% tabyl(above_below_pred)
conterfac_summary %>% select(country, site, pct_hardcoral, 
                             pct_hardcoral_counterfac, above_below_pred)

# Summary table: number of sites above/below predicted by management cat

conterfac_summary %>% 
  group_by(country, above_below_pred) %>% 
  reframe(n_sites = n(), 
          observed_mean_cc = mean(pct_hardcoral, na.rm=T), 
          predicted_mean_cc = mean(pct_hardcoral_counterfac, na.rm=T)) %>% 
  mutate(pct_total_sites = n_sites/nrow(counterfac_sites_df)) %>% 
  select(country, above_below_pred,
         n_sites, pct_total_sites, 
         predicted_mean_cc, observed_mean_cc) %>% 
  arrange(country, above_below_pred)

