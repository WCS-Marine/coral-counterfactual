# Tables and Plots for GCC MS
# W. Friedman 

library(here)
source(here("00-load-cc-libraries.R"))
source(here("00-load-cc-functions.R"))

# Survey data
cc_surveys <- read_rds(here("data", "cc_surveys_combn.RDS"))

# Table S1 - # of survey
cc_surveys %>% tabyl(db, method) %>% adorn_totals() #%>% view()

# Table S2 - years
cc_surveys %>% tabyl(year) #%>% view()

# Table S3 - countries
cc_surveys %>% tabyl(country) #%>% view()

# Survey data aligned to LRP grids
# (median of coral cover associated with each cell)
ccsurveys_lrp <- read_rds(here("data","allreefs_gcc.RDS"))
  
ccsurveys_lrp

# Just cells with observed data
ccmod_df <- read_rds(here("outputs",
              "04a_ccmod_df.RDS"))

# 
ccmod_df %>% 
  ggplot(aes(x = pct_hardcoral))+
  geom_histogram()+
  facet_wrap(~region)

ccmod_df %>% 
  ggplot(aes(x = gebco_depth))+
  geom_histogram()

ccmod_df$pct_hardcoral %>% summary()
ccmod_df$gebco_depth %>% summary()

ccmod_df$aca_top_geo_simple %>% tabyl()

