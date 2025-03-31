## Load libraries
library(mermaidr)
library(tidyverse)

## Get all WCS summary sample event data
allWcsSeHardCoralCoverTBL <- mermaid_get_summary_sampleevents() %>% 
  filter(grepl(pattern = "WCS", x = tags)) %>% 
  select(project, tags, country, site, latitude, longitude,
         management, management_rules, sample_date,
         `benthicpit_percent_cover_benthic_category_avg_Hard coral`,
         `benthiclit_percent_cover_benthic_category_avg_Hard coral`,
         `benthicpqt_percent_cover_benthic_category_avg_Hard coral`,
         quadrat_benthic_percent_percent_hard_avg_avg)

longAllWcsSeHardCoralCoverTBL <- allWcsSeHardCoralCoverTBL %>% 
  pivot_longer(
    cols = c(`benthicpit_percent_cover_benthic_category_avg_Hard coral`,
             `benthiclit_percent_cover_benthic_category_avg_Hard coral`,
             `benthicpqt_percent_cover_benthic_category_avg_Hard coral`,
             quadrat_benthic_percent_percent_hard_avg_avg), # Columns to pivot
    names_to = "Protocol",    # New column for protocol names
    values_to = "HardCoralCover" # New column for coral cover values
  ) %>% 
  filter(!is.na(HardCoralCover))  

longAllWcsSeHardCoralCoverTBL
view(head(longAllWcsSeHardCoralCoverTBL))

write_csv(longAllWcsSeHardCoralCoverTBL, 
          here("wcs-coral-data-28mar2025.csv"))
