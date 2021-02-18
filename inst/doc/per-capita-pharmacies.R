## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Uncomment and run the lines below to see if you have the packages required already installed
# packages <- c("dplyr", "ggplot2", "jsonlite", "knitr", "geofacet", "scales")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org") # }

library(arcos)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(formattable)
library(stringr)
library(scales)
library(knitr)

## ----pull_data, eval=F--------------------------------------------------------
#  west_virginia <- total_pharmacies_state(state="WV", key="WaPo")
#  
#  kable(head(west_virginia))

## ----pull_data_real, echo=F---------------------------------------------------
west_virginia <- readRDS("data/wv_pharms.RDS")

kable(head(west_virginia))

## ----population, eval=F-------------------------------------------------------
#  population <- county_population(state="WV", key="WaPo")
#  
#  kable(head(population))

## ----population_real, echo=F--------------------------------------------------
population <- readRDS("data/wv_pop.RDS")

kable(head(population))

## ----population_refined-------------------------------------------------------
population <- population %>% 
  group_by(BUYER_COUNTY, BUYER_STATE, countyfips) %>% 
  # Figure out the average population between available years
  summarize(average_population=mean(population, na.rm=T)) %>% 
  ## Have to quickly rename these columns to make them lower case so they'll join easily to the other data frame
  rename(buyer_county=BUYER_COUNTY, buyer_state=BUYER_STATE)

## Join the data
wv_joined <- left_join(west_virginia, population)

kable(head(wv_joined))

## ----join---------------------------------------------------------------------
wv_joined <- wv_joined %>% 
  mutate(per_person=total_dosage_unit/average_population/7)

kable(head(wv_joined))

## ----colors, eval=F-----------------------------------------------------------
#  
#  ## Get a list of addresses because it includes BUYER_BUS_ACT information
#  pharmacy_list <- buyer_addresses(state="WV", key="WaPo")
#  
#  # We just want the BUYER_BUS_ACT to tell if these are practitioners are retail pharmacies
#  # This will help us filter out the appropriate pharmacies
#  
#  pharmacy_list <- pharmacy_list %>%
#    select(buyer_dea_no=BUYER_DEA_NO, BUYER_BUS_ACT)
#  
#  # Join to the original data set
#  wv_joined <- left_join(wv_joined, pharmacy_list)
#  
#  # Filter the data so we only have retail and chain pharmacies
#  wv_joined <- wv_joined %>%
#    filter(BUYER_BUS_ACT=="RETAIL PHARMACY" | BUYER_BUS_ACT=="CHAIN PHARMACY")
#  
#  # Just in case, let's get the BUYER_DEA_NO of pharmacies that aren't really pharmacies
#  not_pharms <- not_pharmacies(key="WaPo") %>% pull(BUYER_DEA_NO)
#  
#  # Filter those out, too, if they're in there
#  wv_joined <- wv_joined %>%
#    filter(!buyer_dea_no %in% not_pharms)
#  
#  # clean up column names so we can make a pretty table
#  wv_joined <- wv_joined %>%
#    select(Pharmacy=buyer_name, City=buyer_city, County=buyer_county, `County population`=average_population,
#           Pills=total_dosage_unit, `Pills per person`=per_person) %>%
#    mutate(`County population`=round(`County population`),
#           `Pills per person`=round(`Pills per person`, 1)) %>%
#    arrange(desc(`Pills per person`)) %>%
#    slice(1:100)
#  
#  # Create some custome colors
#  customGreen0 = "#DeF7E9"
#  customGreen = "#71CA97"
#  customRed = "#ff7f7f"
#  
#  # produce a table
#  wv_joined %>%
#    formattable(align=c("l", "l", "l", "r", "r", "r"),
#                list(Pharmacy = formatter("span", style = ~ style(color="grey", font.weight = "bold")),
#                     Pills=color_tile(customGreen0, customGreen),
#                     `Pills per person` = normalize_bar(customRed)
#                     ))

## ----colors_real, echo=F------------------------------------------------------

## Get a list of addresses because it includes BUYER_BUS_ACT information
pharmacy_list <- readRDS("data/wv_pharm_list.RDS")

# We just want the BUYER_BUS_ACT to tell if these are practitioners are retail pharmacies
# This will help us filter out the appropriate pharmacies

pharmacy_list <- pharmacy_list %>% 
  select(buyer_dea_no=BUYER_DEA_NO, BUYER_BUS_ACT)

# Join to the original data set
wv_joined <- left_join(wv_joined, pharmacy_list)

# Filter the data so we only have retail and chain pharmacies
wv_joined <- wv_joined %>% 
  filter(BUYER_BUS_ACT=="RETAIL PHARMACY" | BUYER_BUS_ACT=="CHAIN PHARMACY")

# Just in case, let's get the BUYER_DEA_NO of pharmacies that aren't really pharmacies
not_pharms <- readRDS("data/not_pharms.RDS")

# Filter those out, too, if they're in there
wv_joined <- wv_joined %>% 
  filter(!buyer_dea_no %in% not_pharms)

# clean up column names so we can make a pretty table
wv_joined <- wv_joined %>% 
  select(Pharmacy=buyer_name, City=buyer_city, County=buyer_county, `County population`=average_population,
         Pills=total_dosage_unit, `Pills per person`=per_person) %>% 
  mutate(`County population`=round(`County population`),
         `Pills per person`=round(`Pills per person`, 1)) %>% 
  arrange(desc(`Pills per person`)) %>% 
  slice(1:100)

# Create some custome colors
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

# produce a table
wv_joined %>% 
  formattable(align=c("l", "l", "l", "r", "r", "r"),
              list(Pharmacy = formatter("span", style = ~ style(color="grey", font.weight = "bold")),
                   Pills=color_tile(customGreen0, customGreen),
                   `Pills per person` = normalize_bar(customRed)
                   ))

