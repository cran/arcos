## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Uncomment and run the lines below to see if you have the packages required already installed
# packages <- c("dplyr", "ggplot2", "lubridate", "tidyr", "scales", "leaflet")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org") # }

library(arcos)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(leaflet)

## ----drug_list, warning=F, message=F, eval=F----------------------------------
#  # what drugs are there
#  
#  drugs <- drug_list(key="WaPo")
#  
#  drugs

## ----drug_list_real, warning=F, message=F, echo=F-----------------------------
# what drugs are there
drugs <- readRDS("data/drugs_list.RDS")

drugs

## ----methadone, warning=F, message=F, eval=F----------------------------------
#  # okay, we want METHADONE from Suffolk County
#  
#  methadone <- drug_county_biz(drug = "METHADONE", county = "Suffolk", state = "MA", key = "WaPo")
#  
#  # how big is this file
#  
#  nrow(methadone)

## ----methadone_real, warning=F, message=F, echo=F-----------------------------
methadone <- readRDS("data/methadone.RDS")

# how big is this file

nrow(methadone)

## ----buyers, warning=F, message=F---------------------------------------------
# What type of buyers

methadone %>% 
  select(BUYER_DEA_NO, BUYER_BUS_ACT) %>% 
  unique() %>% 
  count(BUYER_BUS_ACT)

## ----cat, warning=F, message=F------------------------------------------------
# Clean up and consolidate the categories

methadone <- methadone %>% 
  mutate(type=case_when(
    grepl("PHARMACY", BUYER_BUS_ACT) ~ "PHARMACY",
    grepl("DETOX", BUYER_BUS_ACT) ~ "DETOX/MAINTENANCE",
    grepl("MAINT", BUYER_BUS_ACT) ~ "DETOX/MAINTENANCE",
    grepl("-VA", BUYER_BUS_ACT) ~ "VA",
    grepl("CLINIC", BUYER_BUS_ACT) ~ "CLINIC",
    TRUE ~ BUYER_BUS_ACT
  ))

# totals
methadone_summary <- methadone %>% 
  # focus only on purchase orders, which is transaction code S
  filter(TRANSACTION_CODE=="S") %>% 
  group_by(type) %>% 
  # calculate amount
  mutate(amount=CALC_BASE_WT_IN_GM*MME_Conversion_Factor) %>% 
  summarize(total=sum(amount, na.rm=T)) %>% 
  arrange(desc(total))

# chart it  
ggplot(methadone_summary, aes(x=total, y=type)) +
  geom_col() +
  scale_x_continuous(label=comma) +
  theme_minimal() +
  labs(title="Methadone dosages ordered by buyer type")

## ----join_locations, warning=F, message=F, fig.width=9, fig.height=6----------
# Let's calculate the amount of methadone purchased by every buyer in Suffolk County, MA
methadone_by_buyer <- methadone %>% 
  filter(TRANSACTION_CODE=="S") %>% 
  group_by(BUYER_DEA_NO, type) %>% 
  mutate(amount=CALC_BASE_WT_IN_GM*MME_Conversion_Factor) %>% 
  summarize(total=sum(amount, na.rm=T)) %>% 
  arrange(desc(total))

# Saving the BUYER_DEA_NO of each purchaser from our data set so far
buyer_ids <- methadone %>% 
  select(BUYER_DEA_NO) %>% 
  unique()

# Download the addresses of buyers in Suffolk County, MA
buyer_addresses <- buyer_addresses(county = "Suffolk", state = "MA", key = "WaPo")

glimpse(buyer_addresses)

# We only care about the addresses of buyers who purchased methadone
buyers <- left_join(buyer_ids, buyer_addresses)

# Let's bring over the methadone order amounts
buyers <- left_join(buyers, methadone_by_buyer)

# Download the lat and lon of pharmacies in Suffolk County, MA
buyer_latlon <- pharm_latlon(county = "Suffolk", state = "MA", key = "WaPo")

glimpse(buyer_latlon)

# Let's just focus on pharmacies and detox/maintenence facilities

buyers_pharm <- buyers %>% 
  filter(type=="PHARMACY")
buyers_detox <- buyers %>% 
  filter(type=="DETOX/MAINTENANCE")

# We can easily join buyer_latlon because pharmacies are geolocated
buyers_pharm <- left_join(buyers_pharm, buyer_latlon)

# But non-pharmacies are not geolocated.
# So I'm going to geolocate these detox/maintenance locations by hand
# Here's the result

detox <- tribble(
  ~BUYER_DEA_NO, ~lat,  ~lon,
  "PV0130562",	42.366429,	-71.058752,
  "PA0203024",	42.327501,	-71.083288,
  "RB0192574",	42.363672,	-71.05972,
  "RC0304395",	42.319285,	-71.052785,
  "RH0102549",	42.333502,	-71.066541,
  "RH0345783",	42.333502,	-71.066541,
  "RC0252558",	42.333435,	-71.073025,
  "RB0307480",	42.334507,	-71.0741,
  "RC0441751",	42.300033,	-71.101911,
  "RD0284581",	42.319239,	-71.096921,
  "RR0198336",	42.300033,	-71.101911,
  "RC0463327",	42.333138,	-71.070542
)
  

# okay, let's join the detox dataframe to this locations dataframe above
buyers_detox <- left_join(buyers_detox, detox)

# Now that buyers_pharm and buyers_detox both have lat and lon data, we can join them
buyers <- rbind(buyers_pharm, buyers_detox) 

buyers <- buyers %>% 
  filter(!is.na(lon))

# Just ordering the data frame for mapping purposes
buyers <- buyers %>% 
  arrange(desc(total))

# Setting up some color options
cof <- colorFactor(c("#ffa500", "#13ED3F"), domain=c("PHARMACY", "DETOX/MAINTENANCE"))

# mapping with leaflet
m <- leaflet(buyers) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-71.091112, 42.338827, zoom = 12) %>% 
  addCircleMarkers(~lon, ~lat, 
                   popup=paste0(buyers$BUYER_NAME, "<br />",
                                buyers$total, " total methadone"),
                   weight = 3,
                   radius=sqrt(buyers$total)/30, 
                   color=~cof(type),
                   stroke = FALSE, 
                   fillOpacity = 0.3)%>% 
  addLegend("bottomright", 
            colors= c("#ffa500", "#13ED3F"), 
            labels=c("Detox", "Pharmacy"), 
            title="Buyer type") 
m

