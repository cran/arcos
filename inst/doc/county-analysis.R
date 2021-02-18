## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# Uncomment and run the lines below to see if you have the packages required already installed
# packages <- c("dplyr", "ggplot2", "forcats", "jsonlite", "knitr", "geofacet", "scales")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org") # }

library(arcos)
library(knitr)
library(tigris)
library(viridis)
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

## ----top_county_pharmacy, eval=F----------------------------------------------
#  mingo <- total_pharmacies_county(county = "Mingo", state="WV", key="WaPo")
#  
#  kable(head(mingo))

## ----top_county_pharmacy_real, cho=F------------------------------------------
mingo <- readRDS("data/mingo.RDS")

kable(head(mingo))

## ----top_county_pharmacy_chart, fig.width=9, fig.height=7---------------------
ggplot(mingo,
       aes(x=total_dosage_unit, y=fct_reorder(buyer_name, total_dosage_unit))) +
  geom_segment(
       aes(x = 0,
           y=fct_reorder(buyer_name, total_dosage_unit),
           xend = total_dosage_unit,
           yend = fct_reorder(buyer_name, total_dosage_unit)),
           color = "gray50") +
           geom_point() +
    scale_x_continuous(label=comma) +
  labs(x="Dosage units", y="", 
       title = "Pills sold at Mingo, WV pharmacies",
       subtitle = "Between 2006 and 2014",
       caption = "Source: The Washington Post, ARCOS") +
  theme_minimal()

## ----wv_data, eval=F----------------------------------------------------------
#  wv <- summarized_county_annual(state="WV", key="WaPo")
#  
#  kable(head(wv))

## ----wv_data_real, echo=F-----------------------------------------------------
wv <- readRDS("data/wv.RDS")

kable(head(wv))

## ----tigris, warning=F, message=F, quietly=T, results="hide", eval=F----------
#  ## Set the option for shapefiles to load with sf
#  options(tigris_class = "sf")
#  
#  ## Function to download county shapefiles in West Virginia
#  wv_shape <- counties(state="WV", cb=T)
#  
#  ## Join the county dosage data we pulled
#  wv <- left_join(wv, wv_shape, by=c("countyfips"="GEOID"))

## ----tigris_real, warning=F, message=F, quietly=T, results="hide", echo=F-----
wv_shape <- readRDS("data/wv_shape.RDS")

## Join the county dosage data we pulled
wv <- left_join(wv, wv_shape, by=c("countyfips"="GEOID"))

## ----facet_map, fig.width=9, fig.height=7, eval=F-----------------------------
#  # Mapping with ggplot2, sf, and viridis
#  
#  wv %>%
#    ggplot(aes(geometry=geometry, fill = DOSAGE_UNIT, color = DOSAGE_UNIT)) +
#    facet_wrap(~year, ncol=2) +
#    geom_sf() +
#    coord_sf(crs = 26915) +
#    scale_fill_viridis(direction=-1, label = comma) +
#    scale_color_viridis(direction=-1, label = comma) +
#    theme_void() +
#    theme(panel.grid.major = element_line(colour = 'transparent')) +
#    labs(title="Oxycodone and hydrocodone pills in West Virginia", caption="Source: The Washington Post, ARCOS")

## ----facet_map_real, fig.width=9, fig.height=7, echo=F------------------------
# Mapping with ggplot2, sf, and viridis
wv <- readRDS("data/wv_facet.RDS")
wv %>%
  ggplot(aes(geometry=geometry, fill = DOSAGE_UNIT, color = DOSAGE_UNIT)) +
  facet_wrap(~year, ncol=2) +
  geom_sf() +
  coord_sf(crs = 26915) + 
  scale_fill_viridis(direction=-1, label = comma) +
  scale_color_viridis(direction=-1, label = comma) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Oxycodone and hydrocodone pills in West Virginia", caption="Source: The Washington Post, ARCOS")

## ----pop, warning=F, message=F, fig.width=9, fig.height=7, eval=F-------------
#  
#  population <- county_population(state="WV", key="WaPo") %>%
#  # isolate the columns so it doesn't conflict in a join (there are doubles, that's why)
#      select(countyfips, year, population)
#  
#  left_join(wv, population) %>%
#    mutate(per_person=DOSAGE_UNIT/population) %>%
#    ggplot(aes(geometry=geometry, fill = per_person, color = per_person)) +
#    facet_wrap(~year, ncol=2) +
#    geom_sf() +
#    coord_sf(crs = 26915) +
#    scale_fill_viridis(direction=-1, label = comma) +
#    scale_color_viridis(direction=-1, label = comma) +
#    theme_void() +
#    theme(panel.grid.major = element_line(colour = 'transparent')) +
#    labs(title="Oxycodone and hydrocodone pills in West Virginia per person", caption="Source: The Washington Post, ARCOS")

## ----pop_real, warning=F, message=F, fig.width=9, fig.height=7, echo=F--------

population <- readRDS("data/wvpop.RDS")

left_join(wv, population) %>% 
  mutate(per_person=DOSAGE_UNIT/population) %>%
  ggplot(aes(geometry=geometry, fill = per_person, color = per_person)) +
  facet_wrap(~year, ncol=2) +
  geom_sf() +
  coord_sf(crs = 26915) + 
  scale_fill_viridis(direction=-1, label = comma) +
  scale_color_viridis(direction=-1, label = comma) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Oxycodone and hydrocodone pills in West Virginia per person", caption="Source: The Washington Post, ARCOS")

