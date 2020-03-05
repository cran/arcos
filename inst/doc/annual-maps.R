## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Uncomment and run the lines below to see if you have the packages required already installed
# packages <- c("dplyr", "ggplot", "jsonlite", "knitr", "geofacet", "scales")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org") # }

# These are all the packages you'll need to run everything below 

library(dplyr)
library(ggplot2)
library(arcos)
library(jsonlite)
library(knitr)
library(geofacet)
library(scales)

## ----setup---------------------------------------------------------------
states <- combined_buyer_annual(key="WaPo")

kable(head(states))

## ----dplyr---------------------------------------------------------------
annual_states <- states %>% 
  group_by(BUYER_STATE, year) %>% 
  summarize(pills=sum(DOSAGE_UNIT)) %>% 
  filter(!is.na(BUYER_STATE))

kable(head(annual_states))

## ----map, warning=F, message=F, fig.width=9, fig.height=7----------------
ggplot(annual_states, aes(year, pills)) +
  geom_col() +
  facet_geo(~ BUYER_STATE, grid = "us_state_grid2") +
    scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  scale_y_continuous(label=comma) +
  labs(title = "Annual oxycodone and hydrocodone pills by state",
    caption = "Source: The Washington Post, ARCOS",
    x = "",
    y = "Dosage units") +
  theme(strip.text.x = element_text(size = 6))

## ----population, warning=F, message=F------------------------------------
population <- state_population(key="WaPo")

kable(head(population))

## ----join----------------------------------------------------------------
annual_states_joined <- left_join(annual_states, population) %>% 
  filter(!is.na(population))

kable(head(annual_states_joined))

## ----math----------------------------------------------------------------
annual_states_joined <- annual_states_joined %>% 
  mutate(pills_per=pills/population)

kable(head(annual_states_joined))

## ----map2, warning=F, message=F, fig.width=9, fig.height=7---------------

ggplot(annual_states_joined, aes(year, pills_per)) +
  geom_col() +
  facet_geo(~ BUYER_STATE, grid = "us_state_grid2") +
    scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  scale_y_continuous(label=comma) +
  labs(title = "Annual oxycodone and hydrocodone pills per person by state",
    caption = "Source: The Washington Post, ARCOS",
    x = "",
    y = "Dosage units") +
  theme(strip.text.x = element_text(size = 6))

