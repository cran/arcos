## ---- include = FALSE---------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Uncomment and run the lines below to see if you have the packages required already installed
# packages <- c("dplyr", "ggplot2", "jsonlite", "knitr", "geofacet", "scales", "zoo", "knitr")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org") # }

library(arcos)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(scales)
library(zoo)
library(knitr)

## ----buyer--------------------------------------------------------------------
pharm <- buyer_addresses(county = "Seminole", state="FL", key="WaPo")

glimpse(pharm)

## ----lockwood-----------------------------------------------------------------
pharm <- filter(pharm, 
                grepl("LOCKWOOD", BUYER_ADDRESS1))

kable(pharm)

## ----oviedo-------------------------------------------------------------------
pharm1 <- pharmacy_raw(buyer_dea_no = "BW8487438", key = "WaPo")

glimpse(pharm1)

## ----summarizing--------------------------------------------------------------
# consolidating by month

pharm_monthly <- pharm1 %>% 
  # setting transactio code to S because those are purchases
  filter(TRANSACTION_CODE=="S") %>% 
  filter(DRUG_NAME=="OXYCODONE") %>% 
  mutate(TRANSACTION_DATE=mdy(TRANSACTION_DATE)) %>% 
  mutate(year=year(TRANSACTION_DATE),
         month=month(TRANSACTION_DATE),
         month_date=mdy(paste0(month, "/1/", year))) %>% 
  group_by(year, month, month_date) %>% 
  summarize(pills=sum(DOSAGE_UNIT))

## ----six_month_max, warning=F, message=F--------------------------------------
# Slices out the max orders within a window of 6 months prior to the current month
monthly_drug_six <- pharm_monthly %>% 
  ungroup() %>% 
  arrange(month_date) %>% 
  mutate(lag1=lag(pills, 1),
         lag2=lag(pills, 2),
         lag3=lag(pills, 3),
         lag4=lag(pills, 4),
         lag5=lag(pills, 5),
         lag6=lag(pills, 6)
  ) %>% 
  pivot_longer(cols=5:10, names_to="lag", values_to="total") %>% 
  group_by(year, month, pills, month_date) %>% 
  arrange(desc(total)) %>% 
  slice(1)  %>% 
  ungroup()


# creating a column that indicates if a monthly dosage exceeds the 6 month max prior

monthly_drug_six <- monthly_drug_six %>% 
  mutate(six_max_flag=case_when(
         pills > total ~ T,
         TRUE ~ F))

#monthly_drug_six$trail_six_max_flag <- ifelse(monthly_drug_six$pills > monthly_drug_six$total, 1, 0)

## ----six_month_plot, warning=F, message=F-------------------------------------

ggplot(monthly_drug_six, aes(x=month_date, y=pills)) +
  geom_col(fill="cadetblue3") +
  geom_step(aes(x=month_date, y=total, fill="Maximum 6 month order"), color="tomato", opacity=.6)+
    scale_y_continuous(label=comma) +
  theme_minimal() +
  labs(title="Monthly oxycodone pill orders from a Walgreens pharmacy in Oviedo, FL",
       subtitle='As compared to the DEA suspicious "exceeds the 6-month maximum" order criteria')


## ----table1-------------------------------------------------------------------
table(monthly_drug_six$six_max_flag)

## ----rolling avg, warning=F, message=F----------------------------------------
# we'll use the rollmean() function from the zoo package

rolling <- pharm_monthly %>% 
  ungroup() %>% 
  arrange(month_date) %>% 
  mutate(avg_pills = zoo::rollmean(pills, k = 12, fill = NA, align="right")) %>% 
  mutate(avg_pills_2x=avg_pills*2) %>% 
  mutate(roll_flag_2x=case_when(
    avg_pills_2x<pills ~ T,
    TRUE ~ F
  ))

# CALCULATE ROLLING AVERAGE HERE

ggplot(rolling, aes(x=month_date, y=pills)) +
  geom_col(fill="cadetblue3") +
  geom_step(aes(x=month_date, y=avg_pills_2x, fill="Double the 12 month rolling average"), direction="mid",color="tomato", opacity=.06) +
  scale_y_continuous(label=comma) +
  theme_minimal() +
  labs(title="Monthly oxycodone pill orders from a Walgreens pharmacy in Oviedo, FL",
       subtitle='As compared to the DEA suspicious "double the 12-month rolling average" order criteria')


## ----table2-------------------------------------------------------------------
table(rolling$roll_flag_2x)

