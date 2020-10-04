library(dplyr)
library(tidyr)
library(janitor)
library(tidyverse)

GDP_data <- read_csv("WorldEconomicOutlookData.csv") %>% clean_names()

#filtering US data
us_data <- GDP_data %>%
  filter(country == "United States") %>%
  select(country, weo_subject_code, x1980:x2021)
i <- c(3:44)
us_data[, i] <- apply(us_data[,i], 2,
                         function(x) as.numeric(as.character(x)))

# tidying US data
us_data %>%
  pivot_longer(names_to = "years",
               values_to = "values",
               cols = x1980:x2021) -> us_data

#US GDP per capita, US unemployment, US inflation
us_gdp_percapita <- us_data %>%
  filter(weo_subject_code == "NGDPRPPPPCPCH")
write_csv(us_gdp_percapita, "US GDP per capita.csv")

us_unemployment <- us_data %>%
  filter(weo_subject_code == "LUR")
write_csv(us_unemployment, "US unemployment.csv")

us_avginflation <- us_data %>%
  filter(weo_subject_code == "PCPIPCH")
write_csv(us_avginflation, "US avg inflation.csv")
