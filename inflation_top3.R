library(dplyr)
library(tidyr)
library(janitor)
library(tidyverse)

GDP_data <- read_csv("WorldEconomicOutlookData.csv") %>% clean_names()

#creating the inflation database
inflation_dat<- GDP_data %>%
  select(country, weo_subject_code, x2008, x2020) %>%
  filter(weo_subject_code == "PCPIPCH")

#converting column data to numeric data
inflation_dat$x2008 <- as.numeric(inflation_dat$x2008)
inflation_dat$x2020 <- as.numeric(inflation_dat$x2020)

sorted_2008 <- inflation_dat %>%
  arrange(desc(x2008))
sorted_2020 <- inflation_dat %>%
  arrange(desc(x2020))

print("According to the data, the highest %age increase in inflation was in Zimbabwe, Ethiopia and Seychelles in 2008.")
print("According to the data, the highest %age increase in inflation was in Zimbabwe, Sudan and Islamic Republic of Iran in 2020.")
