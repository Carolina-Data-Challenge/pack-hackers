setwd("/Users/mac/Project/pack-hackers")
# libraries needed
library(ggplot2)
library(dplyr)
library(tidyr)
library(writexl)

# read in data and convert n/a and "" to NA values in R
worldEcon <- read.csv("WorldEconomicOutlookData.csv", na.strings = c("n/a", ""))
colnames(worldEcon)
attach(worldEcon)

# data preprocessing
# the last row is not useful information, can delete it
worldEcon <- worldEcon[-1554, ]
worldEcon <- worldEcon[-1553, ]

# check number of missing value for each country and each year
summary(worldEcon)


# get each country and each year current GDP
transformedData <- worldEcon %>%
  select(Country, WEO.Subject.Code, starts_with("X")) %>%
  filter(WEO.Subject.Code == "PPPGDP") %>%
  gather(Year, GDP, X1980:X2021)


write.csv(transformedData, "Transformed.csv")
write_xlsx(transformedData, "Transformed.xlsx")


# world current GDP and GDP per capita
worldConsCurGDP <- worldEcon %>%
  select(Country, WEO.Subject.Code, starts_with("X")) %>%
  pivot_longer(names_to = "Years", values_to = "Value", cols = X1980:X2021) %>%
  pivot_wider(names_from = WEO.Subject.Code, values_from = Value)

write_xlsx(finalData, "world.xlsx")

attach(worldConsCurGDP)
# find correlation among all variables
# create a df only contains numerical variables
numVar <- worldConsCurGDP[3:10] %>%
  filter(Country == "China")
pairs(numVar, col = "red")
# based on the correlation plot, the current GDP, inflation rate, and unemployment rate has little correlation with each other
# so we decide to choose these three to explore

# create the final dataset that we will use
finalData <- worldConsCurGDP %>%
  select(Country, Years, PPPGDP, PCPIEPCH, LUR)

write_xlsx(finalData, "FinalData.xlsx")

summary(finalData)
typeof(PPPGDP)


# find out which countries fall the most for GDP
# look at year 2008 and 2007, see number of countries gdp decreased. gdp2008 - gdp2007
gdpdiff <- finalData %>%
  select(Country, Years, PPPGDP) %>%
  filter(Years == "X2008" | Years == "X2007" | Years == "X2019" | Years == "X2020") %>%
  pivot_wider(names_from = "Years", values_from = "PPPGDP") 

gdpdiff$X2007 <- as.numeric(gdpdiff$X2007)
gdpdiff$X2008 <- as.numeric(gdpdiff$X2008)
gdpdiff$X2019 <- as.numeric(gdpdiff$X2019)
gdpdiff$X2020 <- as.numeric(gdpdiff$X2020)

gdpdiff <- gdpdiff %>%
  filter(!is.na(X2007) & !is.na(X2008) & !is.na(X2019) & !is.na(X2020)) %>%
  mutate(diff2008 = (X2008 - X2007)/X2007, diff2020 = (X2020 - X2019)/X2019)

summary(gdpdiff$diff2008)
hist(gdpdiff$diff2008)
summary(gdpdiff$diff2020)
hist(gdpdiff$diff2020)

# find out the top three countries with highest gdp decrease
sorted2008gdp <- gdpdiff %>%
  arrange(diff2008)
print("According to the data, the top three countries with highest gdp decrease is Chad, Uruguay, and Palau in 2008")

sorted2020gdp <- gdpdiff %>%
  arrange(diff2020)
print("According to the data, the top three countries with highest gdp decrease is S„o TomÈ and PrÌncipe, Comoros, St. Lucia in 2020")


# we will calculate the percent of number of countries gdp decrease
# for 2008
dec2008rate <- length(which(gdpdiff$diff2008 < 0)) / length(gdpdiff$diff2008)
dec2008rate

dec2020rate <- length(which(gdpdiff$diff2020 < 0)) / length(gdpdiff$diff2020)
dec2020rate
