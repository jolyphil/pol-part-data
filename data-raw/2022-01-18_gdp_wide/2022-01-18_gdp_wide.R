library(dplyr)
library(readr)
library(tidyr)

gdp_raw <- read_csv("data-raw/2022-01-18_gdp_wide/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3469501.csv",
                skip = 4)

gdp_wide <- gdp_raw |> 
  rename(country_name = `Country Name`) |> 
  select(-c(`Country Code`, `Indicator Name`, `Indicator Code`, `...66`))

saveRDS(gdp_wide, file = "data/2022-01-18_gdp_wide.rds")
